defmodule Raycaster.Renderer do
  @behaviour :wx_object
  @timer_interval 20
  alias Raycaster.{Position, Vector, Line, Basics}

  require Record
  Record.defrecordp :wx, Record.extract(:wx, from_lib: "wx/include/wx.hrl")
  Record.defrecordp :wxSize, Record.extract(:wxSize, from_lib: "wx/include/wx.hrl")
  Record.defrecordp :wxKey, Record.extract(:wxKey, from_lib: "wx/include/wx.hrl")
  Record.defrecordp :wxPaint, Record.extract(:wxPaint, from_lib: "wx/include/wx.hrl")
  Record.defrecordp :wxCommand, Record.extract(:wxCommand, from_lib: "wx/include/wx.hrl")
  Record.defrecordp :wxMouse, Record.extract(:wxMouse, from_lib: "wx/include/wx.hrl")

  defmodule State do
    defstruct [
      :parent,
      :config,
      :canvas,
      :bitmap,
      :pos,
      :walls,
      :timer
    ]
  end

  def start_link(config) do
    :wx_object.start_link(__MODULE__, config, [])
  end

  def init(config) do
    :wx.batch(fn() -> do_init(config) end)
  end

  def do_init(config) do
    parent = config[:parent]
    panel = :wxPanel.new(parent, [])

    ## Setup sizers
    main_sizer = :wxBoxSizer.new(:wx_const.wx_vertical)
    sizer = :wxStaticBoxSizer.new(:wx_const.wx_vertical, panel)

    canvas = :wxPanel.new(panel, style: :wx_const.wx_full_repaint_on_resize)

    :wxPanel.connect(canvas, :paint, [:callback])
    :wxPanel.connect(canvas, :size)
    :wxPanel.connect(canvas, :left_down)
    :wxPanel.connect(canvas, :left_up)
    :wxPanel.connect(canvas, :motion)

    ## Add to sizers
    :wxSizer.add(sizer, canvas, flag: :wx_const.wx_expand, proportion: 1)

    :wxSizer.add(main_sizer, sizer, flag: :wx_const.wx_expand, proportion: 1)

    :wxPanel.setSizer(panel, main_sizer)
    :wxSizer.layout(main_sizer)

    {w,h} = :wxPanel.getSize(canvas)
    bitmap = :wxBitmap.new(:erlang.max(w,30),:erlang.max(30,h))

    timer = :timer.send_interval(@timer_interval, self, :tick)

    walls = produce_walls()

    state = %State{
      parent: panel,
      config: config,
      canvas: canvas,
      bitmap: bitmap,
      pos: %Position{x: 0, y: 0},
      walls: walls,
      timer: timer
    }

    {panel, state}
  end

  ########
  ## Sync event from callback events, paint event must be handled in callbacks
  ## otherwise nothing will be drawn on windows.
  def handle_sync_event(wx(event: wxPaint()), _wxObj, %State{canvas: canvas, bitmap: bitmap}) do
    dc = :wxPaintDC.new(canvas)
    redraw(dc, bitmap)
    :wxPaintDC.destroy(dc)
    :ok
  end

  def draw_stuff(state) do
    fun = fn(dc) ->
      :wxDC.clear(dc)
      :wxDC.setBrush(dc, :wx_const.wx_red_brush)
      :wxDC.setPen(dc, :wx_const.wx_transparent_pen)
      :wxDC.drawCircle(dc, {round(state.pos.x), round(state.pos.y)}, 5)
      :wxDC.setBrush(dc, :wx_const.wx_transparent_brush)
      :wxDC.setPen(dc, :wx_const.wx_black_pen)
      for wall <- state.walls do
        point1 = Line.point1(wall)
        point2 = Line.point2(wall)
        :wxDC.drawLine(dc, {round(point1.x), round(point1.y)}, {round(point2.x), round(point2.y)})
      end

      rays = to_rays(state.pos)
      :wxDC.setPen(dc, :wxPen.new({255, 0, 0, 0}))

      ray_intersections =
        for ray <- rays do
          intersections =
            state.walls
            |> Enum.map(fn wall ->
              Line.intersect(ray, wall)
            end)
            |> Enum.filter(fn i -> i != :nothing end)
          {ray, intersections}
        end

      for {ray, intersections} <- ray_intersections do
        sorted_intersections =
          intersections
          |> Enum.sort(fn(i1, i2) ->
            i1.vector.length < i2.vector.length
          end)

        line_to_draw =
          case sorted_intersections do
            [] -> ray
            [shortest_intersection|_] -> shortest_intersection
          end

        point1 = Line.point1(line_to_draw)
        point2 = Line.point2(line_to_draw)
        :wxDC.drawLine(dc, {round(point1.x), round(point1.y)}, {round(point2.x), round(point2.y)})
      end
    end
    draw(state.canvas, state.bitmap, fun)
  end

  def to_rays(ray_start=%Position{}) do
    import Basics

    for angle <- [0, 90, 180, 270] do
      %Line{position: ray_start, vector: %Vector{length: 1000, angle: degrees(angle)}}
    end
  end

  def handle_event(wx(event: wxSize(size: {w, h})), state = %State{bitmap: prev, canvas: canvas}) do
    bitmap = :wxBitmap.new(w,h)
    draw(canvas, bitmap, fn(dc) -> :wxDC.clear(dc) end)
    :wxBitmap.destroy(prev)
    {:noreply, %State{state | bitmap: bitmap}}
  end

  def handle_event(wx(event: wxMouse(type: :motion, x: x, y: y)), state = %State{canvas: canvas}) do
    {:noreply, %State{state | pos: %Position{x: x, y: y}}}
  end

  def handle_event(ev = wx(), state = %State{}) do
    {:noreply, state}
  end

  ## Callbacks handled as normal gen_server callbacks
  def handle_info(:tick, state) do
    draw_stuff(state)
    {:noreply, state}
  end
  def handle_info(msg, state) do
    {:noreply, state}
  end

  def handle_call(:shutdown, _from, state = %State{parent: panel, timer: timer}) do
    :wxPanel.destroy(panel)
    :timer.cancel(timer)
    {:stop, :normal, :ok, state}
  end
  def handle_call(msg, _from, state) do
    {:reply,{:error, :nyi}, state}
  end

  def handle_cast(msg, state) do
    IO.puts "Got cast #{inspect msg}"
    {:noreply, state}
  end

  def code_change(_, _, state) do
    {:stop, :ignore, state}
  end

  def terminate(_reason, %State{timer: timer}) do
    :timer.cancel(timer)
    :ok
  end

  #####
  ## Local functions
  #####

  ## Buffered makes it all appear on the screen at the same time
  def draw(canvas, bitmap, fun) do
    memory_dc = :wxMemoryDC.new(bitmap)
    fun.(memory_dc)

    cdc = :wxWindowDC.new(canvas)
    :wxDC.blit(
      cdc,
      {0,0},
      {:wxBitmap.getWidth(bitmap), :wxBitmap.getHeight(bitmap)},
      memory_dc,
      {0,0}
    )
    :wxWindowDC.destroy(cdc)
    :wxMemoryDC.destroy(memory_dc)
  end

  def redraw(dc, bitmap) do
    memory_dc = :wxMemoryDC.new(bitmap)
    :wxDC.blit(
      dc,
      {0,0},
      {:wxBitmap.getWidth(bitmap), :wxBitmap.getHeight(bitmap)},
      memory_dc,
      {0,0}
    )
    :wxMemoryDC.destroy(memory_dc)
  end

  def get_pos(w,h) do
    {:random.uniform(w), :random.uniform(h)}
  end

  def produce_walls do
    import Basics
    [
      %Line{position: %Position{ x: 200, y: 200 }, vector: %Vector{ length: 600, angle: degrees(0)   } },
      %Line{position: %Position{ x: 800, y: 200 }, vector: %Vector{ length: 600, angle: degrees(90)  } },
      %Line{position: %Position{ x: 200, y: 200 }, vector: %Vector{ length: 600, angle: degrees(90)  } },
      %Line{position: %Position{ x: 800, y: 800 }, vector: %Vector{ length: 600, angle: degrees(180) } },
      %Line{position: %Position{ x: 300, y: 680 }, vector: %Vector{ length: 150, angle: degrees(250) } },
      %Line{position: %Position{ x: 650, y: 400 }, vector: %Vector{ length: 120, angle: degrees(235) } },
      %Line{position: %Position{ x: 370, y: 250 }, vector: %Vector{ length: 300, angle: degrees(70)  } },
      %Line{position: %Position{ x: 500, y: 350 }, vector: %Vector{ length: 300, angle: degrees(30)  } },
      %Line{position: %Position{ x: 600, y: 600 }, vector: %Vector{ length:  50, angle: degrees(315) } },
      %Line{position: %Position{ x: 420, y: 600 }, vector: %Vector{ length:  50, angle: degrees(290) } },
    ]
  end
end
