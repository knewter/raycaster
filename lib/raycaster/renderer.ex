defmodule Raycaster.Basics do
  @doc "Converts polar coordinates to cartesian coordinates"
  def from_polar(r, theta) do
    x = r * :math.cos(theta)
    y = r * :math.sin(theta)
    {x, y}
  end

  @doc "Converts degrees to radians"
  def degrees(deg) do
    deg * (:math.pi / 180)
  end
end

defmodule Raycaster.Position do
  defstruct [:x, :y]
end

defmodule Raycaster.Vector do
  alias Raycaster.Position
  defstruct [:angle, :length]

  def vector_between(p1=%Position{}, p2=%Position{}) do
    dx = p2.x - p1.x
    dy = p2.y - p1.y
    %__MODULE__{
      length: :math.sqrt(dx * dx + dy * dy),
      angle: :math.atan2((p2.y - p1.y),(p2.x - p1.x))
    }
  end
end

defmodule Raycaster.Line do
  defstruct [:position, :vector]
  alias Raycaster.{Position, Vector, Basics, Line}

  def point1(%__MODULE__{position: position=%Position{}}) do
    position
  end

  def point2(%__MODULE__{position: position=%Position{}, vector: vector=%Vector{}}) do
    {dx, dy} = Basics.from_polar(vector.length, vector.angle)
    %Position{x: position.x + dx, y: position.y + dy}
  end

  def norms(%__MODULE__{vector: %Vector{ angle: angle }}) do
    { :math.cos(angle), :math.sin(angle) }
  end

  def with_length(line, length) do
    %__MODULE__{line | vector: %Vector{ line.vector | length: length } }
  end

  def line_between(from=%Position{}, to=%Position{}) do
    %Line{position: from, vector: Vector.vector_between(from, to)}
  end
end

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
      :wxDC.drawCircle(dc, {state.pos.x, state.pos.y}, 5)
      :wxDC.setBrush(dc, :wx_const.wx_transparent_brush)
      :wxDC.setPen(dc, :wx_const.wx_black_pen)
      for wall <- state.walls do
        point1 = Line.point1(wall)
        point2 = Line.point2(wall)
        :wxDC.drawLine(dc, {round(point1.x), round(point1.y)}, {round(point2.x), round(point2.y)})
      end
      :wxDC.setPen(dc, :wx_const.wx_transparent_pen)
      :wxDC.setBrush(dc, :wxBrush.new({255, 255, 0})) # yellow
      rays = solve_rays(state.walls, state.pos)
      outer_polygon_points =
        rays
        |> Enum.map(fn ray ->
             point = Line.point2(ray)
             {round(point.x), round(point.y)}
           end)
      IO.inspect outer_polygon_points
      :wxDC.drawPolygon(dc, outer_polygon_points)
    end
    draw(state.canvas, state.bitmap, fun)
  end

  @spec intersect(Line.t, Line.t) :: :nothing | Line.t
  def intersect(line1=%Line{}, line2=%Line{}) do
    import Basics
    start1 = Line.point1(line1)
    { r_px, r_py } = { start1.x, start1.y }
    start2 = Line.point1(line2)
    { s_px, s_py } = { start2.x, start2.y }
    { r_dx, r_dy } = Line.norms line1
    { s_dx, s_dy } = Line.norms line2
    sm = ((r_px * r_dy) - (r_py * r_dx) + (s_py * r_dx) - (s_px * r_dy)) / ((s_dx * r_dy) - (s_dy * r_dx))
    rm = (s_px - r_px + (s_dx * sm)) / r_dx

    cond do
      sm < 0 -> :nothing
      line2.vector.length < sm -> :nothing
      rm < 0 -> :nothing
      :else -> Line.with_length(line1, rm)
    end
  end

  @doc """
  Solve for rays including wall intersection, and return them sorted by angle.
  """
  @spec solve_rays(list(Wall.t), Position.t) :: list(Line.t)
  def solve_rays(walls, ray_start) do
    walls
      |> Enum.flat_map(fn wall -> to_rays(ray_start, wall) end)
      |> Enum.map(fn line -> curtail(walls, line) end)
      |> Enum.filter(fn line -> line != :nothing end)
      |> Enum.sort(fn (line1, line2) ->
           line1.vector.angle > line2.vector.angle
         end)
  end

  def curtail(walls, line) do
    result =
      walls
        |> Enum.map(fn(wall) -> intersect(line, wall) end)
        |> Enum.filter(fn(line) -> line != :nothing end)
        |> Enum.sort(fn(first, second) ->
          first.vector.length < second.vector.length
        end)

    case result do
      [] -> :nothing
      l  -> hd(l)
    end
  end

  def to_rays(position, line) do
    import Basics

    ray_to_start = Line.line_between(position, Line.point1(line))
    ray_to_end = Line.line_between(position, Line.point2(line))

    [
      adjust_angle(degrees(0.5), ray_to_start),
      adjust_angle(degrees(-0.5), ray_to_start),
      adjust_angle(degrees(0.5), ray_to_end),
      adjust_angle(degrees(-0.5), ray_to_end)
    ]
  end

  def adjust_angle(delta, line=%Line{vector: vector}) do
    %Line{ line | vector: %Vector{ vector | angle: vector.angle + delta } }
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
