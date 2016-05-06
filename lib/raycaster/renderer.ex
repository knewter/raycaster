defmodule Raycaster.Renderer do
  @behaviour :wx_object
  @timer_interval 20

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
      :shapes,
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

    shapes = produce_shapes()

    state = %State{
      parent: panel,
      config: config,
      canvas: canvas,
      bitmap: bitmap,
      pos: {0, 0},
      shapes: shapes,
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
      :wxDC.drawCircle(dc, state.pos, 5)
      :wxDC.setBrush(dc, :wx_const.wx_transparent_brush)
      :wxDC.setPen(dc, :wx_const.wx_black_pen)
      for shape <- state.shapes do
        case shape do
          {:polygon, points} ->
            :wxDC.drawPolygon(dc, points)
          _ -> :ignore
        end
      end
    end
    draw(state.canvas, state.bitmap, fun)
  end

  def handle_event(wx(event: wxSize(size: {w, h})), state = %State{bitmap: prev, canvas: canvas}) do
    bitmap = :wxBitmap.new(w,h)
    draw(canvas, bitmap, fn(dc) -> :wxDC.clear(dc) end)
    :wxBitmap.destroy(prev)
    {:noreply, %State{state | bitmap: bitmap}}
  end

  def handle_event(wx(event: wxMouse(type: :left_down, x: x, y: y)), state) do
    {:noreply, %State{state|pos: {x, y}}}
  end

  def handle_event(wx(event: wxMouse(type: :motion, x: x, y: y)), state = %State{canvas: canvas}) do
    {:noreply, %State{state | pos: {x, y}}}
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

  def produce_shapes do
    [
      {:polygon, [{100, 0}, {100, 100}, {200, 100}]},
      {:polygon, [{150, 250}, {150, 270}, {175, 300}, {200, 300}]},
      {:polygon, [{250, 450}, {350, 520}, {375, 300}]},
      {:polygon, [{250, 150}, {250, 270}, {275, 300}, {300, 200}]},
    ]
  end
end
