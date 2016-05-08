defmodule Raycaster.IntersectionVisualizer do
  @behaviour :wx_object
  @timer_interval 20
  alias Raycaster.{Position, Vector, Line, Basics}
  import Basics

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
      :diagram_number,
      :line1,
      :line2,
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

    line1 = %Raycaster.Line{position: %Raycaster.Position{x: 200, y: 434}, vector: %Raycaster.Vector{angle: degrees(45), length: 1000}}
    line2 = %Raycaster.Line{position: %Raycaster.Position{x: 100, y: 500}, vector: %Raycaster.Vector{angle: degrees(0), length: 600}}

    # line1 = %Raycaster.Line{position: %Raycaster.Position{x: 200, y: 434}, vector: %Raycaster.Vector{angle: degrees(90), length: 1000}}
    # line2 = %Raycaster.Line{position: %Raycaster.Position{x: 100, y: 500}, vector: %Raycaster.Vector{angle: degrees(0), length: 600}}

    # line1 = %Raycaster.Line{position: %Raycaster.Position{x: 200, y: 434}, vector: %Raycaster.Vector{angle: degrees(0), length: 1000}}
    # line2 = %Raycaster.Line{position: %Raycaster.Position{x: 300, y: 300}, vector: %Raycaster.Vector{angle: degrees(90), length: 600}}

    # line1 = %Raycaster.Line{position: %Raycaster.Position{x: 952, y: 337}, vector: %Raycaster.Vector{angle: 3.141592653589793, length: 1000}}
    # line2 = %Raycaster.Line{position: %Raycaster.Position{x: 200, y: 200}, vector: %Raycaster.Vector{angle: 1.5707963267948966, length: 600}}

    # line1 = %Raycaster.Line{position: %Raycaster.Position{x: 800, y: 200}, vector: %Raycaster.Vector{angle: 1.5707963267948966, length: 600}}
    # line2 = %Raycaster.Line{position: %Raycaster.Position{x: 994, y: 385}, vector: %Raycaster.Vector{angle: 3.141592653589793, length: 1000}}

    # line1 = %Raycaster.Line{position: %Raycaster.Position{x: 635, y: 384}, vector: %Raycaster.Vector{angle: 4.71238898038469, length: 1000}}
    # line2 = %Raycaster.Line{position: %Raycaster.Position{x: 200, y: 200}, vector: %Raycaster.Vector{angle: 0.0, length: 600}}

    # line1 = %Raycaster.Line{position: %Raycaster.Position{x: 993, y: 458}, vector: %Raycaster.Vector{angle: 4.71238898038469, length: 1000}}
    # line2 = %Raycaster.Line{position: %Raycaster.Position{x: 200, y: 200}, vector: %Raycaster.Vector{angle: 0.0, length: 600}}

    state = %State{
      parent: panel,
      config: config,
      canvas: canvas,
      bitmap: bitmap,
      diagram_number: 0,
      line1: line1,
      line2: line2,
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
    first_diagram = fn(dc) ->
      :wxDC.clear(dc)
      :wxDC.setBrush(dc, :wx_const.wx_transparent_brush)
      :wxDC.setPen(dc, :wx_const.wx_black_pen)
      :wxDC.drawText(dc, 'Line1', {20, 20})
      a = state.line1.vector.angle
      a_degrees = a * (180 / :math.pi)
      :wxDC.drawText(dc, 'line1 angle: #{inspect a} radians. degrees: #{a_degrees}', {20, 40})
      draw_line(dc, state.line1)
    end
    second_diagram = fn(dc) ->
      :wxDC.clear(dc)
      :wxDC.setBrush(dc, :wx_const.wx_transparent_brush)
      :wxDC.setPen(dc, :wx_const.wx_black_pen)
      :wxDC.drawText(dc, 'Line1 and 2', {20, 20})
      draw_line(dc, state.line1)
      :wxDC.setPen(dc, :wxPen.new({255, 0, 0, 0}))
      draw_line(dc, state.line2)
    end
    third_diagram = fn(dc) ->
      :wxDC.clear(dc)
      :wxDC.setBrush(dc, :wx_const.wx_transparent_brush)
      :wxDC.setPen(dc, :wx_const.wx_black_pen)
      :wxDC.drawText(dc, 'Translate Line1 and Line2 such that Line1 is at origin', {20, 20})
      line1 = state.line1
      line2 = state.line2
      line1_translated = %Line{line1 | position: %Position{x: 0, y: 0}}
      line2_translated = %Line{line2 | position: %Position{x: line2.position.x - line1.position.x, y: line2.position.y - line1.position.y}}
      :wxDC.drawText(dc, 'Line 1: #{inspect line1}', {20, 40})
      :wxDC.drawText(dc, 'Line 2: #{inspect line2}', {20, 60})
      :wxDC.drawText(dc, 'Line 1 translated: #{inspect line1_translated}', {20, 80})
      :wxDC.drawText(dc, 'Line 2 translated: #{inspect line2_translated}', {20, 100})
      draw_line(dc, line1_translated)
      :wxDC.setPen(dc, :wxPen.new({255, 0, 0, 0}))
      draw_line(dc, line2_translated)
    end
    fourth_diagram = fn(dc) ->
      :wxDC.clear(dc)
      :wxDC.setBrush(dc, :wx_const.wx_transparent_brush)
      :wxDC.setPen(dc, :wx_const.wx_black_pen)
      :wxDC.drawText(dc, 'Rotate the lines such that line1 has angle 0', {20, 20})
      line1 = state.line1
      line2 = state.line2
      :wxDC.drawText(dc, 'Line 1: #{inspect line1}', {20, 40})
      :wxDC.drawText(dc, 'Line 2: #{inspect line2}', {20, 60})
      line1_translated = %Line{line1 | position: %Position{x: 0, y: 0}}
      line2_translated = %Line{line2 | position: %Position{x: line2.position.x - line1.position.x, y: line2.position.y - line1.position.y}}
      :wxDC.drawText(dc, 'Line 1 translated: #{inspect line1_translated}', {20, 80})
      :wxDC.drawText(dc, 'Line 2 translated: #{inspect line2_translated}', {20, 100})
      rotated_angle = line2.vector.angle - line1.vector.angle
      line1_angle_diff = line1.vector.angle - degrees(0)
      # rotate the second line's origin position through -line1.vector.angle
      # https://en.wikipedia.org/wiki/Rotation_matrix
      rotated_x2 = (line2_translated.position.x * :math.cos(-line1_angle_diff)) - (line2_translated.position.y * :math.sin(-line1_angle_diff))
      rotated_y2 = (line2_translated.position.x * :math.sin(-line1_angle_diff)) + (line2_translated.position.y * :math.cos(-line1_angle_diff))
      rotated_position = %Position{x: rotated_x2, y: rotated_y2}
      line1_rotated = %Line{line1_translated | vector: %Vector{line1.vector | angle: degrees(0) }}
      line2_rotated = %Line{line2_translated | position: rotated_position, vector: %Vector{line2.vector | angle: rotated_angle}}
      :wxDC.drawText(dc, 'Line 1: #{inspect line1_rotated}', {20, 120})
      :wxDC.drawText(dc, 'Line 2: #{inspect line2_rotated}', {20, 140})
      draw_line(dc, line1_rotated)
      :wxDC.setPen(dc, :wxPen.new({255, 0, 0, 0}))
      draw_line(dc, line2_rotated)
    end
    fifth_diagram = fn(dc) ->
      :wxDC.clear(dc)
      :wxDC.setBrush(dc, :wx_const.wx_transparent_brush)
      :wxDC.setPen(dc, :wx_const.wx_black_pen)
      :wxDC.drawText(dc, 'Identify intersection point (note that this matches even if intersection point is past line1 end)', {20, 20})
      line1 = state.line1
      line2 = state.line2
      line1_translated = %Line{line1 | position: %Position{x: 0, y: 0}}
      line2_translated = %Line{line2 | position: %Position{x: line2.position.x - line1.position.x, y: line2.position.y - line1.position.y}}
      rotated_angle = line2.vector.angle - line1.vector.angle
      line1_angle_diff = line1.vector.angle - degrees(0)
      # rotate the second line's origin position through -line1.vector.angle
      # https://en.wikipedia.org/wiki/Rotation_matrix
      rotated_x2 = (line2_translated.position.x * :math.cos(-line1_angle_diff)) - (line2_translated.position.y * :math.sin(-line1_angle_diff))
      rotated_y2 = (line2_translated.position.x * :math.sin(-line1_angle_diff)) + (line2_translated.position.y * :math.cos(-line1_angle_diff))
      rotated_position = %Position{x: rotated_x2, y: rotated_y2}
      line1_rotated = %Line{line1_translated | vector: %Vector{line1.vector | angle: degrees(0) }}
      line2_rotated = %Line{line2_translated | position: rotated_position, vector: %Vector{line2.vector | angle: rotated_angle}}
      draw_line(dc, line1_rotated)
      :wxDC.setPen(dc, :wxPen.new({255, 0, 0, 0}))
      draw_line(dc, line2_rotated)
      # :wxDC.setPen(dc, :wx_const.wx_black_pen)
      # draw_line(dc, line1)
      :wxDC.setPen(dc, :wxPen.new({255, 0, 0, 0}))
      draw_line(dc, line2)
      # identify intersection point
      if line2_rotated.vector.angle != degrees(0) do
        y = -1 * line2_rotated.position.y # multiply by -1 so we can do normal cartesian math here
        :wxDC.drawText(dc, 'line2 rotated y: #{inspect line2_rotated.position.y}', {20, 40})
        # angle is in radians
        a = line2_rotated.vector.angle
        a_degrees = a * (180 / :math.pi)
        :wxDC.drawText(dc, 'line2 rotated angle: #{inspect a} radians. degrees: #{a_degrees}', {20, 60})
        c = y/(:math.sin(a))
        :wxDC.drawText(dc, 'c: #{inspect c}', {20, 80})
        # IO.puts "c: #{inspect c}"
        # IO.puts "c > 0"
        if(c < 0) do
          :nothing
        else
          # So c is the length of line2 where it intersects with line1.  The length of line1 at that point is line2_rotated.position.x + the length of the base of the triangle defined by line2_rotated
          # The length of that base is d where:
          # cos(a) = d/c
          # c * cos(a) = d
          d = c * :math.cos(a)
          :wxDC.drawText(dc, 'd: #{inspect d}', {20, 100})
          # IO.puts "c: #{c}"
          # IO.puts "math cos a: #{:math.cos(a)}"
          # IO.puts "d: #{d}"
          new_length = line2_rotated.position.x + d
          :wxDC.drawText(dc, 'new_length: #{inspect new_length}', {20, 120})
          # The line only intersects if the length of line2 is at least the length of the new line
          # if the rotated line2 doesn't cross the origin, then there is no intersection.
          intersecting_line = Line.with_length(line1, new_length)
          intersection_point = Line.point2(intersecting_line)
          :wxDC.drawText(dc, 'intersection_point: #{inspect intersection_point}', {20, 160})
          :wxDC.drawCircle(dc, {round(intersection_point.x), round(intersection_point.y)}, 5)
          :wxDC.setPen(dc, :wx_const.wx_black_pen)
          draw_line(dc, intersecting_line)
        end
      end
    end
    sixth_diagram = fn(dc) ->
      :wxDC.clear(dc)
      :wxDC.setBrush(dc, :wx_const.wx_transparent_brush)
      :wxDC.setPen(dc, :wx_const.wx_black_pen)
      :wxDC.drawText(dc, 'Identify intersection point (dont intersect if line1 is too short to intersect)', {20, 20})
      line1 = state.line1
      line2 = state.line2
      line1_translated = %Line{line1 | position: %Position{x: 0, y: 0}}
      line2_translated = %Line{line2 | position: %Position{x: line2.position.x - line1.position.x, y: line2.position.y - line1.position.y}}
      rotated_angle = line2.vector.angle - line1.vector.angle
      line1_angle_diff = line1.vector.angle - degrees(0)
      # rotate the second line's origin position through -line1.vector.angle
      # https://en.wikipedia.org/wiki/Rotation_matrix
      rotated_x2 = (line2_translated.position.x * :math.cos(-line1_angle_diff)) - (line2_translated.position.y * :math.sin(-line1_angle_diff))
      rotated_y2 = (line2_translated.position.x * :math.sin(-line1_angle_diff)) + (line2_translated.position.y * :math.cos(-line1_angle_diff))
      rotated_position = %Position{x: rotated_x2, y: rotated_y2}
      line1_rotated = %Line{line1_translated | vector: %Vector{line1.vector | angle: degrees(0) }}
      line2_rotated = %Line{line2_translated | position: rotated_position, vector: %Vector{line2.vector | angle: rotated_angle}}
      draw_line(dc, line1_rotated)
      :wxDC.setPen(dc, :wxPen.new({255, 0, 0, 0}))
      draw_line(dc, line2_rotated)
      # :wxDC.setPen(dc, :wx_const.wx_black_pen)
      # draw_line(dc, line1)
      :wxDC.setPen(dc, :wxPen.new({255, 0, 0, 0}))
      draw_line(dc, line2)
      # identify intersection point
      if line2_rotated.vector.angle != degrees(0) do
        y = -1 * line2_rotated.position.y # multiply by -1 so we can do normal cartesian math here
        :wxDC.drawText(dc, 'line2 rotated y: #{inspect line2_rotated.position.y}', {20, 40})
        # angle is in radians
        a = line2_rotated.vector.angle
        a_degrees = a * (180 / :math.pi)
        :wxDC.drawText(dc, 'line2 rotated angle: #{inspect a} radians. degrees: #{a_degrees}', {20, 60})
        c = y/(:math.sin(a))
        :wxDC.drawText(dc, 'c: #{inspect c}', {20, 80})
        # IO.puts "c: #{inspect c}"
        # IO.puts "c > 0"
        if(c < 0) do
          :nothing
        else
          # So c is the length of line2 where it intersects with line1.  The length of line1 at that point is line2_rotated.position.x + the length of the base of the triangle defined by line2_rotated
          # The length of that base is d where:
          # cos(a) = d/c
          # c * cos(a) = d
          d = c * :math.cos(a)
          :wxDC.drawText(dc, 'd: #{inspect d}', {20, 100})
          # IO.puts "c: #{c}"
          # IO.puts "math cos a: #{:math.cos(a)}"
          # IO.puts "d: #{d}"
          new_length = line2_rotated.position.x + d
          :wxDC.drawText(dc, 'new_length: #{inspect new_length}', {20, 120})
          # The line only intersects if the length of line2 is at least the length of the new line
          # if the rotated line2 doesn't cross the origin, then there is no intersection.
          :wxDC.drawText(dc, 'line2 rotated: #{inspect line2_rotated}', {20, 380})
          intersecting_line = Line.with_length(line1, new_length)
          intersection_point = Line.point2(intersecting_line)
          if(intersection_point.x > Line.point2(line2).x) do
            :nothing
          else
            :wxDC.drawText(dc, 'intersection_point: #{inspect intersection_point}', {20, 160})
            :wxDC.drawCircle(dc, {round(intersection_point.x), round(intersection_point.y)}, 5)
            :wxDC.setPen(dc, :wx_const.wx_black_pen)
            draw_line(dc, intersecting_line)
          end
        end
      end
    end
    diagrams = [
      first_diagram,
      second_diagram,
      third_diagram,
      fourth_diagram,
      fifth_diagram,
      sixth_diagram,
    ]
    diagram_num = rem(state.diagram_number, Enum.count(diagrams))
    current_diagram = Enum.at(diagrams, diagram_num)
    draw(state.canvas, state.bitmap, current_diagram)
  end

  def handle_event(wx(event: wxSize(size: {w, h})), state = %State{bitmap: prev, canvas: canvas}) do
    bitmap = :wxBitmap.new(w,h)
    draw(canvas, bitmap, fn(dc) -> :wxDC.clear(dc) end)
    :wxBitmap.destroy(prev)
    {:noreply, %State{state | bitmap: bitmap}}
  end

  def handle_event(wx(event: wxMouse(type: :left_up, x: x, y: y)), state = %State{canvas: canvas}) do
    {:noreply, %State{state | diagram_number: state.diagram_number + 1}}
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

  def draw_line(dc, line) do
    point1 = Line.point1(line)
    point2 = Line.point2(line)
    :wxDC.drawCircle(dc, {round(point1.x), round(point1.y)}, 5)
    :wxDC.drawLine(dc, {round(point1.x), round(point1.y)}, {round(point2.x), round(point2.y)})
  end
end
