defmodule Raycaster.Renderer do
  @behaviour :wx_object
  alias Extris.Shapes

  require Record
  Record.defrecordp :wx, Record.extract(:wx, from_lib: "wx/include/wx.hrl")
  Record.defrecordp :wxSize, Record.extract(:wxSize, from_lib: "wx/include/wx.hrl")
  Record.defrecordp :wxKey, Record.extract(:wxKey, from_lib: "wx/include/wx.hrl")

  defmodule State do
    defstruct [
      :parent,
      :canvas,
      :timer
    ]
  end

  def start_link(config) do
    IO.puts "starting"
    :wx_object.start_link(__MODULE__, config, [])
  end

  def init(config) do
    :wx.batch(fn() -> do_init(config) end)
  end

  def do_init(config) do
    parent = :proplists.get_value(:parent, config)
    canvas = :wxBufferedPaintDC.new(parent)
    # :wxWindow.hide(parent)
    # :wxWindow.reparent(canvas, parent)
    # :wxWindow.show(parent)

    state = %State{
      parent: parent,
      canvas: canvas
    }

    timer = :timer.send_interval(20, self, :update)

    {parent, %State{ state | timer: timer } }
  end

  def handle_info(:update, state) do
    IO.puts "update"
    new_state = :wx.batch(fn() -> render(state) end)
    {:noreply, new_state}
  end

  def handle_info(:stop, state) do
    :timer.cancel(state.timer)
    {:stop, :normal, state}
  end

  def handle_call(msg, _from, state) do
    {:reply, :ok, state}
  end

  def handle_cast(msg, state) do
    {:noreply, state}
  end

  def handle_event(msg, state) do
    {:noreply, state}
  end

  def code_change(_, _, state) do
    {:stop, :not_yet_implemented, state}
  end

  def terminate(_reason, state) do
    :timer.cancel(state.timer)
    :timer.sleep(300)
  end

  def render(state) do
    draw(state)
  end

  def draw(state) do
    :wxPaintDC.clear(state.canvas)

    # TODO: Draw a polygon somewhere

    state
  end
end
