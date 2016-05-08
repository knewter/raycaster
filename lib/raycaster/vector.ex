defmodule Raycaster.Vector do
  defstruct [:angle, :length]

  def norms(%__MODULE__{ angle: angle }) do
    { :math.cos(angle), :math.sin(angle) }
  end
end
