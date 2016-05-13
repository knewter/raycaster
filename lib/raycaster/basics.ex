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
