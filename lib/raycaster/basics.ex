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

  def reflected_angle(angle) do
    # We add this function to handle producing 'point2' translated to the graphics coordinate system, where y is positive when it goes down
    if (angle >= degrees(0)) do
      degrees(360) - angle
    else
      -(degrees(360)) - angle
    end
  end
end
