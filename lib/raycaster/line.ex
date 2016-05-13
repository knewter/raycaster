defmodule Raycaster.Line do
  defstruct [:position, :vector]
  alias Raycaster.{Position, Vector, Basics}

  def point1(%__MODULE__{position: position=%Position{}}) do
    position
  end

  def point2(%__MODULE__{position: position=%Position{}, vector: vector=%Vector{}}) do
    {dx, dy} = Basics.from_polar(vector.length, vector.angle)
    %Position{x: position.x + dx, y: position.y + dy}
  end

  def rotate(line1=%__MODULE__{position: %Position{x: x, y: y}, vector: %Vector{length: length, angle: angle}}, rotation_angle) do
    import :math
    rotated_angle = angle + rotation_angle
    rotated_x = (x * cos(rotation_angle)) - (y * sin(rotation_angle))
    rotated_y = (x * sin(rotation_angle)) + (y * cos(rotation_angle))
    rotated_position = %Position{x: rotated_x, y: rotated_y}
    %__MODULE__{position: rotated_position, vector: %Vector{ length: length, angle: rotated_angle}}
  end

  def with_length(line, length) do
    %__MODULE__{line | vector: %Vector{ line.vector | length: length } }
  end

  def within_bounding_box?(line, %Position{ x: x, y: y }) do
    point1 = point1(line)
    point2 = point2(line)
    between_x =
      (point1.x - 0.001 <= x) && (x <= point2.x + 0.001) ||
      (point1.x - 0.001 >= x) && (x >= point2.x + 0.001)
    between_y =
      (point1.y - 0.001 <= y) && (y <= point2.y + 0.001) ||
      (point1.y - 0.001 >= y) && (y >= point2.y + 0.001)
    between_x && between_y
  end

  def intersect(line1=%__MODULE__{}, line2=%__MODULE__{}) do
    import Basics
    import :math
    translated_line1 = %__MODULE__{line1 | position: %Position{ x: line1.position.x - line1.position.x, y: line1.position.y - line1.position.y } }
    translated_line2 = %__MODULE__{line2 | position: %Position{ x: line2.position.x - line1.position.x, y: line2.position.y - line1.position.y } }

    rotated_line1 = rotate(translated_line1, -line1.vector.angle)
    rotated_line2 = rotate(translated_line2, -line1.vector.angle)

    if(rotated_line2.vector.angle == degrees(0)) do
      :nothing
    else
      y = -rotated_line2.position.y
      x = rotated_line2.position.x
      a = rotated_line2.vector.angle
      a_complement = pi/2 - a
      b = (y / cos(a_complement)) * sin(a_complement)
      new_length = x + b

      intersecting_line = with_length(line1, new_length)
      intersection_point = point2(intersecting_line)

      if within_bounding_box?(line1, intersection_point) && within_bounding_box?(line2, intersection_point) do
        intersecting_line
      else
        :nothing
      end
    end
  end
end
