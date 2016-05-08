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

  def point2(%__MODULE__{position: position=%Position{}, vector: vector=%Vector{}}, :reflected) do
    import Basics
    angle = reflected_angle(vector.angle)
    {dx, dy} = Basics.from_polar(vector.length, angle)
    %Position{x: position.x + dx, y: position.y + dy}
  end

  def with_length(line, length) do
    %__MODULE__{line | vector: %Vector{ line.vector | length: length } }
  end

  @spec intersect(%Line{}, %Line{}) :: :nothing | %Line{}
  def intersect(line1=%__MODULE__{}, line2=%__MODULE__{}) do
    import Basics
    # translate the lines such that line1 is on the origin
    line1_translated = %Line{line1 | position: %Position{x: 0, y: 0}}
    line2_translated = %Line{line2 | position: %Position{x: line2.position.x - line1.position.x, y: line2.position.y - line1.position.y}}
    # Rotate the lines such that line1 has angle 0
    # (we don't really need line1 rotated here since it will just be the x axis)
    rotated_angle = line2.vector.angle - line1.vector.angle
    line1_angle_diff = line1.vector.angle - degrees(0)
    # rotate the second line's origin position through -line1_angle_diff
    # https://en.wikipedia.org/wiki/Rotation_matrix
    rotated_x2 = (line2_translated.position.x * :math.cos(-line1_angle_diff)) - (line2_translated.position.y * :math.sin(-line1_angle_diff))
    rotated_y2 = (line2_translated.position.x * :math.sin(-line1_angle_diff)) + (line2_translated.position.y * :math.cos(-line1_angle_diff))
    rotated_position = %Position{x: rotated_x2, y: rotated_y2}
    line1_rotated = %Line{line1_translated | vector: %Vector{line1.vector | angle: degrees(0) }}
    line2_rotated = %Line{line2_translated | position: rotated_position, vector: %Vector{line2.vector | angle: rotated_angle}}
    # if line2_rotated has angle 0, then they were parallel and consequently will never intersect
    if line2_rotated.vector.angle == degrees(0) do
      :nothing
    else
      # Otherwise, they intersect where line2_rotated crosses the x axis.
      # We can calculate this with trigonometry.
      # We know that line2.position.y * -1 is the height of the triangle that hits the x axis, and the angle is line2.vector.angle (call it a), and call the hypotenuse of that triangle c
      # Consequently:
      # sin(a) = e/c
      # sin(a) = -y/c
      # c * sin(a) = -y
      # c = -y/sin(a)
      y = -1 * line2_rotated.position.y # multiply by -1 so we can do normal cartesian math here
      # angle is in radians
      a = line2_rotated.vector.angle
      c = y/(:math.sin(a))
      if(c < 0) do # would intersect, but behind the starting point so no go
        :nothing
      else
        # So c is the length of line2 where it intersects with line1.  The length of line1 at that point is line2_rotated.position.x + the length of the base of the triangle defined by line2_rotated
        # The length of that base is d where:
        # cos(a) = d/c
        # c * cos(a) = d
        d = c * :math.cos(a)
        new_length = line2_rotated.position.x + d
        intersecting_line = Line.with_length(line1, new_length)
        intersection_point = Line.point2(intersecting_line)
        # The lines only intersect if the x and y of the supposed intersection lies on both line segments
        between_line2_x =
          ((Line.point1(line2).x <= intersection_point.x) &&
           (intersection_point.x <= Line.point2(line2).x)) ||
          ((Line.point1(line2).x >= intersection_point.x) &&
           (intersection_point.x >= Line.point2(line2).x))
        between_line1_x =
          ((Line.point1(line1).x <= intersection_point.x) &&
           (intersection_point.x <= Line.point2(line1).x)) ||
          ((Line.point1(line1).x >= intersection_point.x) &&
           (intersection_point.x >= Line.point2(line1).x))
        between_line2_y =
          ((Line.point1(line2).y <= intersection_point.y) &&
           (intersection_point.y <= Line.point2(line2).y)) ||
          ((Line.point1(line2).y >= intersection_point.y) &&
           (intersection_point.y >= Line.point2(line2).y))
        between_line1_y =
          ((Line.point1(line1).y <= intersection_point.y) &&
           (intersection_point.y <= Line.point2(line1).y)) ||
          ((Line.point1(line1).y >= intersection_point.y) &&
           (intersection_point.y >= Line.point2(line1).y))
        if(between_line1_x && between_line2_x && between_line1_y && between_line2_y) do
          intersecting_line
        else
          :nothing
        end
      end
    end
  end
end
