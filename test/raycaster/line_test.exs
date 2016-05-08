defmodule Raycaster.LineTest do
  use ExUnit.Case
  alias Raycaster.{Line, Position, Vector}
  import Raycaster.Basics

  test "line intersection (basic number 1)" do
    line1 = %Line{position: %Position{x: 0, y: 0}, vector: %Vector{length: 1, angle: degrees(0)}}
    line2 = %Line{position: %Position{x: 1, y: -1}, vector: %Vector{length: 1, angle: degrees(90)}}
    # line2 should intersect line1 at point 1, 1, so should be line1 with length 1 (or just line1)
    intersection = Line.intersect(line1, line2)
    assert intersection == line1
  end

  test "line intersection (basic number 2)" do
    line1 = %Line{position: %Position{x: 0, y: 0}, vector: %Vector{length: 1, angle: degrees(0)}}
    line2 = %Line{position: %Position{x: 0, y: -1}, vector: %Vector{length: 2, angle: degrees(45)}}
    # line2 should intersect line1 at point 1, 1, so should be line1 with length 1 (or just line1)
    intersection = Line.intersect(line1, line2)
    assert_in_delta(intersection.vector.length, line1.vector.length, 0.001)
  end

  test "line intersection (basic number 3)" do
    line1 = %Line{position: %Position{x: 0, y: 0}, vector: %Vector{length: 1, angle: degrees(0)}}
    line2 = %Line{position: %Position{x: 0, y: -1}, vector: %Vector{length: 2, angle: degrees(270)}}
    # line2 should not intersect line1
    intersection = Line.intersect(line1, line2)
    assert :nothing = intersection
  end

  test "line intersection (basic number 4)" do
    line1 = %Line{position: %Position{x: 0, y: 0}, vector: %Vector{length: 1, angle: degrees(0)}}
    line2 = %Line{position: %Position{x: 0, y: -1}, vector: %Vector{length: 2, angle: degrees(270)}}
    # line2 should not intersect line1
    intersection = Line.intersect(line1, line2)
    assert :nothing = intersection
  end

  test "line intersection (basic number 5)" do
    line1 = %Line{position: %Position{x: 0, y: 0},  vector: %Vector{length: 5,   angle: degrees(315)}}
    line2 = %Line{position: %Position{x: 0, y: -2}, vector: %Vector{length: 600, angle: degrees(0)}}
    intersection = Line.intersect(line1, line2)
    assert intersection.position.x == 0
    assert intersection.position.y == 0
    assert intersection.vector.angle == degrees(315)
    assert_in_delta(intersection.vector.length, 2.82, 0.1)
  end

  test "line intersection (real world example bugfix 1)" do
    line1 = %Raycaster.Line{position: %Raycaster.Position{x: 558, y: 434}, vector: %Raycaster.Vector{angle: degrees(90), length: 1000}}
    line2 = %Raycaster.Line{position: %Raycaster.Position{x: 200, y: 500}, vector: %Raycaster.Vector{angle: degrees(0), length: 600}}
    intersection = Line.intersect(line1, line2)
    assert :nothing = intersection
  end
end
