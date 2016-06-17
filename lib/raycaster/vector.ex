defmodule Raycaster.Vector do
  defstruct [:angle, :length]

  def normalize(vector=%__MODULE__{length: length}) when length > 0 do
    vector
  end
  def normalize(vector=%__MODULE__{length: length, angle: angle}) when length <= 0 do
    import Raycaster.Basics
    inverted_length = -length
    inverted_angle = degrees(180) + angle
    %__MODULE__{length: inverted_length, angle: inverted_angle}
  end
end
