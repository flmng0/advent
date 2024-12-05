defmodule Advent do
  @moduledoc """
  Utilities that are used throughout the challenge.
  """

  defmodule CharGrid do
    defstruct width: 0, height: 0, chars: %{}

    @type t() :: %__MODULE__{
            width: integer(),
            height: integer(),
            chars: map()
          }

    @spec parse(String.t()) :: t()
    def parse(input) do
      chars = input |> String.trim_trailing() |> String.to_charlist()

      width = Enum.find_index(chars, &(&1 == ?\n))
      # +1 acounting for trim_trailing
      height = Enum.count(chars, &(&1 == ?\n)) + 1

      filtered = chars |> Enum.filter(&(&1 != ?\n)) |> Enum.with_index()

      chars =
        for {c, i} <- filtered, into: %{} do
          x = Integer.mod(i, width)
          y = Integer.floor_div(i, width)

          {{x, y}, c}
        end

      %__MODULE__{width: width, height: height, chars: chars}
    end

    def size(%__MODULE__{width: width, height: height}), do: width * height
  end

  def lines(input) do
    input |> String.trim_trailing() |> String.splitter("\n") |> Stream.map(&String.trim/1)
  end

  def map_input(input, mapper) do
    input
    |> lines()
    |> Enum.map(mapper)
  end

  def sign(0), do: 0
  def sign(x), do: x / abs(x)

  def except_index(list, index) do
    {l, [_ | r]} = Enum.split(list, index)

    l ++ r
  end
end
