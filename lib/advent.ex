defmodule Advent do
  @moduledoc """
  Utilities that are used throughout the challenge.
  """

  def lines(input) do
    input |> String.trim_trailing() |> String.splitter("\n") |> Stream.map(&String.trim/1)
  end

  def map_input(input, mapper) do
    input
    |> lines()
    |> Enum.map(mapper)
  end

  def hitcount(nums) do
    Enum.reduce(nums, %{}, fn n, acc ->
      Map.update(acc, n, 1, &(&1 + 1))
    end)
  end

  def sign(0), do: 0
  def sign(x), do: x / abs(x)
end
