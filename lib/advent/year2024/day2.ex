defmodule Advent.Year2024.Day2 do
  @behaviour Advent.Solver

  import Advent, only: [sign: 1, except_index: 2]

  defp parse(input) do
    parse_line = fn line ->
      String.split(line, " ", trim: true) |> Enum.map(&String.to_integer/1)
    end

    Advent.map_input(input, parse_line)
  end

  defp sign_of([x, y | _]), do: sign(y - x)

  defp is_safe?(reports) do
    desired_sign = sign_of(reports)

    reports
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.all?(fn [x, y] ->
      diff = y - x

      abs(diff) in 1..3 and sign(diff) == desired_sign
    end)
  end

  @impl Advent.Solver
  def solve_a(input) do
    parse(input)
    |> Enum.count(&is_safe?/1)
  end

  @impl Advent.Solver
  def solve_b(input) do
    parse(input)
    |> Enum.count(fn reports ->
      with false <- is_safe?(reports) do
        0..(length(reports) - 1)
        |> Enum.map(&except_index(reports, &1))
        |> Enum.any?(&is_safe?/1)
      end
    end)
  end
end
