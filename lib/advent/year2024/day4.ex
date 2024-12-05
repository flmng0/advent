defmodule Advent.Year2024.Day4 do
  @behaviour Advent.Solver

  alias Advent.CharGrid

  defp find_ocurrences(%CharGrid{width: width, height: height, chars: chars}, word) do
    word = String.to_charlist(word)

    test = fn x, y ->
      for dx <- -1..1, dy <- -1..1, dx != 0 or dy != 0 do
        word
        |> Enum.with_index()
        |> Enum.all?(fn {c, i} ->
          x = x + dx * i
          y = y + dy * i

          x in 0..(width - 1) and y in 0..(height - 1) and c == chars[{x, y}]
        end)
      end
    end

    for x <- 0..(width - 1), y <- 0..(height - 1), reduce: 0 do
      total ->
        count = Enum.count(test.(x, y), & &1)
        total + count
    end
  end

  defp find_cross_mas(%CharGrid{width: width, height: height, chars: chars}) do
    center = ?A
    tails = [?M, ?S]

    xrange = 0..(width - 1)
    yrange = 0..(height - 1)

    # Given top-left coord, test against shape
    test = fn x, y ->
      if chars[{x, y}] == center do
        diagonals =
          for dx <- [-1, 1],
              dy <- [-1, 1],
              (x + dx) in xrange and (y + dy) in yrange,
              do: chars[{x + dx, y + dy}]

        check_diagonals = fn ->
          diagonals
          |> Enum.frequencies()
          |> Enum.all?(fn {char, count} -> count == 2 and char in tails end)
        end

        List.first(diagonals) != List.last(diagonals) and
          check_diagonals.()
      else
        false
      end
    end

    for x <- xrange, y <- yrange, reduce: 0 do
      total ->
        total + if test.(x, y), do: 1, else: 0
    end
  end

  @impl Advent.Solver
  def solve_a(input) do
    CharGrid.parse(input)
    |> find_ocurrences("XMAS")
  end

  @impl Advent.Solver
  def solve_b(input) do
    CharGrid.parse(input) |> find_cross_mas()
  end
end
