defmodule Advent.Year2024.Day4 do
  @behaviour Advent.Solver

  defmodule WordSearch do
    defstruct width: 0, height: 0, chars: []

    @type t() :: %__MODULE__{
            width: integer(),
            height: integer(),
            chars: map()
          }
  end

  @spec parse(String.t()) :: WordSearch.t()
  defp parse(input) do
    chars = input |> String.trim_trailing() |> String.to_charlist()

    width = Enum.find_index(chars, &(&1 == ?\n))
    height = Enum.count(chars, &(&1 == ?\n)) + 1

    filtered = chars |> Enum.filter(&(&1 != ?\n)) |> Enum.with_index()

    chars =
      for {c, i} <- filtered, into: %{} do
        x = Integer.mod(i, width)
        y = Integer.floor_div(i, width)

        {{x, y}, c}
      end

    %WordSearch{width: width, height: height, chars: chars}
  end

  defp find_ocurrences(%WordSearch{width: width, height: height, chars: chars}, word) do
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

  defp find_cross_mas(%WordSearch{width: width, height: height, chars: chars}) do
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

        freqs = Enum.frequencies(diagonals)

        Enum.all?(tails, fn t -> freqs[t] == 2 end) and
          List.first(diagonals) != List.last(diagonals)
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
    parse(input)
    |> find_ocurrences("XMAS")
  end

  @impl Advent.Solver
  def solve_b(input) do
    parse(input) |> find_cross_mas()
  end
end
