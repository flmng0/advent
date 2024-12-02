defmodule Advent.Year2024.Day2 do
  @behaviour Advent.Solver

  defdelegate sign(x), to: Advent

  defp parse(input) do
    parse_line = fn line ->
      String.split(line, " ", trim: true) |> Enum.map(&String.to_integer/1)
    end

    Advent.map_input(input, parse_line)
  end

  defp sign_of([x, y | _]), do: sign(y - x)

  defp is_safe?(reports) do
    desired_sign = sign_of(reports)

    [_ | tail] = reports

    Enum.zip(reports, tail)
    |> Enum.all?(fn {x, y} ->
      diff = y - x

      sign(diff) == desired_sign and
        abs(diff) in 1..3
    end)
  end

  @impl Advent.Solver
  def solve_a(input) do
    parse(input)
    |> Enum.count(&is_safe?/1)
  end

  @impl Advent.Solver
  def solve_b(input) do
    # I'm so frustrated that this ended up needing to be brute forced.
    #
    # I want to come back to this at some point
    parse(input)
    |> Enum.count(fn reports ->
      if is_safe?(reports) do
        true
      else
        Enum.with_index(reports, fn _, i -> i end)
        |> Enum.any?(fn i ->
          {l, [_ | r]} = Enum.split(reports, i)

          is_safe?(l ++ r)
        end)
      end
    end)
  end
end
