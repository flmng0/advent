defmodule Advent.Year2024.Day7 do
  @behaviour Advent.Solver

  def parse(input) do
    parse_line = fn line ->
      {result, ":" <> rest} = Integer.parse(line)
      operands = String.split(rest) |> Enum.map(&Advent.parse_int!/1)
      {result, operands}
    end

    Advent.map_input(input, parse_line)
  end

  def test_equation({result, operands}, operators) do
    combinations = Advent.combinations(operators, length(operands) - 1)

    [first | ns] = operands

    Enum.any?(
      combinations,
      fn ops ->
        computed =
          Enum.zip(ns, ops)
          |> Enum.reduce(
            first,
            fn
              {x, :mul}, acc -> acc * x
              {x, :add}, acc -> acc + x
              {x, :cat}, acc -> Advent.parse_int!(to_string(acc) <> to_string(x))
            end
          )

        computed == result
      end
    )
  end

  def solve_a(input) do
    equations = parse(input)

    for eq <- equations, reduce: 0 do
      total ->
        if test_equation(eq, [:mul, :add]) do
          {result, _} = eq
          total + result
        else
          total
        end
    end
  end

  def solve_b(input) do
    equations = parse(input)

    for eq <- equations, reduce: 0 do
      total ->
        if test_equation(eq, [:mul, :add, :cat]) do
          {result, _} = eq
          total + result
        else
          total
        end
    end
  end
end
