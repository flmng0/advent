defmodule Advent.Year2024.Day3 do
  @behaviour Advent.Solver

  defp parse(input, enabled \\ :ignore, acc \\ [])

  defp parse("do()" <> rest, :ignore, acc), do: parse(rest, :ignore, acc)
  defp parse("do()" <> rest, _enabled, acc), do: parse(rest, true, acc)

  defp parse("don't()" <> rest, :ignore, acc), do: parse(rest, :ignore, acc)
  defp parse("don't()" <> rest, _enabled, acc), do: parse(rest, false, acc)

  defp parse("mul(" <> rest, enabled, acc) when enabled in [true, :ignore] do
    with {a, "," <> rest} <- Integer.parse(rest),
         {b, ")" <> rest} <- Integer.parse(rest) do
      parse(rest, enabled, [{a, b}] ++ acc)
    else
      {_, rest} -> parse(rest, enabled, acc)
    end
  end

  defp parse(<<_, rest::binary>>, enabled, acc) do
    parse(rest, enabled, acc)
  end

  defp parse("", _enabled, acc) do
    acc
  end

  @impl Advent.Solver
  def solve_a(input) do
    parse(input)
    |> Enum.map(fn {a, b} -> a * b end)
    |> Enum.sum()
  end

  @impl Advent.Solver
  def solve_b(input) do
    parse(input, true)
    |> Enum.map(fn {a, b} -> a * b end)
    |> Enum.sum()
  end
end
