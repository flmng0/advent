defmodule Advent.Solver do
  @doc """
  Solve Part A of a given day, with provided input.
  """
  @callback solve_a(String.t()) :: integer()

  @doc """
  Solve Part B of a given day, with provided input.
  """
  @callback solve_b(String.t()) :: integer()

  @optional_callbacks [solve_a: 1, solve_b: 1]
end
