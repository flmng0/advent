defmodule DayTemplate do
  use ExUnit.CaseTemplate

  @moduledoc """
  Template case for day tests.

  This is intended to be used with the following options:

  - `:day`: An integer representing which day this is testing.
  - `:part_a`: The expected output for solving Part A of day `:day`.
  - `:part_b`: The expected output for solving Part B of day `:day`.
  - `:input`: The input string to be used when testing.

  # Example

      defmodule Advent.Year2024.Day1Test do
        @input "..."

        use DayTemplate, day: 1, part_a: 11, part_b: 31, input: @input
      end
  """

  using options do
    quote do
      setup context do
        module_name =
          __MODULE__
          |> Atom.to_string()
          |> String.trim_trailing("Test")
          |> String.to_atom()

        {:module, solver} = Code.ensure_loaded(module_name)

        unquote(options) ++ [solver: solver]
      end

      describe "sample tests of day #{unquote(options)[:day]}" do
        test "part a", context do
          assert context.solver.solve_a(context.input) == context.part_a
        end

        test "part b", context do
          assert context.solver.solve_b(context.input) == context.part_b
        end
      end
    end
  end
end
