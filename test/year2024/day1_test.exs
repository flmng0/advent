defmodule Year2024.Day1Test do
  use ExUnit.Case

  @input "3   4
4   3
2   5
1   3
3   9
3   3
"

  @day 1

  def solver, do: Advent.Year2024.Day1

  def part_a, do: {@input, 11}
  def part_b, do: {@input, 31}

  test "day #{@day} part A example case" do
    {input, expected} = part_a()
    assert solver().solve_a(input) == expected
  end

  test "day #{@day} part B example case" do
    {input, expected} = part_b()
    assert solver().solve_b(input) == expected
  end
end
