defmodule Advent.Year2024.Day7Test do
  @input "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
"

  use DayTemplate, day: 7, input: @input, part_a: 3749, part_b: 11387

  alias Advent.Year2024.Day7

  test "input parsed correctly" do
    assert [{190, [10, 19]}, {83, [17, 5]}] = Day7.parse("190: 10 19\n83: 17 5\n")
  end
end
