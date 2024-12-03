defmodule Advent.Year2024.Day3Test do
  @input "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  @input_b "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  use DayTemplate, day: 3, part_a: 161, part_b: 48, input: @input, input_b: @input_b
end
