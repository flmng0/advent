defmodule Advent.Year2024.Day9Test do
  @input "2333133121414131402"
  use DayTemplate, input: @input, part_a: 1928

  alias Advent.Year2024.Day9

  test "compacts correctly" do
    compacted =
      Day9.parse(@input)
      |> Day9.compact()
      |> Map.values()
      |> Enum.map(&Integer.to_string/1)
      |> List.to_string()

    assert compacted == "0099811188827773336446555566"
  end
end
