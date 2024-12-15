defmodule Advent.Year2024.Day6 do
  @behaviour Advent.Solver

  alias Advent.CharGrid

  def parse(input) do
    grid = CharGrid.parse(input)

    {gpos, walls} =
      for {pos, c} <- grid.chars, reduce: {nil, []} do
        {gpos, walls} ->
          case c do
            ?# ->
              {gpos, [pos | walls]}

            ?^ ->
              {pos, walls}

            _ ->
              {gpos, walls}
          end
      end

    %{gpos: gpos, walls: walls, width: grid.width, height: grid.height}
  end

  def move_guard({gx, gy}, walls, facing) do
    hittest = fn
      :up, {x, y} -> {gx == x and gy > y, {x, y + 1}}
      :down, {x, y} -> {gx == x and gy < y, {x, y - 1}}
      :right, {x, y} -> {gx < x and gy == y, {x - 1, y}}
      :left, {x, y} -> {gx > x and gy == y, {x + 1, y}}
    end

    sort = fn
      :up, {_x, y} -> -y
      :down, {_x, y} -> y
      :right, {x, _y} -> x
      :left, {x, _y} -> -x
    end

    walls
    |> Enum.map(fn wall ->
      {hit, pos} = hittest.(facing, wall)
      if hit, do: pos
    end)
    |> Enum.filter(&is_tuple/1)
    |> Enum.sort_by(&sort.(facing, &1))
    |> Enum.at(0)
  end

  def line_positions({x1, y1} = _from, {x2, y2} = _to) do
    for x <- x1..x2, y <- y1..y2 do
      {x, y}
    end
  end

  def turn(:up), do: :right
  def turn(:right), do: :down
  def turn(:down), do: :left
  def turn(:left), do: :up

  def process(grid) do
    process(grid, :up, [])
  end

  def process(
        %{gpos: {gx, gy} = gpos, walls: walls, width: width, height: height} = grid,
        facing,
        visited
      ) do
    with {_x, _y} = new_pos <- move_guard(gpos, walls, facing) do
      new_visited = line_positions(gpos, new_pos)

      process(%{grid | gpos: new_pos}, turn(facing), new_visited ++ visited)
    else
      nil ->
        to =
          case facing do
            :up -> {gx, 0}
            :down -> {gx, height - 1}
            :left -> {0, gy}
            :right -> {width - 1, gy}
          end

        final = line_positions(gpos, to) ++ visited
        MapSet.new(final)
    end
  end

  @impl Advent.Solver
  def solve_a(input) do
    parse(input) |> process() |> MapSet.size()
  end
end
