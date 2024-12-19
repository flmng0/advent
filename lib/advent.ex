defmodule Advent do
  @moduledoc """
  Utilities that are used throughout the challenge.
  """

  defmodule CharGrid do
    defstruct width: 0, height: 0, chars: %{}

    @type t() :: %__MODULE__{
            width: integer(),
            height: integer(),
            chars: map()
          }

    @spec parse(String.t()) :: t()
    def parse(input) do
      chars = input |> String.trim_trailing() |> String.to_charlist()

      width = Enum.find_index(chars, &(&1 == ?\n))
      # +1 acounting for trim_trailing
      height = Enum.count(chars, &(&1 == ?\n)) + 1

      filtered = chars |> Enum.filter(&(&1 != ?\n)) |> Enum.with_index()

      chars =
        for {c, i} <- filtered, into: %{} do
          x = Integer.mod(i, width)
          y = Integer.floor_div(i, width)

          {{x, y}, c}
        end

      %__MODULE__{width: width, height: height, chars: chars}
    end

    def size(%__MODULE__{width: width, height: height}), do: width * height
  end

  # The below additions can be credited to:
  # https://stackoverflow.com/a/64108851
  @spec distribute(list(), list()) :: list(list())
  def distribute(xs, ys), do: Enum.map(xs, &[&1 | ys])

  @spec distribute_on_all(list(), list(list())) :: list(list())
  def distribute_on_all(x, l), do: Enum.map(l, &distribute(x, &1)) |> Enum.concat()

  def repeat(n, f, x) do
    for _ <- 1..n, reduce: x do
      x ->
        f.(x)
    end
  end

  def combinations(items, n) do
    repeat(n, &distribute_on_all(items, &1), [[]])
  end

  # Get chunks of input splitting on new lines
  def chunks(input) do
    chunk_fn = fn
      "", acc ->
        {:cont, Enum.reverse(acc), []}

      elem, acc ->
        {:cont, [elem | acc]}
    end

    after_fn = fn
      [] -> {:cont, []}
      acc -> {:cont, Enum.reverse(acc), []}
    end

    input
    |> lines()
    |> Enum.chunk_while(
      [],
      chunk_fn,
      after_fn
    )
  end

  def lines(input) do
    input |> String.trim_trailing() |> String.splitter("\n") |> Stream.map(&String.trim/1)
  end

  def map_input(input, mapper) do
    input
    |> lines()
    |> Enum.map(mapper)
  end

  def sign(0), do: 0
  def sign(x), do: x / abs(x)

  def except_index(list, index) do
    {l, [_ | r]} = Enum.split(list, index)

    l ++ r
  end

  def parse_int!(n) do
    {n, ""} = Integer.parse(n)
    n
  end
end
