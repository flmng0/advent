defmodule Mix.Tasks.Solve do
  use Mix.Task

  @current_year 2024
  @cwd File.cwd!()
  @inputs_dir Path.join(@cwd, "inputs")

  defp download_base, do: "https://adventofcode.com"

  defp download_path(year, day) do
    "/#{year}/day/#{day}/input"
  end

  defp input(year, day) do
    year_input_dir = Path.join(@inputs_dir, "#{year}")

    if not File.dir?(year_input_dir) do
      :ok = File.mkdir_p(year_input_dir)
    end

    input_path = Path.join(year_input_dir, "day#{day}.txt")

    if not File.regular?(input_path) do
      Mix.shell().info(
        "Input file for year #{year}, day #{day}, not found - attempting to download."
      )

      with {:ok, session} <- System.fetch_env("ADVENT_SESSION"),
           {:ok, response} <-
             Req.new(base_url: download_base())
             |> Req.Request.put_header("Cookie", "session=#{session}")
             |> Req.get(url: download_path(year, day)) do
        File.write!(input_path, response.body)
        {:ok, response.body}
      else
        :error ->
          {:error,
           "Failed to download input file, could not get ADVENT_SESSION environment variable"}

        {:error, _ex} ->
          {:error, "Failed to download input file, unknown error occurred"}
      end
    else
      case File.read(input_path) do
        {:ok, input} ->
          {:ok, input}

        {:error, _} ->
          {:error, "Failed to read input file"}
      end
    end
  end

  defp solver_module(year, day, part_b?) do
    # module = String.to_atom("Advent.Solvers.Year#{year}.Day#{day}")
    module = Module.concat([Advent, "Year#{year}", "Day#{day}"])

    cond do
      not match?({:module, _}, Code.ensure_loaded(module)) ->
        {:error,
         "No solver module exists for day #{day} of year #{year}! Looking for `#{module}`"}

      not part_b? and not function_exported?(module, :solve_a, 1) ->
        {:error, "Part A requested, but solver does not implement part A solution!"}

      part_b? and not function_exported?(module, :solve_b, 1) ->
        {:error, "Part B requested, but solver does not implement part B solution!"}

      true ->
        {:ok, module}
    end
  end

  def solve(%{year: year, day: day, part_b?: part_b?, stdin?: stdin?}) do
    input =
      if stdin? do
        {:ok, IO.read(:stdio, :eof)}
      else
        input(year, day)
      end

    with {:ok, input} <- input,
         {:ok, solver} <- solver_module(year, day, part_b?) do
      function = if part_b?, do: :solve_b, else: :solve_a
      solution = apply(solver, function, [input])

      Mix.shell().info("Solution: #{solution}")
    else
      {:error, msg} ->
        Mix.shell().error(msg)
    end
  end

  defp parse_args(args) when is_list(args) do
    parse_args(%{}, args)
  end

  defp parse_args(%{} = parsed, ["--year", year | rest] = _args) do
    Map.put(parsed, :year, year) |> parse_args(rest)
  end

  defp parse_args(%{} = parsed, ["--stdin" | rest] = _args) do
    Map.put(parsed, :stdin?, true) |> parse_args(rest)
  end

  defp parse_args(%{} = parsed, ["--b" | rest] = _args) do
    Map.put(parsed, :part_b?, true) |> parse_args(rest)
  end

  defp parse_args(%{} = parsed, [day | rest] = _args) do
    Map.put(parsed, :day, day) |> parse_args(rest)
  end

  defp parse_args(%{} = parsed, [] = _args) do
    parsed
  end

  @impl Mix.Task
  def run(args) do
    {:ok, _} = Application.ensure_all_started(:req)

    params = parse_args(args)

    if Map.has_key?(params, :day) do
      params
      |> Map.put_new(:year, @current_year)
      |> Map.put_new(:part_b?, false)
      |> Map.put_new(:stdin?, false)
      |> solve
    else
      Mix.shell().error("No day selected!")
    end
  end
end
