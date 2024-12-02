# Elixir Advent of Code Solutions

[Advent of Code](https://adventofcode.com) solutions implemented using Elixir.


## Why Elixir?

I have always loved the idea of functional programming. In terms of fun-to-use languages,
I would normally use OCaml.

However, recently I have been doing a lot more projects in Phoenix, and advent of code is
always a good way to learn a new language.


## How It Works

> TL;DR: Check [`lib/mix/tasks/solve.ex`](lib/mix/tasks/solve.ex).

- Automatically downloads input files and caches them using a session token cookie (using [`Req`](https://github.com/wojtekmach/req)).
- Loads the "`Solver`" module dynamically, which implements the `Advent.Solver` behaviour.
- Solvers for a given year are located in `lib/advent/year<YEAR>/day<DAY>.ex`.
- Utilities will be implemented in `lib/advent.ex`.

## Testing

> TL;DR: Check [`test/support/day_template.ex`](test/support/day_template.ex).

Tests are written for provided example inputs in `test/year<YEAR>/day<DAY>_test.exs`.

Each test takes advantage of the `ExUnit.CaseTemplate` located at `test/support/day_template.ex`.

The case template automatically defines the tests given some additional parameters.

For more information look at [`day_template.ex`](test/support/day_template.ex) file.




