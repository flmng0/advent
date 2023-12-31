open Base
open Stdio

let () =
  Solver.add (module Day1);
  Solver.add (module Day2);
  Solver.add (module Day3);
  Solver.add (module Day4);
  Solver.add (module Day5);
  Solver.add (module Day6);
  Solver.add (module Day7);
  Solver.add (module Day8);
  Solver.add (module Day9);
  Solver.add (module Day10);
  Solver.add (module Day11);
  Solver.add (module Day13);
  Solver.add (module Day14);
  Solver.add (module Day15);
  Solver.add (module Day16);
  ()

let solve ?(input = stdin) ?(b_side = false) day =
  let part_string = if b_side then "B" else "A" in

  printf "Attempting to solve for day %i part %s.\n" day part_string;

  let solver = Solver.get day in

  let input = In_channel.input_all input in

  let solve = if b_side then solver.part_b else solver.part_a in
  try print_endline (solve input)
  with _ ->
    let message =
      Printf.sprintf "Solving for day %i part %s failed. See above." day
        part_string
    in

    print_endline message
