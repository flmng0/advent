open Base
open Stdio

let () =
  Solver.add (module Day1)
  
let solve ?(input = stdin) ?(b_side = false) day =
  let solver = Solver.get day in

  let input = In_channel.input_all input in

  let solve = if b_side then solver.part_b else solver.part_a in
  try
    print_endline (solve input)
  with
    | _ -> 
      let part = if b_side then "B" else "A" in
      let message = Printf.sprintf "Solving for day %i part %s failed. See above." day part in

      print_endline message
  
