module type Solver = sig
  val solve : in_channel -> unit
  val solve_b : in_channel -> unit
end

let solve ?(input = stdin) ?(b_side = false) day =
  try
    let solver_mod =
      match day with
      | 1 -> (module Day1 : Solver)
      | 2 -> (module Day2 : Solver)
      | _ -> raise Not_found
    in
    let module S = (val solver_mod : Solver) in
    let solver = if b_side then S.solve_b else S.solve in

    solver input
  with Not_found -> Printf.printf "Solver not implemented for day %d\n" day
