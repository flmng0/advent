module type Solver = sig
  val part_a : in_channel -> unit
  val part_b : in_channel -> unit
end

let solve ?(input = stdin) ?(b_side = false) day =
  let solver_mod =
    try
      match day with
      | 1 -> (module Day1 : Solver)
      | 2 -> (module Day2 : Solver)
      | _ -> raise Not_found
    with Not_found ->
      let msg = Format.sprintf "Solver not implemented for day %d" day in
      raise (Failure msg)
  in
  let module S = (val solver_mod : Solver) in
  let solver = if b_side then S.part_b else S.part_a in

  solver input
