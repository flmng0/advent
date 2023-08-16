module Solver = struct
  module K = struct
    type t = int * bool

    let compare (day1, b1) (day2, b2) =
      match Stdlib.compare day1 day2 with 0 -> Stdlib.compare b1 b2 | c -> c
  end

  module type S = sig
    val part_a : in_channel -> unit
    val part_b : in_channel -> unit
    val day : int
  end

  module Map = Map.Make (K)

  let solvers =
    let add (module M : S) map =
      map |> Map.add (M.day, false) M.part_a |> Map.add (M.day, true) M.part_b
    in
    Map.empty
    |> add (module Day1)
    |> add (module Day2)
    |> add (module Day3)
    |> add (module Day4)
    |> add (module Day5)
    |> add (module Day6)

  let get day b = Map.find_opt (day, b) solvers
end

let solve ?(input = stdin) ?(b_side = false) day =
  let solver =
    match Solver.get day b_side with
    | Some s -> s
    | None ->
        let part = if b_side then "B" else "A" in
        let msg =
          Printf.sprintf "No solver implemented for day %d, part %s" day part
        in
        raise (invalid_arg msg)
  in

  solver input
