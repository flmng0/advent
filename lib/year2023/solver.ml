type solver_part = string -> string

module type S = sig
  val part_a : solver_part
  val part_b : solver_part
  val day : int
end

type solver = { part_a : solver_part; part_b : solver_part }

let solvers : (int, solver) Hashtbl.t = Hashtbl.create 25

let add (module M : S) =
  Hashtbl.add solvers M.day { part_a = M.part_a; part_b = M.part_b }

let get day = Hashtbl.find solvers day
