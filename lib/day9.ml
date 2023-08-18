let day = 9

open Util

module IntPair = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Stdlib.compare x0 x1 with 0 -> Stdlib.compare y0 y1 | c -> c
end

module PairSet = Set.Make (IntPair)

(* Uses the Cartesian coordinate system *)
module Rope = struct
  type t = { tl : int * int; hd : int * int }

  let indentity = { tl = (0, 0); hd = (0, 0) }

  type dir = Up | Down | Left | Right
  type motion = { dir : dir; count : int }

  let dists (x0, y0) (x1, y1) = (x1 - x0, y1 - y0)
  let sign n = match n with 0 -> 0 | n -> n / abs n
  let add (x, y) (dx, dy) = (x + dx, y + dy)

  let move dir =
    match dir with
    | Up -> (0, 1)
    | Down -> (0, -1)
    | Left -> (-1, 0)
    | Right -> (1, 0)

  let apply d r =
    let hd = add r.hd (move d) in
    let dx, dy = dists r.tl hd in
    let tx, ty = r.tl in
    let tl =
      if abs dx > 1 || abs dy > 1 then (tx + sign dx, ty + sign dy) else (tx, ty)
    in
    { hd; tl }

  let apply_many ms r =
    let visited, _ =
      List.to_seq ms
      |> Seq.flat_map (fun m -> Seq.repeat m.dir |> Seq.take m.count)
      |> Seq.fold_left
           (fun (vis_acc, r) d ->
             let r = apply d r in
             (PairSet.add r.tl vis_acc, r))
           (PairSet.empty, r)
    in

    visited
end

let parse_motion l =
  let open Rope in
  Scanf.sscanf l "%s@ %d" (fun dir count ->
      let dir =
        match dir with
        | "U" -> Up
        | "D" -> Down
        | "R" -> Right
        | "L" -> Left
        | _ -> invalid_arg "Unexpected direction"
      in
      { count; dir })

let part_a ch =
  let ms = get_lines ch |> List.map parse_motion in
  let visited = Rope.indentity |> Rope.apply_many ms in
  let count = PairSet.elements visited |> List.length in

  print_int count;
  print_newline ()

let part_b _ch = ()
