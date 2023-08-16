let day = 3

open Util
module CharSet = Set.Make (Char)

let sack_set l =
  let chars = String.to_seq l in
  CharSet.empty |> CharSet.add_seq chars

let priority = function
  | 'a' .. 'z' as c -> 1 + Char.code c - Char.code 'a'
  | 'A' .. 'Z' as c -> 27 + Char.code c - Char.code 'A'
  | _ -> raise (Invalid_argument "Character not alphabetical")

let get_sack_priority l =
  (* They're always even length, so not bad *)
  let half_len = String.length l / 2 in
  let a = String.sub l 0 half_len in
  let b = String.sub l half_len half_len in

  let a_set = sack_set a in
  let b_set = sack_set b in

  let issue = CharSet.inter a_set b_set |> CharSet.choose in
  Printf.printf "Issue for sack \"%s\" - %c\n" l issue;

  priority issue

let get_group_badge group =
  let hd = List.hd group in
  let tl = List.tl group in
  List.fold_left CharSet.inter hd tl |> CharSet.choose

let part_a input =
  let priorities = line_seq input |> Seq.map get_sack_priority in
  let total = Seq.fold_left ( + ) 0 priorities in

  Printf.printf "Total priority: %d\n" total;
  ()

let part_b input =
  let badges =
    line_seq input |> seq_windows 3
    |> Seq.map (List.map sack_set)
    |> Seq.map get_group_badge
  in

  let priorities = Seq.map priority badges in
  let total = Seq.fold_left ( + ) 0 priorities in

  Printf.printf "Total priority: %d\n" total;
  ()
