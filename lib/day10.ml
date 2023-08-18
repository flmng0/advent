let day = 10

open Util

module CRT = struct
  type instruction = AddX of int | NoOp
  type t = instruction Seq.t

  let instruction_time = function AddX _ -> 2 | NoOp -> 1
  let apply x = function AddX y -> x + y | NoOp -> x

  let instruction_of_string s =
    match String.split_on_char ' ' s with
    | [ "addx"; num ] -> AddX (int_of_string num)
    | [ "noop" ] -> NoOp
    | _ -> raise Not_found

  let collect_important ~period ~offset instructions =
    let is_important c = (c - offset) mod period = 0 in

    let rec loop acc x cycle runtime = function
      | [] -> List.rev acc
      | current :: rest ->
          let acc = if is_important cycle then (x * cycle) :: acc else acc in
          (* GTE just in case... probably won't ever happen though *)
          let cycle = cycle + 1 in
          if runtime = instruction_time current then
            let x = match current with AddX y -> x + y | NoOp -> x in
            loop acc x cycle 1 rest
          else loop acc x cycle (runtime + 1) (current :: rest)
    in

    loop [] 1 0 0 instructions
end

let part_a ch =
  let open CRT in
  let instructions =
    get_lines ch |> List.filter not_empty |> List.map instruction_of_string
  in
  let important = collect_important ~period:40 ~offset:20 instructions in

  let total = List.fold_left ( + ) 0 important in
  print_int total;
  print_newline ()

let part_b _ch = ()
