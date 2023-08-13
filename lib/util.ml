let not_empty s = String.length s > 0
let sum l = List.fold_left ( + ) 0 l
let firstn n list = list |> List.to_seq |> Seq.take n |> List.of_seq

let input_line_opt chan =
  try Some (input_line chan, chan)
  with End_of_file ->
    close_in chan;
    None

let line_seq chan = Seq.unfold input_line_opt chan
let get_lines chan = List.of_seq @@ line_seq chan

let seq_windows n seq =
  let open Seq in
  let rec loop acc seq () =
    if List.length acc = n then Cons (List.rev acc, loop [] seq)
    else
      match seq () with Nil -> Nil | Cons (v, next) -> loop (v :: acc) next ()
  in
  loop [] seq

let split_seq pred seq =
  let rec loop acc_a acc_b is_after (seq : string Seq.t) =
    match seq () with
    | Nil -> (List.rev acc_a, List.rev acc_b)
    | Cons (line, seq) ->
        if pred line then loop acc_a acc_b true seq
        else if is_after then loop acc_a (line :: acc_b) is_after seq
        else loop (line :: acc_a) acc_b is_after seq
  in
  loop [] [] false seq

let maybe f = function Some x -> f x | None -> ()
let print_all = List.iter print_endline
