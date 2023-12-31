let not_empty s = String.length s > 0
let sum l = List.fold_left ( + ) 0 l
let firstn n list = list |> List.to_seq |> Seq.take n |> List.of_seq

let input_line_opt chan =
  try Some (input_line chan, chan)
  with End_of_file ->
    close_in chan;
    None

let identity x = x
let line_seq chan = Seq.unfold input_line_opt chan
let get_lines chan = line_seq chan |> List.of_seq
let get_blob chan = Seq.fold_left ( ^ ) "" (line_seq chan)

let get_chunks chan =
  let chunks, _ =
    chan |> line_seq
    |> Seq.fold_left
         (fun (acc, lines) line ->
           match line with
           | "" ->
               let curr = String.concat "\n" (List.rev lines) in
               (curr :: acc, [])
           | l -> (acc, l :: lines))
         ([], [])
  in
  List.rev chunks

(* let get_chunks chan = *)
(*   let rec collect_chunk acc ch = *)
(*     let ret_val () = (List.rev acc, ch) in *)
(*     match input_line_opt ch with *)
(*     | None -> ret_val () *)
(*     | Some (line, ch) -> *)
(*         if line = "" then ret_val () else collect_chunk (line :: acc) ch *)
(*   in *)
(*   Cons (collect_chunk [] ch, fun () -> ) *)

let rec ints_step ?(step = 1) i () = Seq.Cons (i, ints_step (i + step))

let range ?(step = 1) start end_ =
  let n = abs (end_ - start) in
  ints_step ~step start |> Seq.take n

let rec skip n seq =
  match seq () with
  | Seq.Nil -> Seq.empty
  | Cons (_, seq) -> if n == 0 then seq else skip (n - 1) seq

let seq_windows n seq =
  let open Seq in
  let rec loop acc seq () =
    if List.length acc = n then Cons (List.rev acc, loop [] seq)
    else
      match seq () with Nil -> Nil | Cons (v, next) -> loop (v :: acc) next ()
  in
  loop [] seq

let split_seq pred seq =
  let rec loop acc_a acc_b is_after seq =
    match seq () with
    | Seq.Nil -> (List.rev acc_a, List.rev acc_b)
    | Cons (line, seq) ->
        if pred line then loop acc_a acc_b true seq
        else if is_after then loop acc_a (line :: acc_b) is_after seq
        else loop (line :: acc_a) acc_b is_after seq
  in
  loop [] [] false seq

let maybe f = function Some x -> f x | None -> ()
let print_all = List.iter print_endline
