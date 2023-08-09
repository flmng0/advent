let sum l = List.fold_left ( + ) 0 l
let firstn n list = list |> List.to_seq |> Seq.take n |> List.of_seq

let input_line_opt chan =
  try Some (input_line chan, chan)
  with End_of_file ->
    close_in chan;
    None

let get_lines chan = Seq.unfold input_line_opt chan |> List.of_seq
