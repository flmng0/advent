let elf line = int_of_string line
let sum l = List.fold_left ( + ) 0 l
let firstn n list = list |> List.to_seq |> Seq.take n |> List.of_seq

let input_line_opt chan =
  try Some (input_line chan, chan)
  with End_of_file ->
    close_in chan;
    None

let get_lines chan = Seq.unfold input_line_opt chan |> List.of_seq

let get_elves lines =
  let rec elves_loop all curr = function
    | [] -> curr :: all
    | h :: t when h = "" -> elves_loop (curr :: all) 0 t
    | h :: t -> elves_loop all (curr + elf h) t
  in
  elves_loop [] 0 lines

let day1a input =
  let lines = get_lines input in
  let elves = get_elves lines in

  let elf = List.fold_left max Int.min_int elves in

  Printf.printf "Highest scoring elf: %d\n" elf;
  ()

let day1b input =
  let elves = get_lines input |> get_elves |> List.sort compare |> List.rev in

  let best3 = firstn 3 elves in

  print_endline "Top 3 elves:";
  best3 |> List.iter (Printf.printf "\t- %n\n");

  Printf.printf "\nSum: %n\n" @@ sum best3;

  ()

let solvers = [ (day1a, day1b) ]

let solve ?(input = stdin) ?(b_side = false) day =
  let a, b = match day with 1 -> (day1a, day1b) | _ -> raise Not_found in
  let solver = if b_side then b else a in

  solver input
