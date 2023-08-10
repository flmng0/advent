open Util

let elf line = int_of_string line

let get_elves lines =
  let rec elves_loop all curr = function
    | [] -> curr :: all
    | h :: t when h = "" -> elves_loop (curr :: all) 0 t
    | h :: t -> elves_loop all (curr + elf h) t
  in
  elves_loop [] 0 lines

let part_a input =
  let lines = get_lines input in
  let elves = get_elves lines in

  let elf = List.fold_left max Int.min_int elves in

  Printf.printf "Highest scoring elf: %d\n" elf;
  ()

let part_b input =
  let elves = get_lines input |> get_elves |> List.sort compare |> List.rev in

  let best3 = firstn 3 elves in

  print_endline "Top 3 elves:";
  best3 |> List.iter (Printf.printf "\t- %n\n");

  Printf.printf "\nSum: %n\n" @@ sum best3;
  ()
