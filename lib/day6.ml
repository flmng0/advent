module CharSet = Set.Make (Char)

let is_unique text =
  let len =
    String.to_seq text |> CharSet.of_seq |> CharSet.elements |> List.length
  in
  len == String.length text

let solve n text =
  let rec aux i =
    let sub = try String.sub text i n with _ -> raise Not_found in
    if is_unique sub then i + n else aux (i + 1)
  in

  aux 0

let part_a input =
  let text = input_line input in
  let pos = solve 4 text in

  print_int pos;
  print_newline ()

let part_b input =
  let text = input_line input in
  let pos = solve 14 text in

  print_int pos;
  print_newline ()
