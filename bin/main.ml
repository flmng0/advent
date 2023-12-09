let year = ref 2023
let day = ref 1
let b_side = ref false
let use_stdin = ref false

let speclist =
  [
    ("-year", Arg.Set_int year, "Solve for the given year");
    ("-day", Arg.Set_int day, "Solve for the given day");
    ("-b", Arg.Set b_side, "Solve the 2nd part");
    ("-stdin", Arg.Set use_stdin, "Use stdin for the input values");
  ]

let anon_fun d =
  day := int_of_string d

let usage_msg = "advent [-year <year>] [-b] [-stdin] <day>"

let () =
  Arg.parse speclist anon_fun usage_msg;

  let input =
    if !use_stdin then stdin
    else
      let path = Printf.sprintf "inputs/%d/day%d.txt" !year !day in
      open_in path
  in
  let b_side = !b_side in

  Advent.solve !year !day ~input ~b_side;
  close_in input;

  ()
