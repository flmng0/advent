let year = ref 2022
let day = ref 1
let b_side = ref false

let speclist =
  [
    ("-year", Arg.Set_int year, "Solve for the given year");
    ("-day", Arg.Set_int day, "Solve for the given day");
    ("-b", Arg.Set b_side, "Solve the 2nd part");
  ]

let usage_msg = "advent -day <day> [-year <year>]"

let () =
  Arg.parse speclist ignore usage_msg;

  let path = Printf.sprintf "inputs/day%d.txt" !day in
  let input = open_in path in
  let b_side = !b_side in

  Advent.solve !day ~input ~b_side
