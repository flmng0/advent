let solve ?(input = stdin) ?(b_side = false) year day =
  match year with
  | 2022 -> Year2022.solve day ~input ~b_side
  | 2023 -> Year2023.solve day ~input ~b_side
  | _ -> raise (Not_found)
