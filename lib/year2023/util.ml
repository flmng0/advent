open Base

let strip_trail lines =
  let rec strip_trail' = function
    | head :: rest -> if String.length head = 0 then strip_trail' rest else head :: rest
    | [] -> invalid_arg "No non-empty lines"
  in
  strip_trail' (List.rev lines) |> List.rev

let lines_of_string ?(include_trail = false) data =
  let all = String.split ~on:'\n' data in
  if include_trail then all else strip_trail all
