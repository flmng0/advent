open Base

let strip_trail lines =
  let rec strip_trail' = function
    | head :: rest ->
        if String.length head = 0 then strip_trail' rest else head :: rest
    | [] -> invalid_arg "No non-empty lines"
  in
  strip_trail' (List.rev lines) |> List.rev

let lines_of_string ?(include_trail = false) data =
  let all = String.split_lines data in
  if include_trail then all else strip_trail all

let read_nums text =
  text |> String.split ~on:' ' |> List.filter_map ~f:(fun t -> String.strip t |> Int.of_string_opt)

let chunks_of_string data =
  let rec loop acc current = function
    | line :: rest -> (
        match line with
        | "" ->
            let chunk = List.rev current |> String.concat_lines in
            loop (chunk :: acc) [] rest
        | line -> loop acc (line :: current) rest)
    | [] -> 
        let chunk = List.rev current |> String.concat_lines in
        List.rev (chunk :: acc)
  in

  let lines = String.split_lines data in
  loop [] [] lines

