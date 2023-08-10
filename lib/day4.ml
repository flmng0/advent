open Util

type range = { first : int; last : int }

let inside a b =
  let inside' a b = a.first <= b.first && a.last >= b.last in
  inside' a b || inside' b a

let intersect a b =
  a.first <= b.last && a.last >= b.first

let parse_range text =
  let nums = String.split_on_char '-' text |> List.map int_of_string in
  match nums with
  | [ a; b ] -> { first = a; last = b }
  | _ -> invalid_arg "Could not parse range, unexpected format"

let range_pair line =
  let ranges = String.split_on_char ',' line |> List.map parse_range in
  match ranges with
  | [ a; b ] -> (a, b)
  | _ -> invalid_arg "Failed to parse range pair, unexpected format"

let part_a input =
  let pairs = line_seq input |> Seq.map range_pair in
  let intersecting = Seq.filter (fun (a, b) -> inside a b) pairs in
  let total = Seq.length intersecting in

  Printf.printf "Count of intersecting pairs: %d\n" total

let part_b input =
  let pairs = line_seq input |> Seq.map range_pair in
  let intersecting = Seq.filter (fun (a, b) -> intersect a b) pairs in
  let total = Seq.length intersecting in

  Printf.printf "Count of intersecting pairs: %d\n" total
