let day = 8

open Util

type grid = { data : int Array.t; cols : int; rows : int }

let parse_grid lines =
  let cols = List.hd lines |> String.length in
  let rows = List.length lines in

  let data = Array.make (cols * rows) 0 in
  let add_line y line =
    String.iteri
      (fun x c ->
        let height = String.make 1 c |> int_of_string in
        let i = x + (y * cols) in
        data.(i) <- height)
      line
  in
  List.iteri add_line lines;

  { data; cols; rows }

let check_hidden x y grid = ()

let solve ch =
  let lines = get_lines ch in
  let grid = parse_grid lines in

  Array.iter (Printf.printf "%d") grid.data;
  ()

let part_a ch = solve ch
let part_b _in = ()
