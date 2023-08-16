let day = 8

open Util

module Grid = struct
  type t = { data : int Array.t; cols : int; rows : int }

  let of_lines lines =
    let cols = List.hd lines |> String.length in
    let rows = List.length lines in

    let data = Array.make (cols * rows) 0 in
    let add_tree x y c =
      let height = String.make 1 c |> int_of_string in
      let i = x + (y * cols) in
      data.(i) <- height
    in
    let add_row y line = String.iteri (fun x c -> add_tree x y c) line in
    List.iteri add_row lines;

    { data; cols; rows }

  let coord i g = (i mod g.cols, i / g.cols)
  let index x y g = x + (y * g.cols)

  let get x y g =
    let i = index x y g in
    g.data.(i)

  type dir = Top | Right | Bottom | Left

  let move n dir =
    match dir with
    | Top -> (-n, 0)
    | Right -> (0, n)
    | Bottom -> (n, 0)
    | Left -> (0, -n)

  let is_hidden x y g =
    let this_h = get x y g in
    let check_taller xoff yoff =
      let h = get (x + xoff) (y + yoff) g in
      h >= this_h
    in

    let rec check_dir i dir =
      let xoff, yoff = move i dir in
      try if check_taller xoff yoff then true else check_dir (i + 1) dir
      with _ -> false
    in

    [ Top; Right; Bottom; Left ] |> List.for_all (check_dir 1)

  let count_hidden g =
    Seq.ints 0
    |> Seq.take (Array.length g.data)
    |> Seq.fold_left
         (fun acc i ->
           let x, y = coord i g in
           Printf.printf "For coordinate (%d, %d): Hidden = %b\n" x y
             (is_hidden x y g);
           match is_hidden x y g with true -> acc + 1 | false -> acc)
         0

  let pp g =
    Printf.printf "Columns: %d\nRows: %d\n\nData: \n" g.cols g.rows;
    Array.iteri
      (fun i h ->
        let m = i mod g.cols in
        if m = 0 then print_string "  ";
        Printf.printf "%d" h;
        if m = g.cols - 1 then print_newline ())
      g.data;
    print_newline ();
    ()
end

let solve ch =
  let lines = get_lines ch in
  let grid = Grid.of_lines lines in

  Grid.pp grid;
  let h_count = Grid.count_hidden grid in
  print_int h_count;
  print_newline ()

let part_a ch = solve ch
let part_b _in = ()
