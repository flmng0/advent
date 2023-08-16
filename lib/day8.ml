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
    if x < 0 || x >= g.cols || y < 0 || y >= g.rows then None
    else
      let i = index x y g in
      Some g.data.(i)

  type dir = Top | Right | Bottom | Left

  let move n dir =
    match dir with
    | Top -> (0, -n)
    | Right -> (n, 0)
    | Bottom -> (0, n)
    | Left -> (-n, 0)

  let is_hidden x y g =
    let this_h = Option.get (get x y g) in

    let rec check_dir i dir =
      let xoff, yoff = move i dir in

      match get (x + xoff) (y + yoff) g with
      | None -> false
      | Some h -> if h >= this_h then true else check_dir (i + 1) dir
    in

    [ Top; Right; Bottom; Left ] |> List.for_all (check_dir 1)

  let scenic_score x y g =
    let this_h = Option.get (get x y g) in
    let rec score_dir i dir =
      let xoff, yoff = move i dir in

      match get (x + xoff) (y + yoff) g with
      | None -> i - 1
      | Some h -> if h >= this_h then i else score_dir (i + 1) dir
    in

    let scores = [ Top; Right; Bottom; Left ] |> List.map (score_dir 1) in
    List.fold_left ( * ) 1 scores

  let best_scenic_score g =
    Seq.ints 0
    |> Seq.take (Array.length g.data)
    |> Seq.fold_left
         (fun last i ->
           let x, y = coord i g in
           let score = scenic_score x y g in
           if score > last then score else last)
         0

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

  let count_visible g =
    let total = Array.length g.data in
    total - count_hidden g

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

let part_a ch =
  let lines = get_lines ch in
  let grid = Grid.of_lines lines in

  Grid.pp grid;
  let v_count = Grid.count_visible grid in
  print_int v_count;
  print_newline ()

let part_b ch =
  let lines = get_lines ch in
  let grid = Grid.of_lines lines in

  let score = Grid.best_scenic_score grid in
  print_int score;
  print_newline ()
