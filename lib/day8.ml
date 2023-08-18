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

  type dir = North | East | South | West

  let move n dir =
    match dir with
    | North -> (0, -n)
    | East -> (n, 0)
    | South -> (0, n)
    | West -> (-n, 0)

  let is_hidden x y g =
    let this_h = Option.get (get x y g) in

    let rec check_dir i dir =
      let xoff, yoff = move i dir in

      match get (x + xoff) (y + yoff) g with
      | None -> false
      | Some h -> if h >= this_h then true else check_dir (i + 1) dir
    in

    [ North; East; South; West ] |> List.for_all (check_dir 1)

  let scenic_score x y g =
    let this_h = Option.get (get x y g) in
    let rec score_dir i dir =
      let xoff, yoff = move i dir in

      match get (x + xoff) (y + yoff) g with
      | None -> i - 1
      | Some h -> if h >= this_h then i else score_dir (i + 1) dir
    in

    let scores = [ North; East; South; West ] |> List.map (score_dir 1) in
    List.fold_left ( * ) 1 scores

  let seq_of_coords g =
    Seq.ints 0 |> Seq.take (Array.length g.data) |> Seq.map (fun i -> coord i g)

  let best_scenic_score g =
    seq_of_coords g
    |> Seq.map (fun (x, y) -> scenic_score x y g)
    |> Seq.fold_left max 0

  let count_hidden g =
    seq_of_coords g
    |> Seq.map (fun (x, y) -> if is_hidden x y g then 1 else 0)
    |> Seq.fold_left ( + ) 0

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

  let v_count = Grid.count_visible grid in
  print_int v_count;
  print_newline ()

let part_b ch =
  let lines = get_lines ch in
  let grid = Grid.of_lines lines in

  let score = Grid.best_scenic_score grid in
  print_int score;
  print_newline ()
