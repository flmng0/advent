open Base

module Grid = struct
  type t = {
    data : (int * int) array;
    pad_rows : int list;
    pad_cols : int list;
  }

  let pairs i = Util.pairs (Array.length i.data)

  let distance ?(expansion = 1) g i j =
    let x1, y1 = g.data.(i) in
    let x2, y2 = g.data.(j) in

    let x1, x2 = Int.(min x1 x2, max x1 x2) in
    let y1, y2 = Int.(min y1 y2, max y1 y2) in

    let ex = List.count g.pad_cols ~f:(fun x -> x > x1 && x < x2) in
    let ey = List.count g.pad_rows ~f:(fun y -> y > y1 && y < y2) in

    let dx = x2 - x1 + (max 1 (expansion - 1)) * ex in
    let dy = y2 - y1 + (max 1 (expansion - 1)) * ey in

    dx + dy

  let of_string s =
    let rec loop acc xs ys x y = function
      | '#' :: s ->
          let acc = (x, y) :: acc in
          let xs = Set.add xs x in
          let ys = Set.add ys y in
          loop acc xs ys (x + 1) y s
      | '.' :: s -> loop acc xs ys (x + 1) y s
      | '\n' :: s -> loop acc xs ys 0 (y + 1) s
      | char :: _ ->
          Printf.invalid_argf "Unexpected character in input '%c'\n" char ()
      | [] -> (acc, xs, ys)
    in

    let width = String.index_exn s '\n' in
    let height = String.count s ~f:(Char.( = ) '\n') in

    let data, xs, ys =
      let xs = Set.empty (module Int) in
      let ys = Set.empty (module Int) in
      let s = String.to_list s in
      loop [] xs ys 0 0 s
    in

    let data = Array.of_list data in

    let pad_rows =
      let all = List.range 0 width |> Set.of_list (module Int) in
      Set.diff all ys |> Set.to_list
    in

    let pad_cols =
      let all = List.range 0 height |> Set.of_list (module Int) in
      Set.diff all xs |> Set.to_list
    in

    { data; pad_rows; pad_cols }
end

let day = 11

let solve ?(expansion = 1) input =
  let g = Grid.of_string input in

  let pairs = Grid.pairs g in

  let dists =
    List.map pairs ~f:(fun (a, b) -> Grid.distance ~expansion g a b)
  in

  List.fold dists ~init:0 ~f:( + ) |> Int.to_string

let part_a input = solve input
let part_b input = solve input ~expansion:1000000

let%test_module "day 11" =
  (module struct
    let input =
      {|...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
|}

    let%test_unit "part a" = [%test_result: string] (part_a input) ~expect:"374"

    let%test_unit "part b" =
      [%test_result: string] (solve input ~expansion:10) ~expect:"1030"

    let%test_unit "part b" =
      [%test_result: string] (solve input ~expansion:100) ~expect:"8410"
  end)
