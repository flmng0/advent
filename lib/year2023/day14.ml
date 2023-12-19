open Util
open Base

type dish = { data : tile array; width : int; height : int }
and tile = Empty | Rock | Cube

let show_dish d =
  let tile y x =
    let i = x + (y * d.width) in
    match d.data.(i) with Empty -> '.' | Rock -> 'O' | Cube -> '#'
  in
  let line y =
    Sequence.range 0 d.width
    |> Sequence.map ~f:(tile y)
    |> Sequence.to_list |> String.of_list
  in
  let lines =
    Sequence.range 0 d.height |> Sequence.map ~f:line |> Sequence.to_list
  in
  lines |> String.concat_lines

let tile_of_char = function
  | '.' -> Empty
  | 'O' -> Rock
  | '#' -> Cube
  | c -> Printf.invalid_argf "Unexpected character in input: %c" c ()

let parse input =
  let lines = lines_of_string input in

  let width = List.hd_exn lines |> String.length in
  let height = List.length lines in

  let parse_line line = line |> String.to_list |> List.map ~f:tile_of_char in

  let data = lines |> List.concat_map ~f:parse_line |> Array.of_list in

  { data; width; height }

let rocks dish =
  Array.to_list dish.data
  |> List.filter_mapi ~f:(fun i t ->
         let x = i % dish.width in
         let y = i / dish.width in

         match t with Rock -> Some (x, y) | _ -> None)

let tilt dish =
  let { data; width; height } = dish in
  let data = Array.copy data in
  let xrange = List.range 0 width in
  let yrange = List.range 0 height in

  let idx x y = x + (y * dish.width) in

  let tilt' ~dir () =
    let scan, select, idx =
      match dir with
      | `n -> (xrange, yrange, idx)
      | `w -> (yrange, xrange, Fn.flip idx)
      | `s -> (List.rev xrange, yrange, idx)
      | `e -> (List.rev yrange, xrange, Fn.flip idx)
    in

    let scanline i =
      List.fold select ~init:(List.hd_exn select) ~f:(fun min j ->
          match data.(idx i j) with
          | Empty -> min
          | Rock ->
              data.(idx i j) <- Empty;
              data.(idx i min) <- Rock;
              min + 1
          | Cube -> j + 1)
      |> ignore
    in

    List.iter scan ~f:scanline;

    { dish with data }
  in
  tilt'

let tilt_north d = tilt ~dir:`n d ()
let day = 14

let part_a input =
  let dish = parse input in
  let tilted = tilt_north dish in

  Stdio.print_endline "Original:";
  Stdio.print_endline (show_dish dish);
  Stdio.print_endline "\nTilted:";
  Stdio.print_endline (show_dish tilted);

  let load =
    rocks tilted
    |> List.fold ~init:0 ~f:(fun acc (_x, y) -> acc + dish.height - y)
  in

  Int.to_string load

let part_b _input = ""

let%test_module "day 14" =
  (module struct
    let input =
      {|O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
|}

    let%test_unit "part a" = [%test_result: string] (part_a input) ~expect:"136"
    let%test_unit "part b" = [%test_result: string] (part_b input) ~expect:""
  end)
