open Util
open Base

type dish = {
  rocks : (int * int) list;
  cubes : (int * int) list;
  width : int;
  height : int;
}

let show_dish d =
  let tile y x =
    if List.mem d.rocks (x, y) ~equal:IntPair.equal then 'O'
    else if List.mem d.cubes (x, y) ~equal:IntPair.equal then '#'
    else '.'
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

let parse input =
  let lines = lines_of_string input in

  let width = List.hd_exn lines |> String.length in
  let height = List.length lines in

  let rec parse' y rocks cubes = function
    | line :: rest ->
        let rocks, cubes =
          line |> String.to_list
          |> List.foldi ~init:(rocks, cubes) ~f:(fun x (r, c) -> function
               | 'O' -> ((x, y) :: r, c) | '#' -> (r, (x, y) :: c) | _ -> (r, c))
        in
        parse' (y + 1) rocks cubes rest
    | [] -> { rocks; cubes; width; height }
  in

  parse' 0 [] [] lines

let tilt_north d =
  let cubes = Map.of_alist_multi (module Int) d.cubes in

  let rec tilt_north' rocks = function
    | rock :: rest ->
        let rx, ry = rock in

        let cs = Map.find cubes rx |> Option.value ~default:[] in
        let rs = Map.find rocks rx |> Option.value ~default:[] in

        let obstacles = List.append cs rs in

        let new_y =
          List.filter obstacles ~f:(fun y -> y < ry)
          |> List.max_elt ~compare:Int.compare
          |> Option.value_map ~default:0 ~f:(( + ) 1)
        in

        let rocks = Map.add_multi rocks ~key:rx ~data:new_y in

        tilt_north' rocks rest
    | [] -> rocks
  in

  let sorted =
    List.sort d.rocks ~compare:(fun (x1, y1) (x2, y2) ->
        IntPair.compare (y1, x1) (y2, x2))
  in
  let rocks =
    tilt_north' (Map.empty (module Int)) sorted
    |> Map.to_alist
    |> List.concat_map ~f:(fun (x, ys) -> List.map ys ~f:(fun y -> (x, y)))
  in
  { d with rocks }

let day = 14

let part_a input =
  let dish = parse input in
  let tilted = tilt_north dish in

  Stdio.print_endline "Original:";
  Stdio.print_endline (show_dish dish);
  Stdio.print_endline "\nTilted:";
  Stdio.print_endline (show_dish tilted);

  let load =
    tilted.rocks
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
