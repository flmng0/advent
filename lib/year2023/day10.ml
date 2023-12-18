open Util
open Base

module Pipes = struct
  type t = { width : int; height : int; tiles : tile array }
  and tile = V | H | NE | NW | SE | SW | Ground | Start

  let tile_of_char = function
    | '|' -> V
    | '-' -> H
    | 'L' -> NE
    | 'J' -> NW
    | '7' -> SW
    | 'F' -> SE
    | '.' -> Ground
    | 'S' -> Start
    | c -> invalid_arg (Printf.sprintf "Unexpected tile character: %c" c)

  let char_of_tile = function
    | V -> '|'
    | H -> '-'
    | NE -> 'L'
    | NW -> 'J'
    | SW -> '7'
    | SE -> 'F'
    | Ground -> '.'
    | Start -> 'S'

  let coord p idx = (Int.rem idx p.width, idx / p.width)

  let coord_opt p idx =
    if idx < 0 || idx >= Array.length p.tiles then None else Some (coord p idx)

  let idx p (x, y) = x + (y * p.width)

  let idx_opt p (x, y) =
    if x < 0 || x >= p.width || y < 0 || y >= p.height then None
    else Some (idx p (x, y))

  let get p coord =
    idx_opt p coord |> Option.map ~f:(fun i -> Array.unsafe_get p.tiles i)

  let travel p from through =
    let x1, y1 = from in
    let x2, y2 = through in

    let dx, dy = (x2 - x1, y2 - y1) in

    get p through
    |> Option.find_map ~f:(function
         | V -> Some (x2, y2 + dy)
         | H -> Some (x2 + dx, y2)
         | NE ->
             if dx = -1 then Some (x2, y2 - 1)
             else if dy = 1 then Some (x2 + 1, y2)
             else None
         | NW ->
             if dx = 1 then Some (x2, y2 - 1)
             else if dy = 1 then Some (x2 - 1, y2)
             else None
         | SE ->
             if dx = -1 then Some (x2, y2 + 1)
             else if dy = -1 then Some (x2 + 1, y2)
             else None
         | SW ->
             if dx = 1 then Some (x2, y2 + 1)
             else if dy = -1 then Some (x2 - 1, y2)
             else None
         | Ground -> None
         | Start -> None)

  let cardinal = [ (0, -1); (1, 0); (0, 1); (-1, 0) ]

  let get_loop p =
    let start =
      Array.findi_exn p.tiles ~f:(fun _i -> function
        | Start -> true | _ -> false)
      |> fst |> coord p
    in

    let rec walk path pos prev =
      match travel p prev pos with
      | Some next ->
          if idx_opt p next |> Option.is_none then None
          else if IntPair.compare start next = 0 then Some (pos :: path)
          else walk (pos :: path) next pos
      | None -> None
    in

    cardinal
    |> List.find_map ~f:(fun (dx, dy) ->
           let sx, sy = start in
           walk [ start ] (sx + dx, sy + dy) start)

  let get_inside p loop =
    let top, bottom, left, right =
      List.fold loop ~init:(p.width, -1, p.height, -1)
        ~f:(fun (t, b, l, r) (x, y) -> (min t y, max b y, min l x, max r x))
    in

    let clockwise =
      let a, b =
        match loop with
        | a :: b :: _ -> (a, b)
        | _ -> invalid_arg "Loop requires 2 or more points"
      in
      let (x1, y1), (x2, y2) = (a, b) in

      let intersections =
        let start, stop, f =
          match (compare x1 x2, compare y1 y2) with
          | 0, 0 -> invalid_arg "2 consecutive points are equal?"
          | 0, dy ->
              let start, stop = if dy > 0 then (left, x1) else (x1, right) in
              let f x =
                let coord = (x, y1) in
                match p.tiles.(idx p coord) with H -> false | _ -> true
              in
              (start, stop, f)
          | dx, 0 ->
              let start, stop = if dx > 0 then (y1, bottom) else (top, y1) in
              let f y =
                let coord = (x1, y) in
                match p.tiles.(idx p coord) with V -> false | _ -> true
              in
              (start, stop, f)
          | _ ->
              Printf.invalid_argf
                "2 consecutive points don't have an equal? Got points: %i %i, \
                 %i %i"
                x1 y1 x2 y2 ()
        in

        Sequence.range start stop |> Sequence.count ~f
      in

      Int.rem intersections 2 = 1
    in

    let up (x, y) =
      match idx_opt p (x, y) with
      | None ->
          Stdio.printf "No idx for coord (%i %i)\n" x y;
          None
      | Some i -> (
          match p.tiles.(i) with
          | NE | NW -> Some true
          | SE | SW -> Some false
          | _ -> None)
    in

    let count_line acc (y, xs) =
      let xs = List.sort xs ~compare:Int.compare in

      let rec count_line' acc inside upwards = function
        | x1 :: x2 :: xs ->
            let dx = Int.abs (x2 - x1) in
            let next_inside, upwards =
              match up (x1, y) with
              | Some u ->
                  if Bool.(u = upwards) then (inside, u) else (not inside, u)
              | None -> (not inside, not upwards)
            in

            let acc = if inside then acc + dx - 1 else acc in

            count_line' acc next_inside upwards (x2 :: xs)
        | _ -> acc
      in

      let is_up = if clockwise then true else false in
      count_line' acc true is_up xs
    in

    let lines = Map.of_alist_multi (module Int) loop in
    Map.to_sequence lines |> Sequence.fold ~init:0 ~f:count_line

  let of_string s =
    let lines = lines_of_string s in
    let width = String.length (List.hd_exn lines |> String.strip) in
    let height = List.length lines in

    let tiles =
      List.concat_map lines ~f:(fun line ->
          String.to_list line |> List.map ~f:tile_of_char)
      |> Array.of_list
    in

    { width; height; tiles }

  let to_string (p : t) =
    let row y =
      List.range ~stop:`exclusive 0 p.width
      |> List.map ~f:(fun x -> p.tiles.(x + (y * p.width)) |> char_of_tile)
    in
    let rows =
      List.range ~stop:`exclusive 0 p.height
      |> List.map ~f:(fun y -> row y |> String.of_list)
    in
    String.concat_lines rows
end

let day = 10

let part_a input =
  let p = Pipes.of_string input in
  let loop = Pipes.get_loop p |> Option.value_exn in

  List.length loop / 2 |> Int.to_string

let part_b input =
  let p = Pipes.of_string input in
  let loop = Pipes.get_loop p |> Option.value_exn in

  let enclosed = Pipes.get_inside p loop in

  Int.to_string enclosed

let%test_module "day 10" =
  (module struct
    let input = {|..F7.
.FJ|.
SJ.L7
|F--J
LJ...
|}

    let input_b1 =
      {|...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
|}

    let%test_unit "pipe parsing" =
      [%test_result: string]
        (Pipes.of_string input |> Pipes.to_string)
        ~expect:input

    let%test_unit "part a" = [%test_result: string] (part_a input) ~expect:"8"

    let%test_unit "part a 1" =
      [%test_result: string] (part_a input_b1) ~expect:"23"

    let%test_unit "part b 1" =
      [%test_result: string] (part_b input_b1) ~expect:"4"

    let input_b2 =
      {|.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
|}

    let%test_unit "part b 2" =
      [%test_result: string] (part_b input_b2) ~expect:"8"

    let input_b3 =
      {|FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
|}

    let%test_unit "part b 3" =
      [%test_result: string] (part_b input_b3) ~expect:"10"
  end)
