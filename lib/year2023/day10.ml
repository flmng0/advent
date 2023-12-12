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

  let travel p from through =
    let x1, y1 = from in
    let x2, y2 = through in

    let dx, dy = (x2 - x1, y2 - y1) in

    idx_opt p through
    |> Option.map ~f:(Array.get p.tiles)
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
          else if IntPair.compare start next = 0 then Some path
          else walk (pos :: path) next pos
      | None -> None
    in

    let cardinal = [ (0, -1); (1, 0); (0, 1); (-1, 0) ] in

    cardinal
    |> List.find_map ~f:(fun (dx, dy) ->
           let sx, sy = start in
           walk [ start ] (sx + dx, sy + dy) start)
    |> Option.map ~f:(List.cons start)

  let of_string s =
    let lines = lines_of_string s in
    let width = String.length (List.hd_exn lines) in
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
  let loop = Pipes.get_loop p in

  List.length (Option.value_exn loop) / 2 |> Int.to_string

let part_b _input = ""

let%test_module "day 10" =
  (module struct
    let input = {|..F7.
.FJ|.
SJ.L7
|F--J
LJ...
|}

    let%test_unit "pipe parsing" =
      [%test_result: string]
        (Pipes.of_string input |> Pipes.to_string)
        ~expect:input

    let%test_unit "part a" = [%test_result: string] (part_a input) ~expect:"8"
    let%test_unit "part b" = [%test_result: string] (part_b input) ~expect:""
  end)
