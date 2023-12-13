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

  let get p coord =
    idx_opt p coord |> Option.map ~f:(fun i -> Array.unsafe_get p.tiles i)

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
          else if IntPair.compare start next = 0 then Some path
          else walk (pos :: path) next pos
      | None -> None
    in

    cardinal
    |> List.find_map ~f:(fun (dx, dy) ->
           let sx, sy = start in
           walk [ start ] (sx + dx, sy + dy) start)
    |> Option.map ~f:(List.cons start)

  let diff a b =
    let x1, y1 = a in
    let x2, y2 = b in
    (x2 - x1, y2 - y1)

  let get_inside p loop =
    let top, bottom, left, right =
      List.fold loop ~init:(p.width, -1, p.height, -1)
        ~f:(fun (t, b, l, r) (x, y) -> (min t y, max b y, min l x, max r x))
    in
    let iw, ih = (bottom - top, right - left) in

    let inner_norm =
      let a, b =
        match loop with
        | a :: b :: _ -> (a, b)
        | _ -> invalid_arg "Loop requires 2 or more points"
      in
      let (x1, y1), (x2, y2) = (a, b) in

      let f =
        match (compare x1 x2, compare y1 y2) with
        | 0, 0 -> invalid_arg "2 consecutive points are equal?"
        | 0, dy ->
            if dy > 0 then fun (x, y) -> x > x1 && y = y1
            else fun (x, y) -> x < x1 && y = y1
        | dx, 0 ->
            if dx > 0 then fun (x, y) -> y < y1 && x = x1
            else fun (x, y) -> y > y1 && x = x1
        | _ -> 
          let msg = Printf.sprintf "2 consecutive points don't have an equal? Got points: %i %i, %i %i" x1 y1 x2 y2 in
          invalid_arg msg
      in

      let intersections = List.count loop ~f in

      let open Int in
      if rem intersections 2 = 0 then fun (x, y) ->
        let x', y' = (y, -x) in
        Sign.(to_int (sign x'), to_int (sign y'))
      else fun (x, y) ->
        let x', y' = (-y, x) in
        Sign.(to_int (sign x'), to_int (sign y'))
    in

    let flood loop_set seed =
      let neighs (x1, y1) =
        List.map cardinal ~f:(fun (x2, y2) -> (x1 + x2, y1 + y2))
      in
      let inside coord = not (Set.mem loop_set coord) in

      let q = Queue.create ~capacity:(iw * ih) () in

      let rec flood' found =
        match Queue.dequeue q with
        | Some n ->
            if inside n then (
              Queue.enqueue_all q (neighs n);
              let found = Set.add found n in
              flood' found)
            else flood' found
        | None -> found
      in

      let found = Set.add (Set.empty (module IntPair)) seed in
      flood' found
    in

    let rec get_inside' acc prev loop =
      match loop with
      | curr :: loop ->
          let diff = diff prev curr in
          let dx, dy = inner_norm diff in

          let x, y = curr in
          let adj = (x + dx, y + dy) in

          if List.mem loop adj ~equal:IntPair.equal || Set.mem acc adj then
            get_inside' acc curr loop
          else
            let loop_set = Set.of_list (module IntPair) loop in
            let found = flood loop_set adj in
            let acc = Set.union found acc in
            get_inside' acc curr loop
      | [] -> acc
    in

    get_inside'
      (Set.empty (module IntPair))
      (List.hd_exn loop) (List.tl_exn loop)
    |> Set.to_list

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

let part_b input =
  let p = Pipes.of_string input in
  let loop = Pipes.get_loop p |> Option.value_exn in

  List.iter ~f:(fun (x, y) -> Stdio.printf "%i %i\n" x y) loop;

  let enclosed = Pipes.get_inside p loop in

  List.length enclosed |> Int.to_string

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

    let _input_b1 = {|...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
|}
    let input_b2 = {|.F----7F7F7F7F-7....
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
    let _input_b3 = {|FF7FSF7F7F7F7F7F---7
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
    (* 
    let%test_unit "part b 1" = [%test_result: string] (part_b input_b1) ~expect:"4"
    let%test_unit "part b 3" = [%test_result: string] (part_b input_b3) ~expect:"10"
*)
    let%test_unit "part b 2" = [%test_result: string] (part_b input_b2) ~expect:"8"

  end)
