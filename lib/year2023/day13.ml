open Util
open Base

let day = 13

type t = { grid : kind array; width : int; height : int }
and kind = Ash | Rock

let kind_equal a b =
  match (a, b) with Ash, Ash -> true | Rock, Rock -> true | _ -> false

let kind_of_char = function
  | '.' -> Ash
  | '#' -> Rock
  | c ->
      Printf.invalid_argf
        "Invalid character for kind, expected '.' or '#', got: %c" c ()

let parse input =
  let lines = lines_of_string input in

  let width = List.hd_exn lines |> String.length in
  let height = List.length lines in

  let parse_line line = line |> String.to_list |> List.map ~f:kind_of_char in

  let grid = lines |> List.concat_map ~f:parse_line |> Array.of_list in

  { grid; width; height }

let count_diffs a b =
  let v line =
    line
    |> List.map ~f:(function Ash -> 0 | Rock -> 1)
    |> List.fold ~init:(0, 0) ~f:(fun (acc, n) x ->
           ((acc + Int.(x lsl n)), n + 1))
  in
  let a_val, _ = v a in
  let b_val, _ = v b in

  Int.(bit_xor a_val b_val |> popcount)

let incidence ?(smudged = false) ~dir t =
  let { grid; width; _ } = t in

  let with_coords =
    Array.to_sequence grid
    |> Sequence.mapi ~f:(fun i k ->
           let x = Int.rem i width in
           let y = i / width in

           (x, y, k))
  in

  let lines =
    with_coords
    |> Sequence.map ~f:(fun (x, y, k) ->
           match dir with `hor -> (x, k) | `ver -> (y, k))
    |> Sequence.to_list
    |> Map.of_alist_multi (module Int)
  in

  let rec aux prev = function
    | [ _ ] -> 0
    | a :: rest ->
        let prev = a :: prev in
        let prev_len = List.length prev in

        let length = min prev_len (List.length rest) in

        let left = List.take prev length in
        let right = List.take rest length in

        let equal a b =
          let diffs = count_diffs a b in
          diffs = 0
        in
        let reflected =
          if not smudged then List.equal equal left right
          else
            match List.zip left right with
            | Ok zipped ->
                zipped
                |> List.fold ~init:0 ~f:(fun acc (a, b) ->
                       acc + count_diffs a b)
                = 1
            | Unequal_lengths ->
                failwith "Unequal lengths for left and right in reflected"
        in
        if reflected then prev_len else aux prev rest
    | _ -> 0
  in
  aux [] (lines |> Map.data)

let summarize ?(smudged = false) t = 
  let i = incidence ~smudged in
  i t ~dir:`hor + (i t ~dir:`ver * 100)

let part_a input =
  let fields = chunks_of_string input |> List.map ~f:parse in

  fields |> List.map ~f:summarize |> List.fold ~init:0 ~f:( + ) |> Int.to_string

let part_b input = 
  let fields = chunks_of_string input |> List.map ~f:parse in

  fields |> List.map ~f:(summarize ~smudged:true) |> List.fold ~init:0 ~f:( + ) |> Int.to_string

let%test_module "day 13" =
  (module struct
    let input =
      {|#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
|}

    let%test_unit "part a" = [%test_result: string] (part_a input) ~expect:"405"
    let%test_unit "part b" = [%test_result: string] (part_b input) ~expect:"400"
  end)

let%test_module "day 15 incidence" =
  (module struct
    let chunk_a =
      parse
        {|#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.|}

    let chunk_b =
      parse
        {|#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#|}

    let%test_unit "horizontal" =
      [%test_result: int] (incidence ~dir:`hor chunk_a) ~expect:5

    let%test_unit "horizontal no ver" =
      [%test_result: int] (incidence ~dir:`ver chunk_a) ~expect:0

    let%test_unit "vertical" =
      [%test_result: int] (incidence ~dir:`ver chunk_b) ~expect:4

    let%test_unit "vertical no hor" =
      [%test_result: int] (incidence ~dir:`hor chunk_b) ~expect:0
  end)
