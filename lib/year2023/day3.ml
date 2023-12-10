open Util
open Base

module Board = struct
  type t = { symbols : sym list; numbers : num list } [@@deriving show]
  and sym = { sx : int; sy : int; sval : char } [@@deriving show]

  and num = { nx : int; ny : int; nlen : int; nval : int }
  [@@deriving show, compare, sexp_of]

  module Num = struct
    module T = struct
      type t = num [@@deriving show, compare, sexp_of]
    end

    include T
    include Comparator.Make (T)
  end

  let nums_touching x y b =
    let is_touching { nx; ny; nlen; _ } = x >= nx && x < nx + nlen && y = ny in
    List.filter ~f:is_touching b.numbers

  let of_string s =
    let lines = lines_of_string s in

    let num_of_chars x y chars =
      let nstr = String.of_list (List.rev chars) in
      let nval = Int.of_string nstr in
      let nlen = String.length nstr in
      let nx = x - nlen in
      let ny = y in

      { nval; nlen; nx; ny }
    in

    let parse_line y acc line =
      let rec loop syms nums x num_chars = function
        | c :: rest -> (
            match c with
            | '.' -> (
                match num_chars with
                | [] -> loop syms nums (x + 1) [] rest
                | chars ->
                    let num = num_of_chars x y chars in
                    loop syms (num :: nums) (x + 1) [] rest)
            | '0' .. '9' as d -> loop syms nums (x + 1) (d :: num_chars) rest
            | sval ->
                let sym = { sx = x; sy = y; sval } in
                loop (sym :: syms) nums (x + 1) num_chars rest)
        | [] ->
            let nums =
              match num_chars with
              | [] -> nums
              | chars ->
                  let num = num_of_chars x y chars in
                  num :: nums
            in

            (syms, nums)
      in
      let syms, nums = acc in
      let chars = String.to_list line in
      loop syms nums 0 [] chars
    in

    let symbols, numbers = lines |> List.foldi ~init:([], []) ~f:parse_line in

    { symbols; numbers }
end

let day = 3

let part_a input =
  let board = Board.of_string input in

  let neighs =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
  in

  let parts =
    let open Board in
    List.concat_map board.symbols ~f:(fun { sx; sy; _ } ->
        List.concat_map neighs ~f:(fun (dx, dy) ->
            nums_touching (sx + dx) (sy + dy) board))
    |> Set.stable_dedup_list (module Num)
  in

  Stdio.printf "Parts found: %s\n"
    (List.map parts ~f:Board.show_num |> String.concat ~sep:", ");

  let total = List.fold ~init:0 ~f:(fun acc num -> acc + num.nval) parts in

  Int.to_string total

let part_b input =
  let board = Board.of_string input in

  let neighs =
    [ (-1, -1); (0, -1); (1, -1); (-1, 0); (1, 0); (-1, 1); (0, 1); (1, 1) ]
  in

  let ratios =
    let open Board in
    board.symbols
    |> List.filter ~f:(fun { sval; _ } -> Char.(sval = '*'))
    |> List.fold ~init:[] ~f:(fun ratios { sx; sy; _ } ->
           let neigh_nums =
             List.concat_map
               ~f:(fun (dx, dy) -> nums_touching (sx + dx) (sy + dy) board)
               neighs
             |> Set.of_list (module Num)
           in

           if Set.length neigh_nums = 2 then
             let ratio =
               Set.fold ~init:1 ~f:(fun acc num -> acc * num.nval) neigh_nums
             in
             ratio :: ratios
           else ratios)
  in

  let total = List.fold ~init:0 ~f:( + ) ratios in

  Int.to_string total

let%test_module "day 3" =
  (module struct
    let input =
      {|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
|}

    let%test_unit "part a" =
      [%test_result: string] (part_a input) ~expect:"4361"

    let%test_unit "part b" =
      [%test_result: string] (part_b input) ~expect:"467835"
  end)
