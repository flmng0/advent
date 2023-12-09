open Util
open Base

let day = 1

let part_a input =
  let lines = lines_of_string input in

  let parse_line l =
    let filter_nums c =
      match c with '0' .. '9' -> Some (Char.to_int c - 48) | _ -> None
    in

    let chars = l |> String.to_list |> List.filter_map ~f:filter_nums in

    let first = List.hd_exn chars in
    let last = List.hd_exn (List.rev chars) in

    (first * 10) + last
  in

  let total =
    lines |> Sequence.of_list |> Sequence.map ~f:parse_line
    |> Sequence.fold ~init:0 ~f:( + )
  in

  Int.to_string total

let part_b input =
  let lines = lines_of_string input in

  let words =
    [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
  in

  let solve_line l =
    let filter_nums i c =
      match c with '0' .. '9' -> Some (i, Char.to_int c - 48) | _ -> None
    in

    let digits = l |> String.to_list |> List.filter_mapi ~f:filter_nums in

    let first = List.hd digits in
    let last = List.last digits in

    let _, first =
      words
      |> List.foldi ~init:first ~f:(fun n acc w ->
             let n = n + 1 in

             String.substr_index ~pattern:w l
             |> Option.map ~f:(fun i -> (i, n))
             |> Option.merge
                  ~f:(fun (i, n) (j, m) -> if i < j then (i, n) else (j, m))
                  acc)
      |> Option.value_exn
    in

    let rl = String.rev l in
    let len = String.length l in

    let _, last =
      words
      |> List.foldi ~init:last ~f:(fun n acc w ->
             let n = n + 1 in

             let rw = String.rev w in

             String.substr_index ~pattern:rw rl
             |> Option.map ~f:(fun i -> (len - i, n))
             |> Option.merge
                  ~f:(fun (i, n) (j, m) -> if i >= j then (i, n) else (j, m))
                  acc)
      |> Option.value_exn
    in

    (first * 10) + last
  in

  let total =
    lines |> Sequence.of_list |> Sequence.map ~f:solve_line
    |> Sequence.fold ~init:0 ~f:( + )
  in

  Int.to_string total

let%test_module "day 5" =
  (module struct
    let input_a = {|1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet
|}

    let%test_unit "part a" =
      [%test_result: string] (part_a input_a) ~expect:"142"

    let input_b =
      {|two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen
|}

    let%test_unit "part b" =
      [%test_result: string] (part_b input_b) ~expect:"281"
  end)
