open Util
open Base

let day = 6

let solve rounds =
  let first_win t d =
    Sequence.(init (t / 2) ~f:Fn.id |> find_exn ~f:(fun i -> i * (t - i) > d))
  in

  let all_ways =
    rounds
    |> List.map ~f:(fun (t, d) ->
           let first = first_win t d in
           let result = t + 1 - (first * 2) in
           result)
  in

  let product = List.fold ~init:1 ~f:( * ) all_ways in

  product

let part_a input =
  let lines = lines_of_string input in
  let rounds =
    match lines with
    | [ times; distances ] ->
        let times =
          times |> String.chop_prefix_exn ~prefix:"Time:" |> read_nums
        in
        let distances =
          distances |> String.chop_prefix_exn ~prefix:"Distance:" |> read_nums
        in

        List.zip_exn times distances
    | _ -> invalid_arg "expected 2 lines"
  in

  Int.to_string (solve rounds)

let part_b input =
  let lines = lines_of_string input in
  let rounds =
    match lines with
    | [ times; distances ] ->
        let time =
          times
          |> String.chop_prefix_exn ~prefix:"Time:"
          |> String.filter ~f:(fun c -> not (Char.is_whitespace c))
          |> Int.of_string
        in
        let distance =
          distances
          |> String.chop_prefix_exn ~prefix:"Distance:"
          |> String.filter ~f:(fun c -> not (Char.is_whitespace c))
          |> Int.of_string
        in
        [(time, distance)]
    | _ -> invalid_arg "expected 2 lines"
  in

  Int.to_string (solve rounds)

let%test_module "day 6" =
  (module struct
    let input = {|Time:      7  15   30
Distance:  9  40  200
|}

    let%test_unit "part a" = [%test_result: string] (part_a input) ~expect:"288"
    let%test_unit "part b" = [%test_result: string] (part_b input) ~expect:"71503"
  end)
