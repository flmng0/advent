open Util
open Base

module Card = struct
  type t = { id : int; winning : int list; have : int list }

  let of_string s =
    let c, s = String.lsplit2_exn ~on:':' s in
    let w, h = String.lsplit2_exn ~on:'|' s in

    let id =
      c
      |> String.chop_prefix_exn ~prefix:"Card "
      |> String.strip |> Int.of_string
    in

    let winning = read_nums w in
    let have = read_nums h in

    { id; winning; have }

  let count_wins c =
    let win_set = Set.of_list (module Int) c.winning in
    let have_set = Set.of_list (module Int) c.have in

    Set.inter win_set have_set |> Set.length
end

let day = 4

let part_a input =
  let cards = lines_of_string input |> List.map ~f:Card.of_string in

  let points c =
    match Card.count_wins c with 0 -> 0 | n -> Int.shift_left 1 (n - 1)
  in

  let total = List.map cards ~f:points |> List.fold ~init:0 ~f:( + ) in

  Int.to_string total

let part_b input =
  let cards = lines_of_string input |> List.map ~f:Card.of_string in

  let rec loop copies = function
    | card :: rest ->
        let wins = Card.count_wins card in
        if wins = 0 then loop copies rest
        else
          let times = Map.find_exn copies card.id in

          let to_copy = List.take rest wins in

          let copies =
            List.fold to_copy ~init:copies ~f:(fun acc c ->
                Map.update acc
                  Card.(c.id)
                  ~f:(fun n -> Option.value_exn n + times))
          in

          loop copies rest
    | [] ->
        Map.fold copies ~init:0 ~f:(fun ~key ~data acc ->
            ignore key;
            acc + data)
  in

  let copies =
    List.map cards ~f:(fun c -> (c.id, 1)) |> Map.of_alist_exn (module Int)
  in

  let total = loop copies cards in

  Int.to_string total

let%test_module "day 4" =
  (module struct
    let input =
      {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
|}

    let%test_unit "part a" = [%test_result: string] (part_a input) ~expect:"13"
    let%test_unit "part b" = [%test_result: string] (part_b input) ~expect:"30"
  end)
