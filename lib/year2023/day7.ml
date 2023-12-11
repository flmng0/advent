open Util
open Base

module Hand = struct
  type t = { hand : char list; bidding : int }

  type ranking =
    | FiveOAK
    | FourOAK
    | FullHouse
    | ThreeOAK
    | TwoPair
    | OnePair
    | HighCard
  [@@deriving show]

  let card_score ?(joker = false) = function
    | 'A' -> 12
    | 'K' -> 11
    | 'Q' -> 10
    | 'J' -> if joker then -1 else 9
    | 'T' -> 8
    | '9' -> 7
    | '8' -> 6
    | '7' -> 5
    | '6' -> 4
    | '5' -> 3
    | '4' -> 2
    | '3' -> 1
    | '2' -> 0
    | _ -> -1

  let hit_count ?(joker = false) { hand; _ } =
    let incr_key n =
      Map.update ~f:(function Some count -> count + n | None -> 1)
    in
    let hits = List.fold hand ~init:(Map.empty (module Char)) ~f:(incr_key 1) in

    if joker then
      match Map.find hits 'J' with
      | Some n when n < 5 ->
          let hits = Map.remove hits 'J' in
          let highest_key, _ =
            Map.fold hits ~init:('Z', 0) ~f:(fun ~key ~data acc ->
                let card, max = acc in
                match Int.compare max data with
                | 0 ->
                    let a = card_score ~joker card in
                    let b = card_score ~joker key in

                    if Int.compare a b > 0 then (card, max) else (key, data)
                | n -> if n > 0 then (card, max) else (key, data))
          in

          incr_key n hits highest_key
      | _ -> hits
    else hits

  let get_ranking ?(joker = false) h =
    let hits =
      hit_count ~joker h |> Map.data
      |> List.sort ~compare:(fun a b -> neg (Int.compare a b))
    in

    match hits with
    | 5 :: _ -> FiveOAK
    | 4 :: _ -> FourOAK
    | 3 :: 2 :: _ -> FullHouse
    | 3 :: _ -> ThreeOAK
    | 2 :: 2 :: _ -> TwoPair
    | 2 :: _ -> OnePair
    | 1 :: _ -> HighCard
    | _ -> invalid_arg "Unknown card combination"

  let get_rank_score = function
    | FiveOAK -> 6
    | FourOAK -> 5
    | FullHouse -> 4
    | ThreeOAK -> 3
    | TwoPair -> 2
    | OnePair -> 1
    | HighCard -> 0

  let rank_score_of_hand ?(joker = false) h =
    get_ranking ~joker h |> get_rank_score

  let of_string line =
    let hand, bidding = String.lsplit2_exn ~on:' ' line in

    let hand = String.to_list hand in
    let bidding = Int.of_string (String.strip bidding) in

    { hand; bidding }

  let scores ?(joker = false) h =
    rank_score_of_hand ~joker h :: List.map ~f:(card_score ~joker) h.hand

  let compare ?(joker = false) a b =
    List.compare Int.compare (scores ~joker a) (scores ~joker b)
end

let day = 7

let part_a input =
  let lines = lines_of_string input in
  let hands =
    List.map ~f:Hand.of_string lines |> List.sort ~compare:Hand.compare
  in

  let total =
    List.foldi ~init:0
      ~f:(fun i acc h -> acc + ((i + 1) * Hand.(h.bidding)))
      hands
  in

  Int.to_string total

let part_b input =
  let lines = lines_of_string input in
  let hands =
    List.map ~f:Hand.of_string lines
    |> List.sort ~compare:(Hand.compare ~joker:true)
  in

  let total =
    List.foldi ~init:0
      ~f:(fun i acc h -> acc + ((i + 1) * Hand.(h.bidding)))
      hands
  in

  Int.to_string total

let%test_module "day 7" =
  (module struct
    let input = {|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
|}

    let%test_unit "part a" =
      [%test_result: string] (part_a input) ~expect:"6440"

    let%test_unit "part b" =
      [%test_result: string] (part_b input) ~expect:"5905"
  end)
