open Util
open Base

module Game = struct
  type t = { id : int; rounds : round list }
  and round = { r : int; g : int; b : int }

  let prefix_len = String.length "Game "

  let valid g =
    let round_valid { r; g; b } = r <= 12 && g <= 13 && b <= 14 in
    List.for_all ~f:round_valid g.rounds

  let power g =
    let mins =
      List.fold ~init:{ r = 0; g = 0; b = 0 }
        ~f:(fun acc r ->
          {
            r = Int.max acc.r r.r;
            g = Int.max acc.g r.g;
            b = Int.max acc.b r.b;
          })
        g.rounds
    in

    mins.r * mins.g * mins.b

  let of_string s =
    let head, tail = String.lsplit2_exn ~on:':' s in
    let id_len = String.length head - prefix_len in
    let id = String.sub ~pos:prefix_len ~len:id_len head |> Int.of_string in

    let rounds =
      String.split ~on:';' tail
      |> List.map ~f:(fun r ->
             let r = String.strip r in

             String.split ~on:',' r
             |> List.fold ~init:{ r = 0; g = 0; b = 0 } ~f:(fun acc x ->
                    match String.strip x |> String.lsplit2_exn ~on:' ' with
                    | v, "red" ->
                        let n = Int.of_string v in
                        { acc with r = n }
                    | v, "green" ->
                        let n = Int.of_string v in
                        { acc with g = n }
                    | v, "blue" ->
                        let n = Int.of_string v in
                        { acc with b = n }
                    | h, t ->
                        invalid_arg
                          (Printf.sprintf "unexpected round string: %s %s" h t)))
    in

    { id; rounds }
end

let day = 2

let part_a input =
  let lines = lines_of_string input in
  let games = List.map ~f:Game.of_string lines in

  let total_valid =
    games
    |> List.filter_map ~f:(fun g -> if Game.valid g then Some g.id else None)
    |> List.fold ~init:0 ~f:( + )
  in

  Int.to_string total_valid

let part_b input =
  let lines = lines_of_string input in
  let games = List.map ~f:Game.of_string lines in

  let powers = List.map ~f:Game.power games in

  let total = List.fold ~init:0 ~f:( + ) powers in

  Int.to_string total

let%test_module "day 2" =
  (module struct
    let input =
      {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
|}

    let%test_unit "part a" = [%test_result: string] (part_a input) ~expect:"8"

    let%test_unit "part b" =
      [%test_result: string] (part_b input) ~expect:"2286"
  end)
