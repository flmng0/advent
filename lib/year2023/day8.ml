open Util
open Base

module DMap = struct
  type t = { dirs : dir list; nodes : node_map }
  and dir = Left | Right
  and node_map = (string * string) Map.M(String).t

  let dir_of_char = function 'L' -> Some Left | 'R' -> Some Right | _ -> None
  let char_of_dir = function Left -> 'L' | Right -> 'R'

  let node_of_string s =
    let name, dirs = String.lsplit2_exn ~on:'=' s in
    let left, right = String.lsplit2_exn dirs ~on:',' in

    let name = String.strip name in

    let left =
      String.strip left ~drop:(fun c -> Char.(is_whitespace c || c = '('))
    in
    let right =
      String.strip right ~drop:(fun c -> Char.(is_whitespace c || c = ')'))
    in

    (name, (left, right))

  let dir_seq s = Sequence.cycle_list_exn s.dirs

  let of_string s =
    match lines_of_string s with
    | dirs :: _ :: nodes ->
        let dirs = String.to_list dirs |> List.filter_map ~f:dir_of_char in

        let nodes =
          List.map nodes ~f:node_of_string |> Map.of_alist_exn (module String)
        in

        { dirs; nodes }
    | _ -> invalid_arg "empty or invalid input!"
end

let day = 8

let part_a input =
  let map = DMap.of_string input in
  let dirs = DMap.dir_seq map in

  let rec walk steps (node_l, node_r) dirs =
    match Sequence.next dirs with
    | Some (dir, dirs) ->
        let next_node =
          DMap.(match dir with Left -> node_l | Right -> node_r)
        in

        if String.(next_node = "ZZZ") then steps + 1
        else
          let node = DMap.(Map.find_exn map.nodes next_node) in
          walk (steps + 1) node dirs
    | None -> invalid_arg "ran out of dirs in an infinite sequence??"
  in

  let node = Map.find_exn map.nodes "AAA" in
  let steps = walk 0 node dirs in

  Int.to_string steps

let part_b input =
  let map = DMap.of_string input in
  let dirs = DMap.dir_seq map in

  let rec walk steps nodes dirs =
    match Sequence.next dirs with
    | Some (dir, dirs) ->
        let node_dir = DMap.(match dir with Left -> fst | Right -> snd) in
        let next_nodes = List.map ~f:node_dir nodes in

        if List.for_all next_nodes ~f:(String.is_suffix ~suffix:"Z") then steps + 1
        else
          let nodes = List.map ~f:(Map.find_exn map.nodes) next_nodes in
          walk (steps + 1) nodes dirs
    | None -> invalid_arg "ran out of dirs in an infinite sequence??"
  in

  let nodes =
    Map.to_alist map.nodes
    |> List.filter_map ~f:(fun (name, node) ->
           if String.is_suffix ~suffix:"A" name then Some node else None)
  in

  let steps = walk 0 nodes dirs in

  Int.to_string steps

let%test_module "day 8" =
  (module struct
    let input =
      {|RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
|}

    let%test_unit "part a" = [%test_result: string] (part_a input) ~expect:"2"

    let%test_unit "part a alt" =
      [%test_result: string]
        (part_a {|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
|})
        ~expect:"6"

    let%test_unit "part b" =
      [%test_result: string]
        (part_b
           {|LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
|})
        ~expect:"6"
  end)
