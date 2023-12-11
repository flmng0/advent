open Util
open Base

module DesertMap = struct
  type t = { dirs : dir list; nodes : node_map }
  and dir = Left | Right
  and node_map = (string * string) Map.M(String).t

  let dir_of_char = function 'L' -> Some Left | 'R' -> Some Right | _ -> None
  let char_of_dir = function Left -> 'L' | Right -> 'R'

  let node_of_string s =
    let name, dirs = String.lsplit2_exn ~on:'=' s in
    let left, right = String.lsplit2_exn dirs ~on:',' in

    let left =
      String.strip left ~drop:(fun c -> Char.(is_whitespace c || c = '('))
    in
    let right =
      String.strip right ~drop:(fun c -> Char.(is_whitespace c || c = ')'))
    in

    (name, (left, right))

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
  let map = DesertMap.of_string input in

  Stdio.print_endline (map.dirs |> List.map ~f:DesertMap.char_of_dir |> String.of_list);

  ""

let part_b _input = ""

let%test_module "day 8" =
  (module struct
    let input = {|RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
|}
    let%test_unit "part a" = [%test_result: string] (part_a input) ~expect:""
    let%test_unit "part b" = [%test_result: string] (part_b input) ~expect:""
  end)
