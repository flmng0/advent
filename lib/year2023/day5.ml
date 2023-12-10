open Util
open Base

module Mapping = struct
  type t = { routes : route list } [@@deriving show]

  and route = { src_start : int; dst_start : int; length : int }
  [@@deriving show]

  let route_contains s r = s >= r.src_start && s < r.src_start + r.length

  let route_of_string s =
    match read_nums s with
    | [ ss; ds; l ] -> Some { src_start = ss; dst_start = ds; length = l }
    | _ -> None

  let of_string s =
    let route_strings = s |> String.split ~on:'\n' |> List.tl_exn in
    let routes = route_strings |> List.filter_map ~f:route_of_string in

    { routes }

  let transform seed m =
    match List.find ~f:(route_contains seed) m.routes with
    | Some r -> seed - r.src_start + r.dst_start
    | None -> seed
end

let seeds_of_string s =
  s |> String.chop_prefix_exn ~prefix:"seeds: " |> read_nums

let day = 5

let part_a input =
  let chunks = chunks_of_string input in

  let seeds = List.hd_exn chunks |> seeds_of_string in
  let mappings = List.tl_exn chunks |> List.map ~f:Mapping.of_string in

  let locations =
    List.map seeds ~f:(fun s -> List.fold ~init:s ~f:Mapping.transform mappings)
  in

  Stdio.printf "Seeds: %s\n" (List.map ~f:Int.to_string seeds |> String.concat ~sep:", ");
  Stdio.printf "Locations: %s\n" (List.map ~f:Int.to_string locations |> String.concat ~sep:", ");

  let min = List.min_elt locations ~compare:Int.compare |> Option.value_exn in

  Int.to_string min

let part_b _input = ""

let%test_module "day 5" =
  (module struct
    let input =
      {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|}

    let%test_unit "part a" = [%test_result: string] (part_a input) ~expect:"35"
    let%test_unit "part b" = [%test_result: string] (part_b input) ~expect:""
  end)
