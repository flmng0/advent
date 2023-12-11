open Util
open Base

module Mapping = struct
  type t = { routes : route list } [@@deriving show]

  and route = { dst_start : int; src_start : int; length : int }
  [@@deriving show]

  let route_range r = (r.src_start, r.src_start + r.length)
  let route_contains s r = s >= r.src_start && s < r.src_start + r.length

  let route_of_string s =
    match read_nums s with
    | [ ds; ss; l ] -> Some { dst_start = ds; src_start = ss; length = l }
    | _ -> None

  let of_string s =
    let route_strings = s |> String.split ~on:'\n' |> List.tl_exn in
    let routes = route_strings |> List.filter_map ~f:route_of_string in

    { routes }

  let transform seed m =
    match List.find ~f:(route_contains seed) m.routes with
    | Some r -> seed - r.src_start + r.dst_start
    | None -> seed

  let split_range range m =
    let rec loop acc srange routes i =
      let sfirst, slast = srange in

      match routes with
      | route :: rest -> (
          let rfirst, rlast = route_range route in

          if rfirst > slast || rlast < sfirst then loop acc srange rest (i + 1)
          else
            match (route_contains sfirst route, route_contains slast route) with
            | true, true -> (srange, Some i) :: acc
            | false, false ->
                let acc =
                  ((rfirst, rlast), Some i) :: ((sfirst, rfirst), None) :: acc
                in
                let srange = (rlast, slast) in
                loop acc srange rest (i + 1)
            | true, false ->
                let acc = ((sfirst, rlast), Some i) :: acc in
                let srange = (rlast, slast) in
                loop acc srange rest (i + 1)
            | false, true ->
                ((rfirst, slast), Some i) :: ((sfirst, rfirst), None) :: acc)
      | [] -> if slast - sfirst = 0 then acc else (srange, None) :: acc
    in

    loop [] range m.routes 0
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

  let min = List.min_elt locations ~compare:Int.compare |> Option.value_exn in

  Int.to_string min

let part_b input =
  let chunks = chunks_of_string input in

  let seed_ranges =
    List.hd_exn chunks |> seeds_of_string |> List.chunks_of ~length:2
    |> List.filter_map ~f:(function
         | [ start; len ] -> Some (start, start + len)
         | _ -> None)
  in
  let mappings = List.tl_exn chunks |> List.map ~f:Mapping.of_string in

  let transformed =
    List.fold mappings ~init:seed_ranges ~f:(fun ranges m ->
        let ranges =
          List.concat_map ~f:(fun r -> Mapping.split_range r m) ranges
          |> List.map ~f:(fun (r, route) ->
                 match route with
                 | Some i ->
                     let Mapping.{ src_start; dst_start; _ } =
                       List.nth_exn Mapping.(m.routes) i
                     in
                     let offset = dst_start - src_start in
                     let start, stop = r in
                     (start + offset, stop + offset)
                 | None -> r)
        in
        ranges)
  in

  let sorted =
    transformed |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  in
  Stdio.printf "%s\n\n"
    (sorted
    |> List.map ~f:(fun (start, stop) -> Printf.sprintf "%i..%i" start stop)
    |> String.concat ~sep:" ; ");

  let min =
    List.map transformed ~f:(fun (start, _stop) -> start)
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn
  in

  Int.to_string min

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
    let%test_unit "part b" = [%test_result: string] (part_b input) ~expect:"46"
  end)
