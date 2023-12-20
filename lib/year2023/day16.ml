open Util
open Base

type dir = Up | Right | Down | Left [@@deriving equal]
type tile = Air | MirrorTRBL | MirrorTLBR | SplitV | SplitH
type t = { tiles : tile array; width : int; height : int }

let tile_of_char = function
  | '.' -> Air
  | '/' -> MirrorTRBL
  | '\\' -> MirrorTLBR
  | '-' -> SplitH
  | '|' -> SplitV
  | c -> Printf.invalid_argf "Invalid character in input: %c" c ()

let reflect dir = function
  | Air -> [ dir ]
  | MirrorTRBL -> (
      match dir with
      | Up -> [ Right ]
      | Right -> [ Up ]
      | Down -> [ Left ]
      | Left -> [ Down ])
  | MirrorTLBR -> (
      match dir with
      | Up -> [ Left ]
      | Right -> [ Down ]
      | Down -> [ Right ]
      | Left -> [ Up ])
  | SplitH -> ( match dir with Right | Left -> [ dir ] | _ -> [ Left; Right ])
  | SplitV -> ( match dir with Up | Down -> [ dir ] | _ -> [ Up; Down ])

let get grid x y =
  if x < 0 || x >= grid.width || y < 0 || y >= grid.height then None
  else
    let i = x + (y * grid.width) in
    Some grid.tiles.(i)

let parse input =
  let lines = lines_of_string input in

  let width = List.hd_exn lines |> String.length in
  let height = List.length lines in

  let parse_line line = line |> String.to_list |> List.map ~f:tile_of_char in

  let tiles = lines |> List.concat_map ~f:parse_line |> Array.of_list in

  { tiles; width; height }

let show ?(visited = []) grid =
  let { tiles; width; height } = grid in
  List.range 0 height
  |> List.iter ~f:(fun y ->
         List.range 0 width
         |> List.iter ~f:(fun x ->
                let i = x + (y * width) in

                if List.mem visited (x, y) ~equal:IntPair.equal then
                  Stdio.printf "#"
                else
                  let char =
                    match tiles.(i) with
                    | Air -> '.'
                    | MirrorTLBR -> '\\'
                    | MirrorTRBL -> '/'
                    | SplitV -> '|'
                    | SplitH -> '-'
                  in
                  Stdio.printf "%c" char);
         Stdio.print_endline "")

let move x y = function
  | Up -> (x, y - 1)
  | Right -> (x + 1, y)
  | Down -> (x, y + 1)
  | Left -> (x - 1, y)

let get_energized grid x y dir =
  let rec loop visited = function
    | (x, y, dir) :: rest -> (
        if
          (* If already gone through this tile, in the current direction, then that's a loop. *)
          List.mem ~equal:equal_dir (Map.find_multi visited (x, y)) dir
        then loop visited rest
        else
          match get grid x y with
          | Some tile ->
              let visited = Map.add_multi visited ~key:(x, y) ~data:dir in
              let next =
                reflect dir tile
                |> List.map ~f:(fun d ->
                       let x, y = move x y d in
                       (x, y, d))
              in

              let rest = List.rev_append rest next in
              loop visited rest
          | None -> loop visited rest)
    | [] -> visited
  in

  loop (Map.empty (module IntPair)) [ (x, y, dir) ]

let day = 16

let part_a input =
  let grid = parse input in

  let visited = get_energized grid 0 0 Right in
  visited |> Map.length |> Int.to_string

let part_b input =
  let grid = parse input in

  let xrange = List.range 0 grid.width in
  let yrange = List.range 0 grid.height in

  [
    List.map xrange ~f:(fun x -> (x, 0, Down));
    List.map yrange ~f:(fun y -> (0, y, Right));
    List.map xrange ~f:(fun x -> (x, grid.height - 1, Up));
    List.map yrange ~f:(fun y -> (grid.width - 1, y, Left));
  ]
  |> List.concat
  |> List.map ~f:(fun (x, y, d) -> get_energized grid x y d |> Map.length)
  |> List.max_elt ~compare:Int.compare
  |> Option.value_exn |> Int.to_string

let%test_module "day 16" =
  (module struct
    let input =
      {|.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
|}

    let%test_unit "part a" = [%test_result: string] (part_a input) ~expect:"46"
    let%test_unit "part b" = [%test_result: string] (part_b input) ~expect:"51"
  end)
