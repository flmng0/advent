open Base

let hash t =
  let rec loop n = function
    | c :: rest ->
        let n = (n + Char.to_int c) * 17 % 256 in
        loop n rest
    | [] -> n
  in
  loop 0 (String.to_list t)

type step = Remove of string | Add of string * int

let parse s =
  match String.chop_suffix s ~suffix:"-" with
  | Some label -> Remove label
  | None ->
      let label, box = String.lsplit2_exn s ~on:'=' in
      let box = Int.of_string box in
      Add (label, box)

let day = 15

let part_a input =
  let input = String.filter input ~f:(function '\n' -> false | _ -> true) in
  let steps = String.split input ~on:',' in

  steps
  |> List.fold ~init:0 ~f:(fun acc step -> acc + hash step)
  |> Int.to_string

let part_b input =
  let input = String.filter input ~f:(function '\n' -> false | _ -> true) in
  let steps = String.split input ~on:',' |> List.map ~f:parse in

  let equal = String.equal in

  let run_step boxes = function
    | Remove label ->
        let box = hash label in
        Map.change boxes box
          ~f:
            (Option.map ~f:(fun lenses -> List.Assoc.remove lenses label ~equal))
    | Add (label, v) ->
        let box = hash label in
        Map.update boxes box ~f:(fun lenses ->
            let lenses = Option.value ~default:[] lenses in
            if List.Assoc.mem lenses label ~equal then
              List.map lenses ~f:(fun (label', v') ->
                  if equal label' label then (label, v) else (label', v'))
            else List.Assoc.add lenses label v ~equal)
  in

  let boxes = steps |> List.fold ~init:(Map.empty (module Int)) ~f:run_step in

  let total =
    boxes
    |> Map.fold ~init:0 ~f:(fun ~key ~data total ->
           data |> List.rev
           |> List.foldi ~init:total ~f:(fun i acc (_, power) ->
                  acc + ((i + 1) * (key + 1) * power)))
  in
  Int.to_string total

let%test_module "day 15" =
  (module struct
    let input = {|rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7|}
    let%test_unit "hash" = [%test_result: int] (hash "ot") ~expect:3

    let%test_unit "part a" =
      [%test_result: string] (part_a input) ~expect:"1320"

    let%test_unit "part b" = [%test_result: string] (part_b input) ~expect:"145"
  end)
