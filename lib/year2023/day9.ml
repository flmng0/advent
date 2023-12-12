open Util
open Base

module History = struct
  type t = int list

  let predict ?(backwards = false) h =
    let diffs vals =
      let rec diffs' acc = function
        | a :: b :: vals -> diffs' ((b - a) :: acc) (b :: vals)
        | _ -> acc
      in
      List.rev (diffs' [] vals)
    in

    let rec seq_loop acc vals =
      let d = diffs vals in
      if List.for_all d ~f:(( = ) 0) then d :: acc else seq_loop (d :: acc) d
    in

    let seqs = seq_loop [ h ] h in

    let rec predict' x = function
      | seq :: seqs ->
          let x =
            if backwards then List.hd_exn seq - x
            else x + List.hd_exn (List.rev seq)
          in
          predict' x seqs
      | [] -> x
    in

    predict' 0 seqs

  let of_string s = read_nums s
end

let day = 9

let part_a input =
  let lines = lines_of_string input in
  let histories = List.map lines ~f:History.of_string in

  let preds = List.map histories ~f:History.predict in

  let total = List.fold preds ~init:0 ~f:( + ) in

  Int.to_string total

let part_b input =
  let lines = lines_of_string input in
  let histories = List.map lines ~f:History.of_string in

  let preds = List.map histories ~f:(History.predict ~backwards:true) in

  let total = List.fold preds ~init:0 ~f:( + ) in

  Int.to_string total

let%test_module "day 9" =
  (module struct
    let input = {|0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
|}

    let%test_unit "part a" = [%test_result: string] (part_a input) ~expect:"114"
    let%test_unit "part b" = [%test_result: string] (part_b input) ~expect:"2"
  end)
