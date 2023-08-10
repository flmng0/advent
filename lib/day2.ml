open Util

type shape = Rock | Paper | Scissors
type outcome = Lose | Win | Tie
type instruction = Shape of shape | Outcome of outcome
type turn = { yours : instruction; theirs : shape }

let shape_score = function Rock -> 1 | Paper -> 2 | Scissors -> 3
let outcome_score = function Lose -> 0 | Tie -> 3 | Win -> 6

let get_shape = function
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | _ -> raise (Invalid_argument "Move not valid")

let shape_from_outcome desired theirs =
  match desired with
  | Win -> (
      match theirs with Rock -> Paper | Paper -> Scissors | Scissors -> Rock)
  | Lose -> (
      match theirs with Rock -> Scissors | Paper -> Rock | Scissors -> Paper)
  | Tie -> theirs

let get_outcome yours theirs =
  match (yours, theirs) with
  | Rock, Paper | Paper, Scissors | Scissors, Rock -> Lose
  | Paper, Rock | Scissors, Paper | Rock, Scissors -> Win
  | _, _ -> Tie

let line_to_turn convert line =
  match String.split_on_char ' ' line with
  | [ a; b ] -> { yours = convert b; theirs = get_shape a }
  | _ -> raise (Invalid_argument "Line not expected")

let turn_to_score t =
  match t.yours with
  | Shape s ->
      let o = get_outcome s t.theirs in
      shape_score s + outcome_score o
  | Outcome o ->
      let s = shape_from_outcome o t.theirs in
      shape_score s + outcome_score o

let solve input convert =
  let turns =
    Seq.unfold input_line_opt input |> Seq.map (line_to_turn convert)
  in
  let scores = Seq.map turn_to_score turns in
  let total = Seq.fold_left ( + ) 0 scores in

  total

let part_a input =
  let convert = function
    | "X" -> Shape Rock
    | "Y" -> Shape Paper
    | "Z" -> Shape Scissors
    | _ -> raise (Invalid_argument "Move input not valid")
  in
  let total = solve input convert in

  Printf.printf "Total score: %d\n" total;

  ()

let part_b input =
  let convert = function
    | "X" -> Outcome Lose
    | "Y" -> Outcome Tie
    | "Z" -> Outcome Win
    | _ -> raise (Invalid_argument "Outcome input not valid")
  in
  let total = solve input convert in

  Printf.printf "Total score: %d\n" total;

  ()
