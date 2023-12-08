let day = 11

(*
open Util

module Monkey = struct
  type key = string
  type value = Self | Num of int
  type operation = Add of value | Mul of value

  type t = {
    mutable inspections : int;
    items : int Queue.t;
    op : operation;
    test : int;
    success : key;
    failure : key;
  }

  let pp_op op =
    let pp_v = function
    | Self -> "old"
    | Num i -> string_of_int i
    in
    match op with
    | Add v -> "+ " ^ (pp_v v)
    | Mul v -> "* " ^ (pp_v v)
  let pp m =
    let items = Queue.to_seq |> List.of_seq |> String.concat ", " in ()
    
  (*
    Each monkey keeps track of its current items.

    A monkey can inspect an item, which applies some operation in the format:
      new = old [*+] (old|\d+)
      i.e: new = old * old
          new = old + 17
          new = old * 32

    The worry level of an item is always divided by 3 before it is tested.

    Tests are in the form of:
      divisible by N
      i.e: divisible by 17
          divisible by 33

    A monkey can have a target resulting from the outcome of the above test.

    If the test is successful, throw to monkey with key "success", else throw
    to monkey with key "failure"
  *)

  let parse_op op_op op_v =
    let v = match op_v with "old" -> Self | n -> Num (int_of_string n) in
    match op_op with
    | '*' -> Mul v
    | '+' -> Add v
    | _ -> invalid_arg "Unexpected operator"

  let of_string s =
    let create key items op_op op_v test success failure =
      let inspections = 0 in
      let items =
        items |> String.split_on_char ','
        |> List.map (fun i -> String.trim i |> int_of_string)
        |> List.to_seq |> Queue.of_seq
      in
      let op = parse_op op_op op_v in

      let monkey = { inspections; items; op; test; success; failure } in
      (key, monkey)
    in

    Scanf.sscanf s
      {|Monkey %s@:
  Starting items: %[0-9, ]
  Operation: new = old %c %[0-9(old)]
  Test: divisible by %d
    If true: throw to monkey %s
    If false: throw to monkey %s|}
      create

  let catch item m = Queue.add item m.items

  let apply_op op item =
    let num = function Self -> item | Num n -> n in
    match op with Add v -> item + num v | Mul v -> item * num v

  let take_turn m =
    let inspect item =
      m.inspections <- m.inspections + 1;
      apply_op m.op item
    in

    let get_bored item = item / 3 in

    let test item = item mod m.test = 0 in

    let rec loop acc =
      match Queue.take_opt m.items with
      | Some item ->
          let item = item |> inspect |> get_bored in
          let target = if test item then m.success else m.failure in
          loop ((target, item) :: acc)
      | None -> List.rev acc
    in

    loop []
end

module StringMap = Map.Make (String)

let run_round monkeys =
  StringMap.iter
    (fun _ m ->
      let thrown = Monkey.take_turn m in
      List.iter
        (fun (target, item) ->
          StringMap.find target monkeys |> Monkey.catch item)
        thrown)
    monkeys

let part_a ch =
  let open Monkey in
  let monkeys =
    get_chunks ch |> List.map Monkey.of_string |> List.to_seq
    |> StringMap.of_seq
  in

  for _ = 1 to 20 do
    run_round monkeys
  done;

  let top_two =
    StringMap.bindings monkeys
    |> List.map (fun (_, m) -> m.inspections)
    |> List.sort Stdlib.compare |> List.rev |> List.to_seq |> Seq.take 2
  in
  let monkey_business = Seq.fold_left ( * ) 1 top_two in
  Printf.printf "Monkey business: %d\n" monkey_business

(* StringMap.to_seq |> Seq.map (fun k m -> (m.inspections, k)) |> List.of_seq |> List.sort (fun (ai, _) (bi, _) -> Stdlib.compare ai bi) |> List.to_seq |> Seq.take 2 |> Seq.map () *)

let part_b _ch = ()
*)
