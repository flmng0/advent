let day = 11

open Util

module Monkey = struct
  type key = string
  type operation = Add of int | Mul of int
  type test = Dibivisible of int

  type t = {
    mutable inspections : int;
    mutable items : int Queue.t;
    op : operation;
    test : test;
    success : key;
    failure : key;
  }
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

  (* let of_lines l = *)
  (*   let key = Scanf.sscanf (List.hd l) "Monkey %d:" identity in *)
  (*   let items = Scanf.sscanf () *)
  (*   (key, monkey) *)
end

let part_a _ch = ()
let part_b _ch = ()
