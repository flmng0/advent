open Base

let strip_trail lines =
  let rec strip_trail' = function
    | head :: rest ->
        if String.length head = 0 then strip_trail' rest else head :: rest
    | [] -> invalid_arg "No non-empty lines"
  in
  strip_trail' (List.rev lines) |> List.rev

let lines_of_string ?(include_trail = false) data =
  let all = String.split_lines data in
  if include_trail then all else strip_trail all

let read_nums text =
  text |> String.split ~on:' '
  |> List.filter_map ~f:(fun t -> String.strip t |> Int.of_string_opt)

let chunks_of_string data =
  let rec loop acc current = function
    | line :: rest -> (
        match line with
        | "" ->
            let chunk = List.rev current |> String.concat_lines in
            loop (chunk :: acc) [] rest
        | line -> loop acc (line :: current) rest)
    | [] ->
        let chunk = List.rev current |> String.concat_lines in
        List.rev (chunk :: acc)
  in

  let lines = String.split_lines data in
  loop [] [] lines

let prime_decomp n =
  let rec prime_decomp' c p =
    if p < c * c then [ p ]
    else if Int.rem p c = 0 then c :: prime_decomp' c (p / c)
    else prime_decomp' (c + 1) p
  in
  prime_decomp' 2 n

let hit_count cmp vals =
  List.fold vals ~init:(Map.empty cmp)
    ~f:(Map.update ~f:(function Some n -> n + 1 | None -> 1))

let lcm vals =
  let f acc v =
    let hits = hit_count (module Int) (prime_decomp v) in
    Map.merge_skewed acc hits ~combine:(fun ~key a b ->
        ignore key;
        max a b)
  in
  let primes = List.fold vals ~init:(Map.empty (module Int)) ~f in
  Map.fold primes ~init:1 ~f:(fun ~key ~data acc -> (key ** data) * acc)

let pairs n =
  let open List in
  range 0 (n - 1) |> concat_map ~f:(fun a -> range (a + 1) n |> map ~f:(fun b -> (a, b)))

module IntPair = struct
  module T = struct
    type t = int * int [@@deriving equal, compare, sexp_of]
  end

  include T
  include Comparator.Make (T)
end

module Vector = struct
  open Float

  type t = { x : float; y : float }

  (* Constructors *)
  let zero = { x = 0.0; y = 0.0 }
  let unit_x = { x = 1.0; y = 0.0 }
  let unit_x_neg = { x = -1.0; y = 0.0 }
  let unit_y = { x = 0.0; y = 1.0 }
  let unit_y_neg = { x = 0.0; y = -1.0 }
  let of_xy x y = { x; y }
  let of_int_xy x y = { x = of_int x; y = of_int y }

  let of_point p =
    let x, y = p in
    of_xy x y

  let of_int_point p =
    let x, y = p in
    of_int_xy x y

  (* Operator functions *)
  let add v w = { x = v.x + w.x; y = v.y + w.y }
  let sub v w = { x = v.x - w.x; y = v.y - w.y }
  let neg v = { x = -v.x; y = -v.y }
  let mul v m = { x = v.x * m; y = v.y * m }
  let div v m = { x = v.x / m; y = v.y / m }

  (* Methods *)
  let length_sq v = (v.x * v.x) + (v.y * v.y)
  let length v = length_sq v |> sqrt

  let normal v =
    let l = length v in
    div v l

  let with_length v l = mul (normal v) l

  (* Operators *)
  let ( + ) v w = add v w
  let ( - ) v w = sub v w
  let ( ~- ) v = neg v
  let ( * ) v m = mul v m
  let ( / ) v m = div v m
end

let%test_unit "prime_decomp" =
  [%test_result: int list] (prime_decomp 12) ~expect:[ 2; 2; 3 ]

let%test_unit "lcm two" = [%test_result: int] (lcm [ 12; 18 ]) ~expect:36

let%test_unit "lcm many" =
  [%test_result: int] (lcm [ 10; 16; 24; 85 ]) ~expect:4080

let%test_unit "pairs" = 
  [%test_result: (int * int) list] (pairs 4) ~expect:[0, 1; 0, 2; 0, 3; 1, 2; 1, 3; 2, 3]
