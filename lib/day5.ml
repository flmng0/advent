open Util

type stack = char list
type index = int
type instruction = { count : int; from_pos : index; to_pos : index }

let column n line =
  match String.sub line ((n * 4) + 1) 1 with " " -> None | v -> Some v

let parse_state chunk =
  let chunk = List.rev chunk in
  let parse_indices l =
    l |> String.split_on_char ' ' |> List.filter not_empty
    |> List.map int_of_string
  in
  let indices = List.hd chunk |> parse_indices in

  let state = Hashtbl.create 50 in

  let add_line line =
    List.iteri
      (fun i idx ->
        let v = column i line in
        maybe (Hashtbl.add state idx) v)
      indices
  in
  List.iter add_line (List.tl chunk);

  (indices, state)

let print_state indices state =
  List.iter
    (fun idx ->
      let v =
        match Hashtbl.find_opt state idx with Some v -> v | None -> "No value"
      in
      Printf.printf "%d - %s\n" idx v)
    indices

let parse_instructions chunk =
  let scan_line l =
    Scanf.sscanf l "move %d from %d to %d" (fun count from_pos to_pos ->
        { count; from_pos; to_pos })
  in
  List.map scan_line chunk

let print_instructions instructions =
  List.iter
    (fun i ->
      Printf.printf "move %d from %d to %d\n" i.count i.from_pos i.to_pos)
    instructions

let apply_instructions state instrs =
  let out = Hashtbl.copy state in

  let apply_single i =
    for _ = 1 to i.count do
      let v = Hashtbl.find out i.from_pos in
      Hashtbl.add out i.to_pos v;
      Hashtbl.remove out i.from_pos
    done
  in

  List.iter apply_single instrs;
  out

let apply_instructions_retained state instrs =
  let out = Hashtbl.copy state in

  let apply_single i =
    let rec collect_crates acc = function
      | 0 -> acc
      | n ->
          let crate = Hashtbl.find out i.from_pos in
          Hashtbl.remove out i.from_pos;
          collect_crates (crate :: acc) (n - 1)
    in
    let crates = collect_crates [] i.count in
    List.iter (Hashtbl.add out i.to_pos) crates
  in
  List.iter apply_single instrs;

  out

let part_a input =
  let empty_line = function "" -> true | _ -> false in
  let stacks_chunk, instr_chunk = split_seq empty_line (line_seq input) in

  let indices, state = parse_state stacks_chunk in
  let instructions = parse_instructions instr_chunk in

  let result = apply_instructions state instructions in

  print_state indices result;

  indices |> List.map (Hashtbl.find result) |> List.iter (Printf.printf "%s");
  print_newline ();
  ()

let part_b input =
  let empty_line = function "" -> true | _ -> false in
  let stacks_chunk, instr_chunk = split_seq empty_line (line_seq input) in

  let indices, state = parse_state stacks_chunk in
  let instructions = parse_instructions instr_chunk in

  let result = apply_instructions_retained state instructions in

  print_state indices result;

  indices |> List.map (Hashtbl.find result) |> List.iter (Printf.printf "%s");
  print_newline ();
  ()
