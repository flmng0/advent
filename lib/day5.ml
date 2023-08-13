open Util

type index = int
type instruction = { count : int; from_pos : index; to_pos : index }
type state = { indices : index list; stacks : (index, string) Hashtbl.t }

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

  { indices; stacks = state }

let print_state state =
  List.iter
    (fun idx ->
      let v =
        match Hashtbl.find_opt state.stacks idx with
        | Some v -> v
        | None -> "No value"
      in
      Printf.printf "%d - %s\n" idx v)
    state.indices

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

let apply_instructions state instrs applier =
  let out = Hashtbl.copy state.stacks in

  List.iter (applier out) instrs;

  out

let print_tops indices result =
  indices |> List.map (Hashtbl.find result) |> List.iter (Printf.printf "%s");
  print_newline ()

let solve input crane =
  let empty_line = function "" -> true | _ -> false in
  let stacks_chunk, instr_chunk = split_seq empty_line (line_seq input) in

  let state = parse_state stacks_chunk in
  let instructions = parse_instructions instr_chunk in

  let result = apply_instructions state instructions crane in
  print_tops state.indices result

let part_a input =
  let crane tbl i =
    for _ = 1 to i.count do
      let v = Hashtbl.find tbl i.from_pos in
      Hashtbl.remove tbl i.from_pos;
      Hashtbl.add tbl i.to_pos v;
      ()
    done
  in

  solve input crane

let part_b input =
  let crane tbl i =
    let rec collect_crates acc = function
      | 0 -> acc
      | n ->
          let crate = Hashtbl.find tbl i.from_pos in
          Hashtbl.remove tbl i.from_pos;
          collect_crates (crate :: acc) (n - 1)
    in
    let crates = collect_crates [] i.count in
    List.iter (Hashtbl.add tbl i.to_pos) crates
  in

  solve input crane
