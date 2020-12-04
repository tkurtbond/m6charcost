(* om6.ml - calculate point costs of Mini-Six Characters in JSON. *)

(* Remember: ./om6.native ~/current/RPG/the-kids/Mini-Six/Star-Wars/kids-pcs/kids-pcs-2.json *)
open Core
open Stdlib

let calculate_character (character : Yojson.Basic.t) =
  print_endline "calculate_character";
  Yojson.Basic.pp Format.std_formatter character;
  print_newline ();
  let open Yojson.Basic.Util in 
  let assoc_has a l =
    match List.assoc_opt a (to_assoc l) with Some b -> true | None -> false in
  if assoc_has "Name" character then begin
    let name = character |> member "Name" |> to_string in
    Printf.printf "Name: %s\n" name
  end;
  if assoc_has "Description" character then begin
    let desc = character |> member "Description" |> to_string in
    Printf.printf "    %s\n" desc
  end;
  if assoc_has "Player" character then begin
    let player = character |> member "Player" |> to_string in
    Printf.printf "Player: %s\n" player
  end;
  let statistics =
    try character |> member "Statistics" |> to_list |> filter_string
    with Type_error (_, _)-> ["Might"; "Agility"; "Wit"; "Charm"] in 
  let iter_stats stat_name =
    let `List (stat_value_json:`List (skills)) in
    let stat_value = to_string stat_value_json in
    Printf.printf "    %s: %s\n" statname stat_value in
  List.iter iter_stats statistics

let process_filename filename =
  Printf.eprintf "Filename: %s\n" filename;
  flush stderr;
  let json = Yojson.Basic.from_file filename in
  match json with
  | `List l -> List.iter calculate_character l
  | _ -> raise (Failure "Unexpected JSON - not a `List")

let () =
  match Sys.argv with
  | [| _ |] -> raise (Failure "No argument supplied!")
  | [| _; filename |] -> process_filename filename
  | _ -> raise (Failure "Too many arguments supplied!")
