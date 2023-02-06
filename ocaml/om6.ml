let find key (yaml : Yaml.value) = match yaml with 
  | `O assoc -> List.assoc_opt key assoc
  | _ -> None

let process_character character =
  match character with
  | `O alist ->
     match List.assoc "Name" alist

let process_characters characters =
  List.iter process_character characters

let () =
  let y = Yaml_unix.of_file_exn Fpath.(v "../test-files/kids-pcs-3.yaml") in
  match y with
  | `A characters ->
      process_characters characters
  | _ -> failwith "unable to understand input file"
