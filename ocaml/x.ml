(* https://discuss.ocaml.org/t/ocaml-yaml-extracting-value-using-a-key/6396/4 
   https://opam.ocaml.org/packages/yaml/

*)

open Core
open Yaml

let ys = In_channel.read_all "/Users/tkb/current/RPG/the-kids/Mini-Six/Star-Wars/kids-pcs/kids-pcs-3.json"

let y = yaml_of_string ys
