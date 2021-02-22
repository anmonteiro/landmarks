(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Ppxlib

let auto = ref false
let remove = ref false
let threads = Mapper.with_thread

let split c s =
  let open String in
  let res = ref [] in
  let pos = ref 0 in
  let len = length s in
  while
    match index_from s !pos c with
    | exception Not_found ->
        res := sub s !pos (len - !pos) :: !res;
        false
    | k ->
        res := sub s !pos (k - !pos) :: !res;
        pos := k + 1;
        !pos < len || (res := "" :: !res; false)
  do () done;
  List.rev !res

let default_auto, default_remove, default_threads =
  match Sys.getenv "OCAML_LANDMARKS" with
  | exception Not_found -> false, false, false
  | env ->
      let opts = split ',' env in
      List.mem "auto" opts,
      List.mem "remove" opts,
      List.mem "threads" opts

let () =
  let args = Arg.[
      "--remove", Set remove, "ignore all landmarks annotations.";
      "--thread", Set threads, "use the thread-safe version.";
      "--auto", Set auto, "measure all top-level functions."]
  in
  List.iter (fun (k, spec, doc) -> Ppxlib.Driver.add_arg k spec ~doc) args;
  let mapper s =
    let mapper = if !remove && not !auto then
      new Ast_traverse.map
    else if !remove then
      (new Mapper.remove_attributes)
    else
      new Mapper.toplevel_mapper !auto
    in
    mapper#structure s
  in
  Ppxlib.Driver.register_transformation
    ~preprocess_impl:mapper
    "landmarks"
