(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

open Ppxlib
open Parsetree
open Longident
open Location

let ocaml_major, ocaml_minor =
  try
    Scanf.sscanf Sys.ocaml_version "%d.%d" (fun major minor -> major, minor)
  with Scanf.Scan_failure _ -> 4, 7

class mapper =
  object (_self)
    inherit  Ast_traverse.map
    method! expression expr =
        (* Subsitute String.capitalize_ascii to support 4.02 *)
      match expr with
      | ({pexp_desc =
            Pexp_ident ({txt = Ldot (Lident "String", "capitalize_ascii"); _} as ident); _} as expr) when ocaml_major = 4 && ocaml_minor = 2 ->
          { expr with pexp_desc = Pexp_ident { ident with txt = Ldot (Lident "String", "capitalize")}}

      (* Subsitute Stdlib into Pervasives *)
      | ({pexp_desc =
            Pexp_ident ({txt = Ldot (Lident "Stdlib", smth); _} as ident); _} as expr)  when ocaml_major = 4 && ocaml_minor < 7 ->
          { expr with pexp_desc = Pexp_ident { ident with txt = Ldot (Lident "Pervasives", smth)}}

      | expr -> expr
  end

let structure_mapper s = (new mapper)#structure s

let () =
  Ppxlib.Driver.register_transformation
    ~preprocess_impl:structure_mapper
    "compatibility"
