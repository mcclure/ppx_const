open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let getenv s = try Sys.getenv s with Not_found -> ""

let ifenv_mapper argv =
  (* Our ifenv_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      (* Is this an extension node? *)
      | { pexp_loc=loc; pexp_desc =
          (* Should have name "ifenv". *)
          Pexp_ifthenelse (cond, thenClause, elseOption)} ->
            Exp.constant ~loc (Const_string ("STRING", None))
      (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  }

let () = register "ifenv" ifenv_mapper
