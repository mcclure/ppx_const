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
      | { pexp_desc =
          (* Should have name "getenv". *)
          Pexp_extension ({ txt = "const"; loc }, pstr)} ->
        begin match pstr with
        | (* Should have a single structure item, which is evaluation of a constant string. *)
          PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc = Pexp_ifthenelse (cond, thenClause, elseOption) }, _) }] ->
          (* Replace with a constant string with the value from the environment. *)
          let which = false in
          if which then thenClause else (match elseOption with Some x -> x | _ -> 
            Ast_helper.with_default_loc loc (fun _ -> Ast_convenience.unit ()))
        | _ ->
          raise (Location.Error (
                  Location.error ~loc "[%const] accepts an if statement, e.g. if%const true then 1"))
        end      (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  }

let () = register "ifenv" ifenv_mapper
