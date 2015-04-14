open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let getenv s = try Sys.getenv s with Not_found -> ""

let rec ifenv_mapper argv =
  (* Our ifenv_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with
    expr = let rec process mapper expr =
      match expr with
      (* Is this an extension node? *)
      | { pexp_desc =
          (* Should have name "getenv". *)
          Pexp_extension ({ txt = "const"; loc }, pstr)} ->
        begin match pstr with
        | (* Should have a single structure item, which is evaluation of a constant string. *)
          PStr [{ pstr_desc =
                  Pstr_eval ({ pexp_loc  = loc;
                               pexp_desc = Pexp_ifthenelse( {pexp_loc=cond_loc;pexp_desc=cond_desc},
                                  then_clause, else_option) }, _) }] ->
          (* Replace with a constant string with the value from the environment. *)
          let which = match cond_desc with
            | Pexp_construct ({txt=Lident "true"},None) -> true
            | Pexp_construct ({txt=Lident "false"},None) -> false
            | Pexp_apply( {pexp_desc=Pexp_ident({txt=Lident "="})}, [_,{pexp_desc=x};_,{pexp_desc=y}] ) ->
              begin match x,y with
                | Pexp_constant x, Pexp_constant y -> x = y
                | _ ->
                  raise (Location.Error (
                    Location.error ~loc:cond_loc "[%const if...] does not know how to compare those two expressions"))
              end
            | _ ->
              raise (Location.Error (
                  Location.error ~loc:cond_loc "[%const if...] does not know how to interpret that kind of expression"))
          in
          let recurse = process mapper in
          if which then recurse then_clause else (match else_option with Some x -> recurse x | _ -> 
            Ast_helper.with_default_loc loc (fun _ -> Ast_convenience.unit ()))
        | _ ->
          raise (Location.Error (
                  Location.error ~loc "[%const] accepts an if statement, e.g. if%const true then 1"))
        end      (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
    in
    process
  }

let () = register "ifenv" ifenv_mapper
