open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let const_mapper argv =
  (* Our const_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with expr =
    (* Create a recursive function and then immediately return it *)
    let rec process mapper expr =
      (* Shared error handler used by multiple cases below *)
      let didnt_find_if loc =
        raise (Location.Error (
                  Location.error ~loc "[%const] accepts an if statement, e.g. if%const true then 1"))
      in
      match expr with
      (* Is this an extension node? *)
      | { pexp_desc = Pexp_extension ({ txt = "const"; loc }, pstr)} ->
        begin match pstr with
        | PStr [{ pstr_desc = Pstr_eval (exp,_) }] ->
          (* Unpack expression, then recurse to handle internal if%matches and match on result *)
          begin match process mapper exp with
            (* Currently only match with ifthenelse *)
            | { pexp_loc  = loc;
                pexp_desc = Pexp_ifthenelse (cond, then_clause, else_opt) } ->
              (* Used by = and <> *)
              let pairTest x y op = 
                match x,y with
                | Pexp_constant x, Pexp_constant y -> op x y
                | _ ->
                  raise (Location.Error (
                      Location.error ~loc:cond.pexp_loc "[%const if...] does not know how to compare these two expressions"))
              in
              (* Evaluate conditional *)
              let which = match cond with
                | [%expr true] -> true
                | [%expr false] -> false
                | [%expr [%e? x] = [%e? y]] ->
                  pairTest x.pexp_desc y.pexp_desc (=)
                | [%expr [%e? x] <> [%e? y]] ->
                  pairTest x.pexp_desc y.pexp_desc (<>)
                | _ ->
                  raise (Location.Error (
                      Location.error ~loc:cond.pexp_loc "[%const if...] does not know how to interpret this kind of expression"))
              in
              (* Depending on value of conditional, replace self extension node with either the then or else clause contents *)
              if which then then_clause else (match else_opt with Some x -> x | _ ->
                (* Or, if the else clause is selected but is not specified, a () *)
                Ast_helper.with_default_loc loc (fun _ -> Ast_convenience.unit ()))
            | { pexp_loc = match_loc;
                pexp_desc = Pexp_match (match_expr, cases) } ->
              let () = match match_expr.pexp_desc with
                | Pexp_constant _ -> ()
                | _ ->
                  raise (Location.Error
                           (Location.error ~loc:match_expr.pexp_loc
                              "[%const match...] does not know how to interpret this kind of expression"))
              in
              let check_case (case : case)  = match case with
                | { pc_lhs = { ppat_desc = Ppat_constant _ }; pc_guard = None; _ }
                | { pc_lhs = { ppat_desc = Ppat_var _ }; pc_guard = None; _ }
                | { pc_lhs = { ppat_desc = Ppat_any }; pc_guard = None; _ } -> ()
                | { pc_guard = Some guard; _ } ->
                  raise (Location.Error
                           (Location.error ~loc:guard.pexp_loc
                              "[%const match...] Guards are not allowed in match%const"))
                | { pc_lhs; _ } ->
                  raise (Location.Error
                           (Location.error ~loc:pc_lhs.ppat_loc
                              "[%const match...] Bad pattern in match%const")) in
              let () = List.iter check_case cases in
              let rec find_match cases = match cases with
                | case :: cases ->
                  begin match case.pc_lhs.ppat_desc with
                    | Ppat_any -> case.pc_rhs
                    | Ppat_var _ ->
                      [%expr let [%p case.pc_lhs] = [%e match_expr] in [%e case.pc_rhs]]
                    | Ppat_constant const ->
                      if match_expr.pexp_desc = Pexp_constant const
                      then case.pc_rhs
                      else find_match cases
                    | _ ->
                      raise (Location.Error
                               (Location.error ~loc:case.pc_lhs.ppat_loc
                                  "[%const match] Bad pattern"))
                  end
                | [] -> raise (Location.Error
                                 (Location.error ~loc:match_loc
                                    "[%const match...] No match case succeeded!"))
              in find_match cases
            (* Failed to match Pexp_ifthenelse, so fail *)
            | _ -> didnt_find_if loc
          end
        (* Failed to match Pstr, so fail *)
        | _ -> didnt_find_if loc
        end
      (* Failed to match Pexp_extension, so hand this off to the default mapper. *)
      | x -> default_mapper.expr mapper x;
    in
    process
  }

let () = register "const" const_mapper
