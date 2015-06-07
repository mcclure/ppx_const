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
                  Location.error ~loc "[%const] accepts an if statement, e.g. if%const true then 1, or a match statement"))
      in
      match expr with
      (* Is this an extension node? *)
      | { pexp_desc = Pexp_extension ({ txt = "const"; loc }, pstr)} ->
        begin match pstr with
        | PStr [{ pstr_desc = Pstr_eval (exp,_) }] ->
          (* Unpack expression, then recurse to handle internal if%matches and match on result *)
          begin match process mapper exp with
            (* Syntax extension 1 -- ifthenelse *)
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

            (* Syntax extension 1 -- match *)
            | { pexp_loc = match_loc;
                pexp_desc = Pexp_match (match_expr, cases) } ->
              (* Basic syntax-check expression *)
              let () = match match_expr with
                | { pexp_desc = Pexp_constant _; _ }
                | [%expr true]
                | [%expr false] -> ()
                | _ ->
                  raise (Location.Error
                           (Location.error ~loc:match_expr.pexp_loc
                              "[%const match...] does not know how to interpret this kind of expression"))
              in
              (* Syntax-check, bar "when" *)
              let check_case (case : case)  = match case with
                | { pc_guard = Some guard; _ } ->
                  raise (Location.Error
                           (Location.error ~loc:guard.pexp_loc
                              "[%const match...] Guards are not allowed in match%const"))
                | { pc_lhs = { ppat_desc = Ppat_constant _ }; _ }
                | { pc_lhs = [%pat? true]; _ }
                | { pc_lhs = [%pat? false]; _ }
                | { pc_lhs = { ppat_desc = Ppat_var _ }; _ }
                | { pc_lhs = [%pat? _]; _ } -> ()
                | { pc_lhs; _ } ->
                  raise (Location.Error
                           (Location.error ~loc:pc_lhs.ppat_loc
                              "[%const match...] Bad pattern in match%const")) in
              let () = List.iter check_case cases in
              (* Evaluate match, check | expressions one by one *)
              let rec find_match cases = match cases with
                | case :: cases ->
                  begin match case.pc_lhs with
                    (* _ always matches *)
                    | [%pat? _] -> case.pc_rhs
                    (* Variable names always match become "bindings", as with normal match *)
                    | { ppat_desc = Ppat_var _; _ } ->
                      [%expr let [%p case.pc_lhs] = [%e match_expr] in [%e case.pc_rhs]]
                    (* Constants get tested for equality *)
                    | { ppat_desc = Ppat_constant const; _ } ->
                      if match_expr.pexp_desc = Pexp_constant const
                      then case.pc_rhs
                      (* When matches are not found, recurse *)
                      else find_match cases
                    (* true and false are special case *)
                    | [%pat? true] -> begin match match_expr with
                        | [%expr true] -> case.pc_rhs
                        | _ -> find_match cases
                      end
                    | [%pat? false] -> begin match match_expr with
                        | [%expr false] -> case.pc_rhs
                        | _ -> find_match cases
                      end
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
