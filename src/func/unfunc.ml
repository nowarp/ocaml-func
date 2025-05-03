open! IStd
open Ast_common
module Sane = Ast
module Raw = Ast_raw

let cur_filename = ref ""

let is_primitive_type s = Sane.string_ty_prim s |> Option.is_some

let failwith_expr msg expr =
  failwith @@ Printf.sprintf "%s: %s: %s"
    !cur_filename msg @@ Raw.show_expr expr

let unfunc_compiler_directive (cd : Raw.compiler_directive) : Sane.compiler_directive =
  match cd with
  | Raw.DirectiveInclude sl -> Sane.DirectiveInclude sl
  | Raw.DirectivePragma pd ->
    Sane.DirectivePragma { Sane.pr_key = pd.pr_key
                         ; Sane.pr_value = pd.pr_value
                         }

(* Provides contextual conversion function for types. [type_variables_in_scope]
   are the type variables introduced by [forall] (fd_type_variables) in a
   function. *)
let rec expr_to_typ (type_variables_in_scope : string list) (e : Raw.expr) : Sane.typ =
  let mk t = { id = e.id; node = t } in
  match e.node with
  | Raw.ExprId id ->
    Sane.string_ty_prim id
    |> Option.value_map ~default:(Sane.TyIdentifier id |> mk)
                        ~f:(fun id -> Sane.TyPrimitive id |> mk)
  | Raw.ExprFunTy (t1, t2) ->
    mk (Sane.TyFunction (expr_to_typ type_variables_in_scope t1,
                         expr_to_typ type_variables_in_scope t2))
  | Raw.ExprTuple ts ->
    mk (Sane.TyTuple (List.map ~f:(expr_to_typ type_variables_in_scope) ts))

  | Raw.ExprTensor ts ->
    mk (Sane.TyTensor (List.map ~f:(expr_to_typ type_variables_in_scope) ts))
  | Raw.ExprUnderscore ->
    mk Sane.TyHole
  | Raw.ExprUnit ->
    mk Sane.TyUnit
  | _ ->
    failwith_expr "Unknown type" e

let rec unfunc_expr (e : Raw.expr) : Sane.expr =
  let mk node = { id = e.id; node = node } in
  match e.node with
  | Raw.ExprAssign (lhs, op, rhs) ->
    mk (Sane.ExprAssign (unfunc_expr lhs, op, unfunc_expr rhs))
  | Raw.ExprCond (c, t, f) ->
    mk (Sane.ExprCond (unfunc_expr c, unfunc_expr t, unfunc_expr f))
  | Raw.ExprBinOp (lhs, bop, rhs) ->
    mk (Sane.ExprBinOp (unfunc_expr lhs, bop, unfunc_expr rhs))
  | Raw.ExprUnOp (uop, x) ->
    mk (Sane.ExprUnOp (uop, unfunc_expr x))
  | Raw.ExprMethodCall (obj, m, args) ->
    mk (Sane.ExprMethodCall (unfunc_expr obj,
                             unfunc_expr m,
                             List.map ~f:unfunc_expr args))
  | Raw.ExprId id ->
    mk (Sane.ExprId id)
  | Raw.ExprNum n ->
    mk (Sane.ExprNum n)
  | Raw.ExprBool b ->
    mk (Sane.ExprBool b)
  | Raw.ExprString s ->
    mk (Sane.ExprString s)
  | Raw.ExprUnderscore ->
    mk Sane.ExprUnderscore
  | Raw.ExprUnit ->
    mk Sane.ExprUnit
  | Raw.ExprTensor es ->
    mk (Sane.ExprTensor (List.map ~f:unfunc_expr es))
  | Raw.ExprTuple es ->
    mk (Sane.ExprTuple (List.map ~f:unfunc_expr es))
  | Raw.ExprFunCall (name, args)
  | Raw.ExprVarDecl (name, { node = Raw.ExprTensor(args); _ })  ->
    mk (Sane.ExprFunCall (unfunc_expr name, List.map ~f:unfunc_expr args))
  | Raw.ExprVarDecl _ ->
    e |> failwith_expr
      "Unexpected var decl encountered outside of a declaration context"
  | Raw.ExprFunTy _ ->
    e |> failwith_expr
         "Unexpected expression node in expression context"
let unfunc_specifier (sp : Raw.specifier) : Sane.specifier =
  match sp with
  | Raw.Impure -> Sane.Impure
  | Raw.Inline -> Sane.Inline
  | Raw.InlineRef -> Sane.InlineRef
  | Raw.MethodId e_opt ->
    Sane.MethodId (Option.map ~f:(unfunc_expr) e_opt)

let unfunc_type_expr (type_variables_in_scope : string list) (e : Raw.expr option) : Sane.typ option =
  match e with
  | None -> None
  | Some te -> Some (expr_to_typ type_variables_in_scope te)

let unfunc_global_var_decl (type_variables_in_scope : string list) (gvd : Raw.global_var_declaration) : Sane.global_var_declaration =
  { Sane.gvd_type = unfunc_type_expr type_variables_in_scope gvd.gvd_type
  ; Sane.gvd_name = unfunc_expr gvd.gvd_name
  }

let unfunc_constant_decl (type_variables_in_scope : string list) (cd : Raw.constant_declaration) : Sane.constant_declaration =
  { Sane.cd_type = unfunc_type_expr type_variables_in_scope cd.cd_type
  ; Sane.cd_name = unfunc_expr cd.cd_name
  ; Sane.cd_value = unfunc_expr cd.cd_value
  }

let unfunc_asm_specifier (aspec : Raw.asm_specifier) : Sane.asm_specifier =
  { Sane.as_params = aspec.as_params
  ; Sane.as_returns = aspec.as_returns
  }

let unfunc_asm_function_body (afb : Raw.asm_function_body) : Sane.asm_function_body =
  let (aspec, strs) = afb in
  (unfunc_asm_specifier aspec, strs)

let rec extract_vd_sane (e : Raw.expr) : Sane.vardef list option =
  match e.node with
  (* <decl> = <init>; *)
  | Raw.ExprAssign (lhs, OpAssign, rhs) -> begin
    match lhs.node with
    (* <ty> <name> = <init>; *)
    | Raw.ExprVarDecl (({ node = Raw.ExprId _; _ } as ty_expr),
                       ({ node = Raw.ExprId _; _ } as name_expr))
    (* <ty> _ = <init>; *)
    | Raw.ExprVarDecl (({ node = Raw.ExprId _; _ } as ty_expr),
                       ({ node = Raw.ExprUnderscore; _ } as name_expr))
    (* (int, int) p = <init>; *)
    | Raw.ExprVarDecl (({ node = Raw.ExprTuple _
                               | Raw.ExprTensor _;_ } as ty_expr),
                       ({ node = Raw.ExprId _
                               | Raw.ExprUnderscore; _ } as name_expr)) ->
    [ Sane.VD { vd_ty = expr_to_typ [] ty_expr
              ; vd_name = unfunc_expr name_expr
              ; vd_init = unfunc_expr rhs |> Option.some } ]
      |> Option.some
    | Raw.ExprUnderscore ->
      (* _ = <init>; *)
      [ Sane.VDHole ] |> Option.some
    | _ -> None
  end
  (* (<decl>, <decl>);
     (<decl> = <init>, <decl> = <init>);
     (<decl>, <decl> = <init>); *)
  | Raw.ExprTuple ts | Raw.ExprTensor ts ->
    ts
    |> List.fold_left ~init:(return []) ~f:(fun acc t ->
        acc
        >>= fun acc_parts ->
        extract_vd_sane t
        >>= fun parts ->
        return (acc_parts @ parts))
  (* <decl>; *)
  | Raw.ExprVarDecl (ty_expr, ({ node = Raw.ExprId _; _ } as name_expr)) ->
    [ Sane.VD { vd_ty = expr_to_typ [] ty_expr
              ; vd_name = unfunc_expr name_expr
              ; vd_init = None } ]
    |> Option.some
  (* _; *)
  | Raw.ExprUnderscore ->
    [ Sane.VDHole ] |> Option.some
  | _ -> None

let rec extract_vd_destruct (e : Raw.expr) : Sane.destruct option =
  let mk_ty = expr_to_typ [] in (* TODO: function typevars *)
  let mk_ds_part ty_opt name =
    match (ty_opt, name.node) with
      (* var <var_decl>; *)
      | (Some({ node = Raw.ExprId("var"); _ }),
         Raw.ExprVarDecl (real_ty, real_name)) ->
        (mk_ty real_ty |> Option.some, unfunc_expr real_name)
      | _ -> ((ty_opt >>= fun ty -> mk_ty ty |> Option.some), unfunc_expr name)
  in
  let mk_ds init dss =
    { Sane.ds_bindings = dss
    ; Sane.ds_source = unfunc_expr init }
    |> Option.some
  in
  match (e.node) with
  (* <decl> = <init>; *)
  | Raw.ExprAssign (lhs, OpAssign, rhs) -> begin
    match (lhs.node) with
    (* <ty> (<name>, <name>) = <init>;
       <ty> (<name>, <ty> <name>) = <init>;
       <ty> [<name>, <name>] = <init>; *)
    | Raw.ExprVarDecl (({ node = Raw.ExprId _; _ } as ty),
                       ({ node = Raw.ExprTuple ns
                               | Raw.ExprTensor ns; _ })) ->
      ns
      |> List.map ~f:(fun n -> mk_ds_part (Some(ty)) n)
      |> mk_ds rhs
    (* (<ty>, <ty>) (<name>, <name>) = <init>; *)
    | Raw.ExprVarDecl (({ node = Raw.ExprTuple ns
                               | Raw.ExprTensor ns; _ }),
                       ({ node = Raw.ExprTuple ts
                               | Raw.ExprTensor ts; _ }))
      when phys_equal (List.length ns) (List.length ts) ->
        List.zip_exn ts ns
        |> List.map ~f:(fun (t, n) -> mk_ds_part (Some(t)) n)
        |> mk_ds rhs
    (* (<name>, <name>) = <init>;
       (<ty> <name>, <ty> <name>) = <init>;
       (<name>, <ty> <name>) = <init>;
       (<name>, _, <ty> <name>) = <init>; *)
    | Raw.ExprTuple ts
    | Raw.ExprTensor ts ->
      ts
      |> List.fold_left ~init:(return []) ~f:(fun acc t ->
          acc
          >>= fun acc_parts ->
          match t.node with
          | Raw.ExprId _ | Raw.ExprUnderscore ->
            return (acc_parts @ [mk_ds_part None t])
          | Raw.ExprVarDecl (({ node = Raw.ExprId _; _ } as name_expr),
                             ({ node = Raw.ExprId _; _ } as ty_expr)) ->
            return (acc_parts @ [mk_ds_part (Some(ty_expr)) name_expr])
          | _ -> None)
      >>= mk_ds rhs
    | _ -> None
  end
  | _ -> None

let unfunc_vds (e : Raw.expr) : Sane.stmt option =
  let mk node = { id = e.id; node } in
  match extract_vd_destruct e with
  | Some parts -> Some (mk @@ Sane.StmtDestruct parts)
  | None -> match extract_vd_sane e with
            | Some parts -> Some (mk @@ Sane.StmtVarDef parts)
            | None -> None

let rec unfunc_stmt (st : Raw.stmt) : Sane.stmt =
  let mk node = { id = st.id; node } in
  let process_vds e default_stmt =
    e
    |> unfunc_vds
    |> Option.value_or_thunk ~default:(fun _ -> default_stmt @@ unfunc_expr e)
  in
  match st.node with
  (* WARNING: FunC permits pathologically unrestricted variable
     declarations in ANY expression context - we handle only the "sane" subset
     and explicitly reject all other manifestations of this semantic nightmare.
   *)
  | Raw.StmtReturn e ->
    process_vds e (fun expr -> mk @@ Sane.StmtReturn expr)
  | Raw.StmtExpr e ->
    process_vds e (fun expr -> mk @@ Sane.StmtExpr expr)
  | Raw.StmtBlock stmts ->
    mk (Sane.StmtBlock (List.map ~f:unfunc_stmt stmts))
  | Raw.StmtEmpty ->
    mk Sane.StmtEmpty
  | Raw.StmtRepeat (count, body) ->
    mk (Sane.StmtRepeat (unfunc_expr count, unfunc_stmt body))
  | Raw.StmtIf ifs ->
    let unfunc_if () =
      { Sane.if_cond = unfunc_expr ifs.if_condition
      ; Sane.if_body = unfunc_stmt ifs.if_body
      ; Sane.if_elseif =
          List.map ifs.if_elseif ~f:(fun ec ->
            { Sane.elseif_cond = unfunc_expr ec.elseif_cond
            ; Sane.elseif_body = unfunc_stmt ec.elseif_body
            ; Sane.elseif_not = ec.elseif_not
            })
      ; Sane.if_else = Option.map ~f:unfunc_stmt ifs.if_else
      ; Sane.if_not = ifs.if_not
      }
    in
    mk (Sane.StmtIf (unfunc_if ()))
  | Raw.StmtDo (body, cond) ->
    mk (Sane.StmtDo (unfunc_stmt body, unfunc_expr cond))
  | Raw.StmtWhile (cond, body) ->
    mk (Sane.StmtWhile (unfunc_expr cond, unfunc_stmt body))
  | Raw.StmtTryCatch (body, cond, catcher) ->
    mk (Sane.StmtTryCatch (unfunc_stmt body,
                           Option.map ~f:unfunc_expr cond,
                           unfunc_stmt catcher))

let unfunc_function_body (fb : Raw.function_body) : Sane.function_body =
  match fb with
  | Raw.Block s -> Sane.Block (unfunc_stmt s)
  | Raw.Asm afb -> Sane.Asm (unfunc_asm_function_body afb)

let unfunc_parameter_declaration (type_variables_in_scope : string list) (pd : Raw.parameter_declaration) : Sane.parameter_declaration =
  { Sane.pd_type = expr_to_typ type_variables_in_scope pd.pd_type
  ; Sane.pd_name = pd.pd_name >>= fun n -> return @@ unfunc_expr n
  }

let unfunc_function_declaration (fd : Raw.function_declaration) : Sane.function_declaration =
  let type_vars = match fd.fd_type_variables with
    | None -> []
    | Some tvs -> tvs
  in
  { Sane.fd_type_variables = (match fd.fd_type_variables with None -> None | Some l -> Some l)
  ; Sane.fd_ret_ty = expr_to_typ type_vars fd.fd_return_type
  ; Sane.fd_name = unfunc_expr fd.fd_name
  ; Sane.fd_args = fd.fd_arguments
                        |> List.map ~f:(unfunc_parameter_declaration type_vars)
  ; Sane.fd_specifiers = fd.fd_specifiers
                         |> List.map ~f:unfunc_specifier
  }

let unfunc_function_definition (fdef : Raw.function_definition) : Sane.function_definition =
  { Sane.fd_sign = unfunc_function_declaration fdef.fd_signature
  ; Sane.fd_body = unfunc_function_body fdef.fd_body
  }

let unfunc_top_level_item (item : Raw.top_level_item) : Sane.top_level_item =
  match item with
  | Raw.ItemFuncDef fd -> Sane.ItemFuncDef (unfunc_function_definition fd)
  | Raw.ItemFuncDecl fd -> Sane.ItemFuncDecl (unfunc_function_declaration fd)
  | Raw.ItemGlobalVarDecls gvds ->
    Sane.ItemGlobalVarDecls (List.map ~f:(unfunc_global_var_decl []) gvds)
  | Raw.ItemCompilerDirective cd ->
    Sane.ItemCompilerDirective (unfunc_compiler_directive cd)
  | Raw.ItemConstDecls cds ->
    Sane.ItemConstDecls (List.map ~f:(unfunc_constant_decl []) cds)

let unfunc_elseif_clause (ec : Raw.elseif_clause) : Sane.elseif_clause =
  { Sane.elseif_cond = unfunc_expr ec.elseif_cond
  ; Sane.elseif_body = unfunc_stmt ec.elseif_body
  ; Sane.elseif_not = ec.elseif_not
  }

let unfunc_if_stmt (ifs : Raw.if_stmt) : Sane.if_stmt =
  { Sane.if_cond = unfunc_expr ifs.if_condition
  ; Sane.if_body = unfunc_stmt ifs.if_body
  ; Sane.if_elseif = List.map ~f:unfunc_elseif_clause ifs.if_elseif
  ; Sane.if_else = Option.map ~f:unfunc_stmt ifs.if_else
  ; Sane.if_not = ifs.if_not
  }

let unfunc_translation_unit (tu : Raw.translation_unit) : Sane.translation_unit =
  { Sane.filename = tu.filename
  ; Sane.items = List.map ~f:unfunc_top_level_item tu.items
  }

let unfunc_raw_ast (ast : Raw.program) : Sane.program =
  (* Printf.printf "%s\n" @@ Raw.show_program ast; *)
  cur_filename := ast.filename;
  unfunc_translation_unit ast
