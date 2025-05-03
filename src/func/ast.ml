open Ast_common

type program = translation_unit
and translation_unit = { filename: string
                       ; items: top_level_item list }

and top_level_item =
  | ItemFuncDef of function_definition
  | ItemFuncDecl of function_declaration
  | ItemGlobalVarDecls of global_var_declaration list
  | ItemCompilerDirective of compiler_directive
  | ItemConstDecls of constant_declaration list

and compiler_directive =
  | DirectiveInclude of string_literal
  | DirectivePragma of pragma_directive

and pragma_directive = {
  pr_key : string;
  pr_value : string option;
}

and global_var_declaration = {
  gvd_type : typ option;
  gvd_name : expr;
}

and constant_declaration = {
  cd_type : typ option;
  cd_name : expr;
  cd_value : expr;
}

and function_definition = {
  fd_sign: function_declaration;
  fd_body : function_body;
}

and function_declaration = {
  fd_type_variables : type_variable list option;
  fd_ret_ty : typ;
  fd_name : expr;
  fd_args : parameter_declaration list;
  fd_specifiers : specifier list;
}

and function_name = string

and specifier =
  | Impure
  | Inline
  | InlineRef
  | MethodId of expr option

and type_variable = identifier

and parameter_declaration = {
  pd_type : typ;
  pd_name : expr option;
}

and function_body =
  | Block of stmt
  | Asm of asm_function_body

and asm_function_body = asm_specifier * string_literal list

and asm_specifier = {
  as_params : identifier list option;
  as_returns : number_literal list option;
}

and stmt' =
  | StmtVarDef of vardef list
  | StmtDestruct of destruct
  | StmtReturn of expr
  | StmtBlock of stmt list
  | StmtExpr of expr
  | StmtEmpty
  | StmtRepeat of expr * stmt
  | StmtIf of if_stmt
  | StmtDo of stmt * expr
  | StmtWhile of expr * stmt
  | StmtTryCatch of stmt * expr option * stmt
[@@deriving show]
and stmt = stmt' identified
[@@deriving show]

and vardef =
  | VD of { vd_ty: typ
          ; vd_name: expr
          ; vd_init: expr option }
  | VDHole
[@@deriving show]

and destruct = {
  ds_bindings: ds_binding list;
  ds_source: expr;
}
[@@deriving show]
and ds_binding = typ option * expr
[@@deriving show]

and if_stmt = {
  if_cond : expr;
  if_body : stmt;
  if_elseif : elseif_clause list;
  if_else : stmt option;
  if_not : bool;
}
[@@deriving show]
and elseif_clause = {
  elseif_cond : expr;
  elseif_body : stmt;
  elseif_not : bool;
}
[@@deriving show]

and expr' =
  | ExprAssign of expr * assign_op * expr
  | ExprCond of expr * expr * expr
  | ExprBinOp of expr * bin_op * expr
  | ExprUnOp of unary_operator * expr
  | ExprMethodCall of expr * expr * expr list
  | ExprFunCall of expr * expr list
  | ExprId of identifier
  | ExprNum of number_literal
  | ExprBool of bool
  | ExprString of string_literal
  | ExprUnderscore
  | ExprUnit
  | ExprTensor of expr list
  | ExprTuple of expr list
[@@deriving show]
and expr = expr' identified
[@@deriving show]

and typ' =
  | TyPrimitive of primitive_type
  | TyVar
  | TyHole
  | TyIdentifier of identifier
  | TyUnit
  | TyTuple of typ list
  | TyTensor of typ list
  | TyFunction of typ * typ
[@@deriving show]
and primitive_type =
  | TyPrimInt
  | TyPrimCell
  | TyPrimSlice
  | TyPrimBuilder
  | TyPrimCont
  | TyPrimTuple
[@@deriving show]
and typ = typ' identified
[@@deriving show]

let string_ty_prim = function
  | "int" -> Some TyPrimInt
  | "cell" ->  Some TyPrimCell
  | "builder"->Some TyPrimBuilder
  | "cont" ->  Some TyPrimCont
  | "tuple" -> Some TyPrimTuple
  | "slice" -> Some TyPrimSlice
  | _ -> None
