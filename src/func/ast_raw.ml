open Ast_common

type program = translation_unit
[@@deriving show]

and translation_unit = { filename: string
                       ; items: top_level_item list }
[@@deriving show]

and compiler_directive =
  | DirectiveInclude of string_literal
  | DirectivePragma of pragma_directive

and pragma_directive = {
  pr_key : string;
  pr_value : string option;
}

and top_level_item =
  | ItemFuncDef of function_definition
  | ItemFuncDecl of function_declaration
  | ItemGlobalVarDecls of global_var_declaration list
  | ItemCompilerDirective of compiler_directive
  | ItemConstDecls of constant_declaration list

and global_var_declaration = {
  gvd_type : expr option;
  gvd_name : expr;
}

and constant_declaration = {
  cd_type : expr option;
  cd_name : expr;
  cd_value : expr;
}

and function_definition = {
  fd_signature: function_declaration;
  fd_body : function_body;
}

and function_declaration = {
  fd_type_variables : type_variable list option;
  fd_return_type : expr;
  fd_name : expr;
  fd_arguments : parameter_declaration list;
  fd_specifiers : specifier list;
}

and specifier =
  | Impure
  | Inline
  | InlineRef
  | MethodId of expr option

and type_variable = identifier

and parameter_declaration = {
  pd_type : expr;
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
  | StmtReturn of expr
  | StmtBlock of stmt list
  | StmtExpr of expr
  | StmtEmpty
  | StmtRepeat of expr * stmt
  | StmtIf of if_stmt
  | StmtDo of stmt * expr
  | StmtWhile of expr * stmt
  | StmtTryCatch of stmt * expr option * stmt
and stmt = stmt' identified

and if_stmt = {
  if_condition : expr;
  if_body : stmt;
  if_elseif : elseif_clause list;
  if_else : stmt option;
  if_not : bool;
}
and elseif_clause = {
  elseif_cond : expr;
  elseif_body : stmt;
  elseif_not : bool;
}

(* A spectacle of syntactic horror containing both expressions and types *)
and expr' =
  | ExprAssign of expr * assign_op * expr
  | ExprVarDecl of expr * expr
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
  | ExprFunTy of expr * expr
[@@deriving show]
and expr = expr' identified
[@@deriving show]
