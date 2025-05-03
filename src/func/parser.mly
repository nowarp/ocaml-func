%{
open Ast_raw
open Ast_common

let save node (lo, hi) =
  let id = Id.next_id () in
  Span.save id (lo.Lexing.pos_bol) (hi.Lexing.pos_bol);
  { id; node }
%}

%token <string> IDENTIFIER
%token <string> NUMBER_LITERAL
%token <string> STRING_LITERAL
%token <string * char> STRING_LITERAL_SUFFIX

%token INCLUDE
%token PRAGMA
%token COMPUTE_ASM_LTR
%token ALLOW_POST_MODIFICATION
%token ASM
%token TRUE
%token FALSE
%token FORALL
%token INLINE_REF
%token INLINE
%token IMPURE
%token METHOD_ID
%token UNDERSCORE
%token GLOBAL
%token CONST
%token RETURN
%token REPEAT
%token IF
%token IFNOT
%token ELSE
%token ELSEIF
%token ELSEIFNOT
%token DO
%token UNTIL
%token WHILE
%token TRY
%token CATCH

%token EQ               "="
%token PLUS_EQ          "+="
%token MINUS_EQ         "-="
%token STAR_EQ          "*="
%token SLASH_EQ         "/="
%token TILDESLASH_EQ    "~/="
%token HATSLASH_EQ      "^/="
%token PERCENT_EQ       "%="
%token TILDEPERCENT_EQ  "~%="
%token HATPERCENT_EQ    "^%="
%token LTLT_EQ          "<<="
%token GTGT_EQ          ">>="
%token TILDEGTGT_EQ     "~>>="
%token HATGTGT_EQ       "^>>="
%token AMP_EQ           "&="
%token PIPE_EQ          "|="
%token HAT_EQ           "^="

%token QUESTION     "?"
%token COLON        ":"

%token EQEQ         "=="
%token LT           "<"
%token GT           ">"
%token LEQ          "<="
%token GEQ          ">="
%token NEQ          "!="
%token LTLTGT       "<<>"

%token LTLT         "<<"
%token GTGT         ">>"
%token TILDEGTGT    "~>>"
%token HATGTGT      "^>>"

%token MINUS        "-"
%token PLUS         "+"
%token PIPE         "|"
%token HAT          "^"

%token STAR         "*"
%token SLASH        "/"
%token PERCENT      "%"
%token TILDESLASH   "~/"
%token HATSLASH     "^/"
%token TILDEPERCENT "~%"
%token HATPERCENT   "^%"
%token SLASHPERCENT "/%"
%token AMP          "&"

%token TILDE        "~"
%token DOT          "."

%token LPAREN       "("
%token RPAREN       ")"
%token LBRACKET     "["
%token RBRACKET     "]"
%token LBRACE       "{"
%token RBRACE       "}"
%token SEMICOLON    ";"
%token COMMA        ","
%token ARROW        "->"

%token EOF

%left QUESTION COLON
%left EQEQ NEQ LT GT LEQ GEQ
%left PLUS MINUS PIPE HAT
%left STAR SLASH PERCENT TILDESLASH HATSLASH SLASHPERCENT
%left AMP
%left TILDE
%right EQ PLUS_EQ MINUS_EQ STAR_EQ SLASH_EQ TILDESLASH_EQ HATSLASH_EQ
%right PERCENT_EQ TILDEPERCENT_EQ HATPERCENT_EQ
%right LTLT_EQ GTGT_EQ TILDEGTGT_EQ HATGTGT_EQ
%right AMP_EQ PIPE_EQ HAT_EQ

%nonassoc below_CALL
%nonassoc CALL
%nonassoc DOT

%start translation_unit
%type <Ast_raw.translation_unit> translation_unit

%%

translation_unit:
  | list(top_level_item) EOF
    { { filename = $startpos.Lexing.pos_fname
      ; items = $1 } }

top_level_item:
  | function_definition            { ItemFuncDef($1) }
  | function_declaration SEMICOLON { ItemFuncDecl($1) }
  | global_var_declarations        { ItemGlobalVarDecls($1) }
  | compiler_directive             { ItemCompilerDirective($1) }
  | constant_declarations          { ItemConstDecls($1) }

compiler_directive:
  | INCLUDE string_literal_simple SEMICOLON  { DirectiveInclude($2) }
  | PRAGMA pragma_version SEMICOLON { DirectivePragma($2) }
  | PRAGMA ALLOW_POST_MODIFICATION SEMICOLON
    { DirectivePragma( { pr_key = "allow-post-modification";
                         pr_value = None } ) }
  | PRAGMA COMPUTE_ASM_LTR SEMICOLON
    { DirectivePragma( { pr_key = "compute-ast-ltr";
                         pr_value = None } ) }
pragma_version:
  | IDENTIFIER option(pragma_value) { { pr_key = $1; pr_value = $2 } }
%inline pragma_value:
  | binop NUMBER_LITERAL DOT NUMBER_LITERAL DOT NUMBER_LITERAL
    { Printf.sprintf("%s%s.%s.%s") (string_of_binop $1) $2 $4 $6 }
  | EQ NUMBER_LITERAL DOT NUMBER_LITERAL DOT NUMBER_LITERAL
    { Printf.sprintf("=%s.%s.%s") $2 $4 $6 }

global_var_declarations:
  | GLOBAL separated_nonempty_list(COMMA, identifier) SEMICOLON;
    { List.map (fun gvd_name -> { gvd_type = None; gvd_name }) $2 }
  | GLOBAL separated_nonempty_list(COMMA, global_var_declaration) SEMICOLON
    { $2 }
global_var_declaration:
  | expr_fd identifier { { gvd_type = Some($1); gvd_name = $2 } }

constant_declarations:
  | CONST separated_nonempty_list(COMMA, constant_declaration) SEMICOLON { $2 }

constant_declaration:
  | identifier identifier EQ expr
    { { cd_type = Some($1); cd_name = $2; cd_value = $4 } }
  | identifier EQ expr
    { { cd_type = None; cd_name = $1; cd_value = $3 } }

function_declaration:
  | option(type_variables) expr_fd fun_name delimited(LPAREN, option(separated_nonempty_list(COMMA, parameter_declaration)), RPAREN) list(specifier)
  { { fd_type_variables = $1;
      fd_return_type = $2;
      fd_name = $3;
      fd_arguments = Option.value $4 ~default:([]);
      fd_specifiers = $5; } }

%inline fun_name:
  | identifier { $1 }
  | TILDE IDENTIFIER { save (ExprId("~"^$2)) $loc }

function_definition:
  | function_declaration function_body
  { { fd_signature = $1;
      fd_body = $2; } }

type_variables:
  | FORALL separated_nonempty_list(COMMA, type_variable) ARROW { $2 }
type_variable:
  | IDENTIFIER      { $1 }
  | expr IDENTIFIER { $2 }

specifier:
  | IMPURE                  { Impure }
  | INLINE                  { Inline }
  | INLINE_REF              { InlineRef }
  | METHOD_ID option(expr)  { MethodId($2) }

parameter_declaration:
  | expr_fd identifier { { pd_type = $1; pd_name = Some $2 } }
  | expr_fd { { pd_type = $1; pd_name = None } }

function_body:
  | block_stmt { Block $1 }
  | asm_function_body { Asm $1 }

asm_function_body:
  | ASM asm_specifier asm_instruction_list SEMICOLON
    { ($2, List.rev $3) }

asm_specifier:
  | LPAREN option(asm_params) RPAREN
    { { as_params = $2; as_returns = None } }
  | LPAREN option(asm_params) ARROW option(asm_returns) RPAREN
    { { as_params = $2; as_returns = $4 } }
  | /* empty */
    { { as_params = None; as_returns = None } }

asm_params:
  | IDENTIFIER { [$1] }
  | asm_params IDENTIFIER { $2 :: $1 }

asm_returns:
  | NUMBER_LITERAL { [$1] }
  | asm_returns NUMBER_LITERAL { $2 :: $1 }

asm_instruction_list:
  | asm_instruction { [$1] }
  | asm_instruction_list asm_instruction { $2 :: $1 }

asm_instruction:
  | string_literal_simple { $1 }

block_stmt:
  | LBRACE list(stmt) RBRACE
    { save (StmtBlock $2) $loc }

stmt:
  | RETURN expr SEMICOLON
    { save (StmtReturn $2) $loc }
  | block_stmt { $1 }
  | SEMICOLON
    { save (StmtEmpty) $loc }
  | REPEAT expr block_stmt
    { save (StmtRepeat($2, $3)) $loc }
  | if_stmt { save $1 $loc }
  | DO block_stmt UNTIL expr
    { save (StmtDo($2, $4)) $loc }
  | WHILE expr block_stmt
    { save (StmtWhile($2, $3)) $loc }
  | TRY block_stmt CATCH option(expr) block_stmt
    { save (StmtTryCatch($2, $4, $5)) $loc }
  | expr SEMICOLON
    { save (StmtExpr $1) $loc }

%inline if_stmt:
  | IF expr block_stmt list(elseif_clause) option(else_clause) { StmtIf {
      if_condition = $2;
      if_body = $3;
      if_elseif = $4;
      if_else = $5;
      if_not = false;
    } }
  | IFNOT expr block_stmt list(elseif_clause) option(else_clause) { StmtIf {
      if_condition = $2;
      if_body = $3;
      if_elseif = $4;
      if_else = $5;
      if_not = true;
    } }
%inline else_clause:
  | ELSE block_stmt { $2 }
%inline elseif_clause:
  | ELSEIF expr block_stmt
    { { elseif_cond = $2;
        elseif_body = $3;
        elseif_not = false; } }
  | ELSEIFNOT expr block_stmt
    { { elseif_cond = $2;
        elseif_body = $3;
        elseif_not = true; } }

expr:
  | expr_noassign { $1 }
  | expr_noassign assignment_operator expr %prec below_CALL
    { save (ExprAssign($1, $2, $3)) $loc }
expr_noassign:
  | method_call { save $1 $loc }
  | identifier delimited(LPAREN, argument_list, RPAREN) %prec CALL
    { save (ExprFunCall($1, $2)) $loc }
  | expr_noassign QUESTION expr_noassign COLON expr_noassign %prec QUESTION
    { save (ExprCond($1, $3, $5)) $loc }
  | expr_noassign binop expr_noassign %prec PLUS
    { save (ExprBinOp($1, $2, $3)) $loc }
  | unop expr_noassign %prec TILDE
    { save (ExprUnOp($1, $2)) $loc }
  | var_decl { $1 }
  | identifier { $1 }
  | NUMBER_LITERAL
    { save (ExprNum($1)) $loc }
  | string_literal
    { save (ExprString($1)) $loc }
  | TRUE
    { save (ExprBool(true)) $loc }
  | FALSE
    { save (ExprBool(false)) $loc }
  | LPAREN RPAREN
    { save (ExprUnit) $loc }
  | UNDERSCORE
    { save (ExprUnderscore) $loc }
  | LBRACKET separated_nonempty_list(COMMA, expr) RBRACKET
    { save (ExprTuple $2) $loc }
  | LPAREN separated_nonempty_list(COMMA, expr) RPAREN
    { save (ExprTensor $2) $loc }
  | delimited (LPAREN, expr, RPAREN) { $1 }
(* An abomination appearing in the function definition *)
expr_fd:
  | LPAREN RPAREN
    { save (ExprUnit) $loc }
  | identifier { $1 }
  | UNDERSCORE
    { save (ExprUnderscore) $loc }
  | LBRACKET separated_nonempty_list(COMMA, expr_fd) RBRACKET
    { save (ExprTuple $2) $loc }
  | LPAREN separated_nonempty_list(COMMA, expr_fd) RPAREN
    { save (ExprTensor $2) $loc }
  | delimited (LPAREN, expr_fd, RPAREN) { $1 }
  (* | identifier ARROW expr_fd { FunctionType($1, $3) } *)

%inline identifier:
  IDENTIFIER { save (ExprId($1)) $loc }

%inline string_literal:
  | string_literal_simple { $1 }
  | STRING_LITERAL_SUFFIX
    {
      let (str_val, suffix) = $1 in
      { str_val; str_ty = Some(string_type_of_char suffix) } }
%inline string_literal_simple:
  | STRING_LITERAL
    { { str_val = $1; str_ty = None } }

assignment_operator:
  | EQ                { OpAssign }
  | PLUS_EQ           { OpAssignAdd }
  | MINUS_EQ          { OpAssignSub }
  | STAR_EQ           { OpAssignMul }
  | SLASH_EQ          { OpAssignDivFloor }
  | TILDESLASH_EQ     { OpAssignDivRound }
  | HATSLASH_EQ       { OpAssignDivCeil }
  | PERCENT_EQ        { OpAssignMod }
  | TILDEPERCENT_EQ   { OpAssignModRound }
  | HATPERCENT_EQ     { OpAssignModCeil }
  | LTLT_EQ           { OpAssignShl }
  | GTGT_EQ           { OpAssignShr }
  | TILDEGTGT_EQ      { OpAssignShrRound }
  | HATGTGT_EQ        { OpAssignShrCeil }
  | AMP_EQ            { OpAssignBitAnd }
  | PIPE_EQ           { OpAssignBitOr }
  | HAT_EQ            { OpAssignBitXor }

binop:
  | "=="  { OpEq        }
  | "<"   { OpLt        }
  | ">"   { OpGt        }
  | "<="  { OpLeq       }
  | ">="  { OpGeq       }
  | "!="  { OpNeq       }
  | "<<>" { OpCmp       }
  | "<<"  { OpShl       }
  | ">>"  { OpShr       }
  | "~>>" { OpShrRound  }
  | "^>>" { OpShrCeil   }
  | "-"   { OpSub       }
  | "+"   { OpAdd       }
  | "|"   { OpBitOr     }
  | "^"   { OpBitXor    }
  | "*"   { OpMul       }
  | "/"   { OpDivFloor  }
  | "%"   { OpMod       }
  | "~/"  { OpDivRound  }
  | "^/"  { OpDivCeil   }
  | "~%"  { OpModRound  }
  | "^%"  { OpModCeil   }
  | "/%"  { OpDivMod    }
  | "&"   { OpBitAnd    }

unop:
  | MINUS { UMinus }
  | TILDE { UTilde }

%inline method_call:
  | expr_noassign TILDE identifier LPAREN RPAREN %prec CALL
    { ExprMethodCall($1, $3, []) }
  | expr_noassign DOT   identifier LPAREN RPAREN %prec CALL
    { ExprMethodCall($1, $3, []) }
  | expr_noassign TILDE identifier LPAREN argument_list RPAREN %prec CALL
    { ExprMethodCall($1, $3, $5) }
  | expr_noassign DOT   identifier LPAREN argument_list RPAREN %prec CALL
    { ExprMethodCall($1, $3, $5) }

(* lhs of a variable declaration could be present by any imaginable garbage:
  ```
  // "valid" variable declarations:
  int x = <expr>;                     // id + id
  var x = <expr>;                     // id + id
  (int, int) p = <expr>;              // tensor + id
  (int, var) p = <expr>;              // tensor + id
  (int, int, int) (x, y, z) = <expr>; // tensor + tensor
  var (x, y, z) = <expr>;             // id + tensor
  var [x, y, z] = <expr>;             // id + tuple
  // not variable declarations:
  [int, int, int] [x, y, z] = <expr>; // tuple + tuple
  (int x, int y, int z) = <expr>;     // tensor of vardecls
  [int x, int y, int z] = <expr>;     // tuple of vardecls
  (int x = 1, int y = 2, int z = 3);  // tensor of assigns
  ```
  Obviously, it can appear *everywhere* in the source code and enables to use
  *any* expression in the variable declaration because SOMEONE HATES
  MAINTAINABLE PARSERS and wants to make future generations suffer through
  ambiguous grammars.
*)
%inline var_decl:
  | expr_lhs expr_lhs { save (ExprVarDecl($1, $2)) $loc }
%inline expr_lhs:
  | identifier { $1 }
  | LBRACKET separated_nonempty_list(COMMA, expr) RBRACKET
    { save (ExprTuple $2) $loc }
  | LPAREN separated_nonempty_list(COMMA, expr) RPAREN
    { save (ExprTensor $2) $loc }

argument_list:
  | separated_list(COMMA, expr) { $1 }
