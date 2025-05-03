{
open Parser
open Lexing

exception LexingError of string

let incr_linenum lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { pos with
    Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
    Lexing.pos_bol = pos.Lexing.pos_cnum;
  }
}

rule token = parse
  | '\n' { incr_linenum lexbuf; token lexbuf }
  | [' ' '\t' '\r' ] { token lexbuf }
  | ";;" [^ '\n' '\r']* { token lexbuf }
  | "//" [^ '\n' '\r']* { token lexbuf }
  | "/*" ( [^ '*' ] | '*' [^ '/' ] )* '*' '/' { token lexbuf }
  | "{-" { comment lexbuf; token lexbuf }
  | "#include" { INCLUDE }
  | "compute-asm-ltr" { COMPUTE_ASM_LTR }
  | "allow-post-modification" { ALLOW_POST_MODIFICATION }
  | "#pragma" { PRAGMA }
  | "asm" { ASM }
  | "forall" { FORALL }
  | "inline_ref" { INLINE_REF }
  | "inline" { INLINE }
  | "impure" { IMPURE }
  | "method_id" { METHOD_ID }
  | "global" { GLOBAL }
  | "const" { CONST }
  | "return" { RETURN }
  | "repeat" { REPEAT }
  | "ifnot" { IFNOT }
  | "if" { IF }
  | "elseifnot" { ELSEIFNOT }
  | "elseif" { ELSEIF }
  | "else" { ELSE }
  | "do" { DO }
  | "until" { UNTIL }
  | "while" { WHILE }
  | "try" { TRY }
  | "catch" { CATCH }
  | ";" { SEMICOLON }
  | "," { COMMA }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "=" { EQ }
  | "+=" { PLUS_EQ }
  | "-=" { MINUS_EQ }
  | "*=" { STAR_EQ }
  | "/=" { SLASH_EQ }
  | "~/=" { TILDESLASH_EQ }
  | "^/=" { HATSLASH_EQ }
  | "%=" { PERCENT_EQ }
  | "~%=" { TILDEPERCENT_EQ }
  | "^%=" { HATPERCENT_EQ }
  | "<<=" { LTLT_EQ }
  | ">>=" { GTGT_EQ }
  | "~>>=" { TILDEGTGT_EQ }
  | "^>>=" { HATGTGT_EQ }
  | "&=" { AMP_EQ }
  | "|=" { PIPE_EQ }
  | "^=" { HAT_EQ }
  | "==" { EQEQ }
  | "!=" { NEQ }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "<=>" { LTLTGT }
  | "<" { LT }
  | ">" { GT }
  | "<<" { LTLT }
  | ">>" { GTGT }
  | "~>>" { TILDEGTGT }
  | "^>>" { HATGTGT }
  | "->" { ARROW }
  | "-" { MINUS }
  | "+" { PLUS }
  | "|" { PIPE }
  | "^" { HAT }
  | "*" { STAR }
  | "/" { SLASH }
  | "%" { PERCENT }
  | "~/" { TILDESLASH }
  | "^/" { HATSLASH }
  | "~%" { TILDEPERCENT }
  | "^%" { HATPERCENT }
  | "/%" { SLASHPERCENT }
  | "&" { AMP }
  | "~" { TILDE }
  | "." { DOT }
  | "?" { QUESTION }
  | ":" { COLON }
  | ['0'-'9']+ as num { NUMBER_LITERAL(num) }
  | "0x" ['0'-'9''a'-'f''A'-'F']+ as num { NUMBER_LITERAL(num) }
  | "\"\"\"" { STRING_LITERAL(multiline_string [] lexbuf |> String.concat "\n") }
  | "\"" [^ '"' '\\']* "\"" ['H' 'h' 'c' 'u' 's' 'a']? as str {
      let len = String.length str in
      if len >= 2 && (str.[len - 1] = 'H' || str.[len - 1] = 'h' || str.[len - 1] = 'c' || str.[len - 1] = 'u' || str.[len - 1] = 's' || str.[len - 1] = 'a') then
        STRING_LITERAL_SUFFIX(String.sub str 0 (len - 1), str.[len - 1])
      else
        STRING_LITERAL(str)
    }

  (* Any imaginable garbage could be used as identifier *)
  | "`" [^ '`' '\\']+ "`" as id { IDENTIFIER(id) }
  | ['a'-'z' 'A'-'Z' '0'-'9' '_' '$' '%']+
    [^ ' ' '\t' '\n' '+' '-' '*' '/' '%' ',' ';' '(' ')' '{' '}' '[' ']' '=' '<' '>' '|' '^' '.' '~']* as id
    { if id = "_" then UNDERSCORE else IDENTIFIER(id) }

  | eof { EOF }
  | _ as c { raise (LexingError (Printf.sprintf "Unrecognized character '%c'" c)) }

and comment = parse
  | "-}" { () }
  | "{-" { comment lexbuf; comment lexbuf }
  | '\n' { incr_linenum lexbuf; comment lexbuf }
  | _ { comment lexbuf }

and multiline_string acc = parse
  | "\"\"\"" { List.rev acc }
  | '\n' {
      incr_linenum lexbuf;
      multiline_string (("\n" :: acc)) lexbuf
    }
  | [^'"' '\n']+ as str { multiline_string (str :: acc) lexbuf }
  | '"' '"'? [^'"' '\n']* as str { multiline_string (str :: acc) lexbuf }
  | eof { raise (LexingError "Unterminated multiline string") }
