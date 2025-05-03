open Core

let parse_raw (filename : string) : (Ast_raw.program, string) result =
  let chan = In_channel.create filename in
  let lexbuf = Lexing.from_channel chan in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  try
    let result = Parser.translation_unit Lexer.token lexbuf in
    In_channel.close chan;
    Ok(result)
  with
  | Lexer.LexingError msg ->
    In_channel.close chan;
    let pos = lexbuf.lex_curr_p in
    let line = pos.pos_lnum in
    let cnum = pos.pos_cnum - pos.pos_bol + 1 in
    Error (Printf.sprintf "Lexing error at %s:%d:%d: %s"
             filename line cnum msg);
  | Parser.Error ->
    In_channel.close chan;
    let pos = lexbuf.lex_curr_p in
    let line = pos.pos_lnum in
    let cnum = pos.pos_cnum - pos.pos_bol + 1 in
    Error (Printf.sprintf "Parsing error at %s:%d:%d"
             filename line cnum)

let parse (filename : string): (Ast.program, string) result =
  parse_raw filename |> Result.map ~f:Unfunc.unfunc_raw_ast
