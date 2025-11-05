{ open Parser }

let digit = ['0'-'9']
let sign = ['-' '+']
let int = sign? digit+

let symchars = ['0'-'9' 'A'-'Z' 'a'-'z' '\x80'-'\xFF' '!' '$' '&' '*' '+' '-' '.' '/' ':' '<' '?' '=' '>' '@' '^' '_' '|' '\'']
let ident = symchars+

let white = [' ' '\t']+
let nl = '\r' | '\n' | "\r\n"

rule token = parse
  | white { token lexbuf }
  | '\n'  { Lexing.new_line lexbuf; token lexbuf }
  | "def" { DEF }
  | int   { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | ident { WORD (Lexing.lexeme lexbuf) }
  | '"'   { read_string (Buffer.create 31) lexbuf }
  | '('   { LEFT_PAREN }
  | ')'   { RIGHT_PAREN }
  | '['   { LEFT_BRACKET }
  | ']'   { RIGHT_BRACKET }
  | ';'   { token lexbuf }
  | '#'   { skip_line lexbuf }
  | _     { raise (Failure ("Character not allowed in source text: '" ^ Lexing.lexeme lexbuf ^ "'")) }
  | eof   { EOF }
and skip_line = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | eof  { EOF }
  | _    { skip_line lexbuf }
and read_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf; }
  | _ { raise (Failure ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (Failure "Unterminated string literal") }
