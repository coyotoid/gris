%{
open Ast
%}

%token EOF

%token <string> WORD
%token <Ast.literal> LITERAL

%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET

%token DEF
%token IN

%type <Ast.expr> program

%start program

%%

let program :=
  | ~ = expr; EOF;               <>
  | ~ = define; EOF;             <>
  | ~ = define; IN; ~ = program; <ECat>
  | EOF;                         { EId }

let define :=
  | DEF; ~ = WORD; ~ = expr; <EDef>

let expr :=
  | ~ = term;           <>
  | ~ = term; ~ = expr; <ECat>

let term := word | literal | group | quote

let word :=
  | ~ = WORD; <ECall>

let literal := ~ = LITERAL; <EPush>

  let group :=
  | LPAREN;           RPAREN; { EGroup EId }
  | LPAREN; ~ = expr; RPAREN; <EGroup>

let quote :=
  | LBRACKET;           RBRACKET; { EBlock EId }
  | LBRACKET; ~ = expr; RBRACKET; <EBlock>
