%{
open Ast
%}

%token EOF
%token <int> INT
%token <string> STRING
%token <string> WORD
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACKET
%token RIGHT_BRACKET

%token DEF

%type <Ast.expr> top_expr
%start top_expr

%%

let term := word | lit | group | quote

top_expr:
  | d=def; EOF { d }
  | e=expr; EOF { e }
  | d=def; e=top_expr; { ECat (d, e) }
  | EOF { EId }

def:
  | DEF; n=WORD; e=term { EDef (n, e) }

expr:
  | t=term { t }
  | t=term; e=expr
    { match e with
      | EId -> t
      | _ -> ECat (t, e) }
  ;

word:
  | w=WORD { ECall w }
  ;
lit:
  | i=INT    { EPush (AInt i) }
  | s=STRING { EPush (AStr s) }
  ;
group:
  | LEFT_PAREN; e=expr; RIGHT_PAREN { EGroup e }
  | LEFT_PAREN;         RIGHT_PAREN { EGroup EId }
  ;
quote:
  | LEFT_BRACKET; e=expr; RIGHT_BRACKET { EQuote e }
  | LEFT_BRACKET;         RIGHT_BRACKET { EQuote EId }
  ;
