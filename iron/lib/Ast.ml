type atom = AInt of int | AStr of string

type expr =
  | EId
  | ECat of expr * expr
  | EPush of atom
  | ECall of string
  | EDef of string * expr
  | EGroup of expr
  | EQuote of expr

let string_of_atom = function
  | AInt i -> string_of_int i
  | AStr s -> "\"" ^ String.escaped s ^ "\""

let rec string_of_expr = function
  | EId -> ""
  | ECat (e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
  | EPush a -> string_of_atom a
  | ECall w -> w
  | EDef (n, e) -> "def " ^ n ^ " " ^ string_of_expr e
  | EGroup e -> "(" ^ string_of_expr e ^ ")"
  | EQuote e -> "[" ^ string_of_expr e ^ "]"
