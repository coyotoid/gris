type literal = LInt of int | LStr of string | LBool of bool

let string_of_literal = function
  | LInt i -> string_of_int i
  | LBool b -> string_of_bool b
  | LStr s -> "\"" ^ String.escaped s ^ "\""

type expr =
  | EId
  | ECat of expr * expr
  | EPush of literal
  | ECall of string
  | EDef of string * expr
  | EGroup of expr
  | EBlock of expr

let rec string_of_expr = function
  | EId -> ""
  | ECat (e1, e2) -> string_of_expr e1 ^ " " ^ string_of_expr e2
  | EPush a -> string_of_literal a
  | ECall w -> w
  | EDef (n, e) -> Printf.sprintf "def %s = %s in" n (string_of_expr e)
  | EGroup e -> "(" ^ string_of_expr e ^ ")"
  | EBlock e -> "{" ^ string_of_expr e ^ "}"
