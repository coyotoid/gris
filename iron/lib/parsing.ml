open Lexing

type ('i, 'e) result = RNext of 'i list * 'e | RProduce of 'i list | RDone

exception Malformed_input

type atom = AInt of int | AString of string | AWord of string

let string_of_atom = function
  | AInt i -> string_of_int i
  | AString s -> "\"" ^ String.escaped s ^ "\""
  | AWord w -> w

type expr = EGroup of expr list | EAtom of atom

let rec string_of_expr ?(tl = true) xs =
  let show_group =
    Fun.compose (String.concat " ") (List.map (string_of_expr ~tl:false))
  in
  match xs with
  | EGroup xs when tl -> show_group xs
  | EGroup xs -> "(" ^ show_group xs ^ ")"
  | EAtom a -> string_of_atom a

let parse tokens =
  let parse_nesting =
    let rec aux n acc tk =
      if n < 0 then raise Malformed_input
      else
        match tk with
        | [] -> acc
        | LParen :: xs -> aux (n + 1) ((n + 1) :: acc) xs
        | RParen :: xs -> aux (n - 1) ((n - 1) :: acc) xs
        | _ :: xs -> aux n (n :: acc) xs
    in
    aux 0 []
  in
  let rec parse_group tokens =
    match parse_root tokens [] with next, expr -> RNext (next, EGroup expr)
  and parse_word tokens =
    match tokens with
    | [] -> RDone
    | LParen :: xs -> parse_group xs
    | RParen :: xs -> RProduce xs
    | String s :: xs -> RNext (xs, EAtom (AString s))
    | Int i :: xs -> RNext (xs, EAtom (AInt i))
    | Word w :: xs -> RNext (xs, EAtom (AWord w))
  and parse_root tokens acc =
    match parse_word tokens with
    | RDone -> (tokens, List.rev acc)
    | RProduce iter -> (iter, List.rev acc)
    | RNext (iter, sexp) -> parse_root iter (sexp :: acc)
  in
  match parse_nesting tokens with
  | [] -> EGroup []
  | x :: _ when not (Int.equal x 0) -> raise Malformed_input
  | _ -> EGroup (snd (parse_root tokens []))
