exception Type_error of string

type[@ocaml.unboxed] type_id = TypeId of int
type ty = TyVar of type_id | TyCon of string

let ty_int = TyCon "Int"
let ty_str = TyCon "Str"
let ty_bool = TyCon "Bool"

let string_of_ty = function
  | TyCon n -> n
  | TyVar (TypeId v) -> Printf.sprintf "'%d" v

type eff =
  ty list * ty list (* stored top-first: (a b c -- a) -> ([c; b; a], [a]) *)

let string_of_eff (takes, leaves) =
  let collect_vars acc tys =
    let aux acc = function
      | TyCon _ -> acc
      | TyVar i -> if List.mem i acc then acc else i :: acc
    in
    List.fold_left aux acc tys
  in
  let name_of_ix i =
    let base = Char.chr (Char.code 'a' + (i mod 26)) in
    if i < 26 then String.make 1 base else Printf.sprintf "%c%d" base (i / 26)
  in
  let ids = collect_vars (collect_vars [] takes) leaves in
  let vars = Hashtbl.create (List.length ids) in
  List.iteri (fun idx id -> Hashtbl.add vars id (name_of_ix idx)) ids;
  let string_of_ty_pretty = function
    | TyVar id -> Hashtbl.find vars id
    | t -> string_of_ty t
  in
  let show_list = function
    | [] -> "[]"
    | xs -> "[" ^ String.concat " " (List.rev_map string_of_ty_pretty xs) ^ "]"
  in
  show_list takes ^ " -> " ^ show_list leaves
