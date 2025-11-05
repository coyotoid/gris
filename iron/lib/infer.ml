exception Type_error of string
exception Unknown_word of string

type[@ocaml.unboxed] type_id = TypeId of int
type ty = TyVar of type_id | TyCon of string

let string_of_ty = function
  | TyCon n -> n
  | TyVar (TypeId v) -> Printf.sprintf "'%d" v

let ty_int = TyCon "Int"
let ty_str = TyCon "Str"

type eff =
  ty list * ty list (* stored top-first: (a b c -- a) -> ([c; b; a], [a]) *)

let string_of_eff (takes, leaves) =
  let show_list = function
    | [] -> "[]"
    | xs -> "[" ^ String.concat " " (List.rev_map string_of_ty xs) ^ "]"
  in
  show_list takes ^ " -> " ^ show_list leaves

let string_of_eff_pretty (takes, leaves) =
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

type ty_env = {
  vars : (type_id, ty) Hashtbl.t;
  sigs : (string, eff) Hashtbl.t;
  mk_id : unit -> type_id;
}

let make_ty_env () : ty_env =
  {
    vars = Hashtbl.create 256;
    sigs = Hashtbl.create 16;
    mk_id =
      (let state = ref 0 in
       fun () ->
         let id = !state in
         incr state;
         TypeId id);
  }

let rec substitute_ty env = function
  | TyCon _ as t -> t
  | TyVar v as t -> (
      match Hashtbl.find_opt env.vars v with
      | None -> t
      | Some t' -> substitute_ty env t')

let occurs env (TypeId v) t =
  match substitute_ty env t with
  | TyVar (TypeId var') -> Int.equal v var'
  | _ -> false

let bind env (TypeId id as v) t =
  let t = substitute_ty env t in
  if substitute_ty env (TyVar v) = t then ()
  else if occurs env v t then
    raise
      (Type_error ("occurrence check failed for type var " ^ string_of_int id))
  else Hashtbl.add env.vars v t

let unify env t1 t2 =
  let t1 = substitute_ty env t1 in
  let t2 = substitute_ty env t2 in
  match (t1, t2) with
  | TyCon c1, TyCon c2 when String.equal c1 c2 -> ()
  | TyVar (TypeId v1), TyVar (TypeId v2) when Int.equal v1 v2 -> ()
  | TyVar v, t | t, TyVar v -> bind env v t
  | _ ->
      raise
        (Type_error
           (Printf.sprintf "type mismatch: %s vs. %s" (string_of_ty t1)
              (string_of_ty t2)))

let unify_list env a b =
  match List.compare_lengths a b with
  | 0 -> List.iter2 (unify env) a b
  | _ -> raise (Type_error "type list mismatch (should not happen?)")

let instantiate_effect env (takes, leaves) =
  let map = Hashtbl.create 16 in
  let fresh v =
    match Hashtbl.find_opt map v with
    | Some id -> id
    | None ->
        let id = env.mk_id () in
        Hashtbl.add map v id;
        id
  in
  let aux = List.map (function TyVar v -> TyVar (fresh v) | t -> t) in
  (aux takes, aux leaves)

let prim_effect name : eff option =
  match name with
  | "dup" -> Some ([ TyVar (TypeId 0) ], [ TyVar (TypeId 0); TyVar (TypeId 0) ])
  | "drop" -> Some ([ TyVar (TypeId 0) ], [])
  | "swap" ->
      Some
        ( [ TyVar (TypeId 1); TyVar (TypeId 0) ],
          [ TyVar (TypeId 0); TyVar (TypeId 1) ] )
  | "bury" ->
      Some
        ( [ TyVar (TypeId 2); TyVar (TypeId 1); TyVar (TypeId 0) ],
          [ TyVar (TypeId 1); TyVar (TypeId 0); TyVar (TypeId 2) ] )
  | "unbury" ->
      Some
        ( [ TyVar (TypeId 2); TyVar (TypeId 1); TyVar (TypeId 0) ],
          [ TyVar (TypeId 0); TyVar (TypeId 2); TyVar (TypeId 1) ] )
  | "+" -> Some ([ ty_int; ty_int ], [ ty_int ])
  | "*" -> Some ([ ty_int; ty_int ], [ ty_int ])
  | "^" -> Some ([ ty_str; ty_str ], [ ty_str ])
  | "print" -> Some ([ ty_str ], [])
  | "show" -> Some ([ ty_int ], [ ty_str ])
  | _ -> None

let rec infer env tree =
  let stack = ref [] in
  let vars = ref [] in

  let ensure_args n =
    let deficit = n - List.length !stack in
    if deficit > 0 then
      for _ = 1 to deficit do
        let id = env.mk_id () in
        stack := !stack @ [ TyVar id ];
        vars := !vars @ [ id ]
      done
  in

  let run_word eff =
    let takes, leaves = instantiate_effect env eff in
    let arity = List.length takes in
    ensure_args arity;
    unify_list env takes (List.take arity !stack);
    for _ = 1 to arity do
      stack := List.tl !stack
    done;
    let leaves' = List.map (substitute_ty env) leaves in
    stack := leaves' @ !stack
  in

  let rec aux' =
    let open Parsing in
    function
    | EAtom (AWord "def") :: EAtom (AWord name) :: EGroup defn :: xs ->
        let takes, leaves = infer env (EGroup defn) in
        Hashtbl.add env.sigs name (takes, leaves);
        aux' xs
    | EAtom (AInt _) :: xs ->
        stack := ty_int :: !stack;
        aux' xs
    | EAtom (AString _) :: xs ->
        stack := ty_str :: !stack;
        aux' xs
    | EAtom (AWord w) :: xs ->
        (match prim_effect w with
        | Some eff -> run_word eff
        | None -> (
            match Hashtbl.find_opt env.sigs w with
            | Some eff -> run_word eff
            | None -> raise (Unknown_word w)));
        aux' xs
    | EGroup x :: xs ->
        aux' x;
        aux' xs
    | [] -> ()
  in
  aux' [ tree ];
  let inputs = List.map (fun var -> substitute_ty env (TyVar var)) !vars in
  let outputs = List.map (substitute_ty env) !stack in
  (inputs, outputs)

(* combinator test harness *)
let%expect_test "nip (swap drop)" =
  let tree = Lexing.lex "swap drop" |> Parsing.parse in
  let eff = infer (make_ty_env ()) tree in
  print_endline (string_of_eff_pretty eff);
  [%expect {| [a b] -> [b] |}]

let%expect_test "over (swap dup bury)" =
  let tree = Lexing.lex "swap dup bury" |> Parsing.parse in
  let eff = infer (make_ty_env ()) tree in
  print_endline (string_of_eff_pretty eff);
  [%expect {| [a b] -> [a b a] |}]

let%expect_test "tuck (dup bury)" =
  let tree = Lexing.lex "dup bury" |> Parsing.parse in
  let eff = infer (make_ty_env ()) tree in
  print_endline (string_of_eff_pretty eff);
  [%expect {| [a b] -> [b a b] |}]
