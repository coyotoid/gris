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

let compose_type_env e1 e2 =
  let e1' =
    Hashtbl.to_seq e1.vars
    |> Seq.map (fun (v, t) -> (v, substitute_ty e2 t))
    |> Seq.filter (fun (v, _) -> not (Hashtbl.mem e2.vars v))
  in
  Hashtbl.add_seq e2.vars e1'

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

let rec infer env tree =
  let stack = Stack.create () in
  let vars = Stack.create () in

  let prims =
    [
      ("dup", ([ TyVar (TypeId 0) ], [ TyVar (TypeId 0); TyVar (TypeId 0) ]));
      ("drop", ([ TyVar (TypeId 0) ], []));
      ( "swap",
        ( [ TyVar (TypeId 1); TyVar (TypeId 0) ],
          [ TyVar (TypeId 0); TyVar (TypeId 1) ] ) );
      ( "bury",
        ( [ TyVar (TypeId 2); TyVar (TypeId 1); TyVar (TypeId 0) ],
          [ TyVar (TypeId 1); TyVar (TypeId 0); TyVar (TypeId 2) ] ) );
      ( "unbury",
        ( [ TyVar (TypeId 2); TyVar (TypeId 1); TyVar (TypeId 0) ],
          [ TyVar (TypeId 0); TyVar (TypeId 2); TyVar (TypeId 1) ] ) );
      ("+", ([ ty_int; ty_int ], [ ty_int ]));
      ("*", ([ ty_int; ty_int ], [ ty_int ]));
      ("^", ([ ty_str; ty_str ], [ ty_str ]));
      ("print", ([ ty_str ], []));
      ("show", ([ ty_int ], [ ty_str ]));
    ]
  in
  Hashtbl.add_seq env.sigs (List.to_seq prims);

  let ensure_args n =
    let deficit = n - Stack.length stack in
    if deficit > 0 then
      for _ = 1 to deficit do
        let id = env.mk_id () in
        Stack.push (TyVar id) stack;
        Stack.push id vars
      done
  in

  let rec aux' =
    let open Parsing in
    function
    | TAtom (AWord "def") :: TAtom (AWord name) :: TGroup defn :: xs ->
        let takes, leaves = infer env (TGroup defn) in
        Hashtbl.add env.sigs name (takes, leaves);
        aux' xs
    | TAtom (AInt _) :: xs ->
        Stack.push ty_int stack;
        aux' xs
    | TAtom (AString _) :: xs ->
        Stack.push ty_str stack;
        aux' xs
    | TAtom (AWord w) :: xs ->
        (match Hashtbl.find_opt env.sigs w with
        | None -> raise (Unknown_word w)
        | Some eff ->
            let takes, leaves = instantiate_effect env eff in
            let arity = List.length takes in
            ensure_args arity;
            unify_list env takes
              (Stack.to_seq stack |> Seq.take arity |> List.of_seq);
            for _ = 1 to arity do
              Stack.drop stack
            done;
            let takes' = List.rev_map (substitute_ty env) leaves in
            Stack.add_seq stack (List.to_seq takes'));
        aux' xs
    | TGroup x :: xs ->
        aux' x;
        aux' xs
    | [] -> ()
  in
  aux' [ tree ];
  let inputs =
    Stack.to_seq vars
    |> Seq.map (fun var -> substitute_ty env (TyVar var))
    |> List.of_seq
  in
  let outputs =
    List.map (substitute_ty env) (Stack.to_seq stack |> List.of_seq)
  in
  (inputs, outputs)
