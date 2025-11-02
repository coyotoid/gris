exception Type_error of string
exception Unknown_word of string

type[@ocaml.unboxed] type_id = TypeId of int

let int_of_type_id (TypeId id) = id

type ty = TyVar of type_id | TyCon of string

let ty_int = TyCon "Int"
let ty_str = TyCon "Str"

type eff =
  ty list * ty list (* stored top-first: (a b c -- a) -> ([c; b; a], [a]) *)

type ty_env = { vars : (type_id, ty) Hashtbl.t; mk_id : unit -> type_id }

let make_ty_env () : ty_env =
  {
    vars = Hashtbl.create 16;
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

let bind env v t =
  let t = substitute_ty env t in
  if substitute_ty env (TyVar v) = t then ()
  else if occurs env v t then raise (Type_error "occurrence check failed")
  else Hashtbl.add env.vars v t

let rec unify env t1 t2 =
  let t1 = substitute_ty env t1 in
  let t2 = substitute_ty env t2 in
  let commute () = unify env t2 t1 in
  match (t1, t2) with
  | TyCon c1, TyCon c2 when String.equal c1 c2 -> ()
  | TyVar (TypeId v1), TyVar (TypeId v2) when Int.equal v1 v2 -> ()
  | TyVar v, t -> bind env v t
  | _, TyVar _ -> commute ()
  | _ -> raise (Type_error "type mismatch")

let unify_list env a b =
  match List.compare_lengths a b with
  | 0 -> List.iter2 (unify env) a b
  | _ -> raise (Type_error "type mismatch")

let instantiate_effect env ((takes, leaves) : eff) : eff =
  let map = Hashtbl.create 8 in
  let fresh v =
    match Hashtbl.find_opt map v with
    | Some id -> id
    | None ->
        let id = env.mk_id () in
        Hashtbl.add map v id;
        id
  in
  let instantiate =
    List.map (function TyCon _ as t -> t | TyVar v -> TyVar (fresh v))
  in
  (instantiate takes, instantiate leaves)

let effect_of_primitive : Interpreter.prim -> eff =
  let open Interpreter in
  function
  | PrimDup (* a -- a a *) ->
      ([ TyVar (TypeId 0) ], [ TyVar (TypeId 0); TyVar (TypeId 0) ])
  | PrimDrop (* a -- *) -> ([ TyVar (TypeId 0) ], [])
  | PrimSwap (* a b -- b a *) ->
      ( [ TyVar (TypeId 1); TyVar (TypeId 0) ],
        [ TyVar (TypeId 0); TyVar (TypeId 1) ] )
  | PrimBury (* a b c -- c a b *) ->
      ( [ TyVar (TypeId 2); TyVar (TypeId 1); TyVar (TypeId 0) ],
        [ TyVar (TypeId 1); TyVar (TypeId 0); TyVar (TypeId 2) ] )
  | PrimUnbury (* a b c -- b c a *) ->
      ( [ TyVar (TypeId 2); TyVar (TypeId 1); TyVar (TypeId 0) ],
        [ TyVar (TypeId 0); TyVar (TypeId 2); TyVar (TypeId 1) ] )
  | PrimAdd | PrimMul (* Int Int -- Int *) -> ([ ty_int; ty_int ], [ ty_int ])
  | PrimConcat (* Str Str -- Str *) -> ([ ty_str; ty_str ], [ ty_str ])
  | PrimPrint (* Str -- *) -> ([ ty_str ], [])
  | PrimShow (* Int -- *) -> ([ ty_int ], [ ty_str ])

let infer tree =
  let env = make_ty_env () in
  let stack = Stack.create () in
  let vars = Stack.create () in

  let ensure_args n =
    let deficit = n - Stack.length stack in
    if deficit > 0 then
      for _ = 1 to deficit do
        let id = env.mk_id () in
        Stack.push (TyVar id) stack;
        Stack.push id vars
      done
  in

  let take_n n = Stack.to_seq stack |> Seq.take n |> List.of_seq in

  let rec aux =
    let open Parsing in
    function
    | Parsing.TGroup xs -> List.iter aux xs
    | Parsing.TAtom x -> (
        match x with
        | AInt _ -> Stack.push ty_int stack
        | AString _ -> Stack.push ty_str stack
        | AWord w -> (
            match Interpreter.prim_of_string_opt w with
            | None -> raise (Unknown_word w)
            | Some prim ->
                let takes, leaves =
                  instantiate_effect env (effect_of_primitive prim)
                in
                let arity = List.length takes in
                ensure_args arity;
                unify_list env takes (take_n arity);
                for _ = 1 to arity do
                  Stack.drop stack
                done;
                let takes' = List.rev_map (substitute_ty env) leaves in
                Stack.add_seq stack (List.to_seq takes')))
  in
  aux tree;
  let inputs =
    Stack.to_seq vars
    |> Seq.map (fun var -> substitute_ty env (TyVar var))
    |> List.of_seq
  in
  let outputs =
    List.map (substitute_ty env) (Stack.to_seq stack |> List.of_seq)
  in
  (inputs, outputs)
