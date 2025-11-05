open Typing
open Primitive

exception Unknown_word of string

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

let rec infer env expr =
  let stack = CCDeque.create () in
  let vars = CCDeque.create () in

  let ensure_args n =
    let deficit = n - CCDeque.length stack in
    if deficit > 0 then
      for _ = 1 to deficit do
        let id = env.mk_id () in
        CCDeque.push_back stack (TyVar id);
        CCDeque.push_back vars id
      done
  in

  let infer_word eff =
    let takes, leaves = instantiate_effect env eff in
    let arity = List.length takes in
    ensure_args arity;
    let args =
      let rec collect acc n =
        if n = 0 then List.rev acc
        else collect (CCDeque.take_front stack :: acc) (n - 1)
      in
      collect [] arity
    in
    unify_list env takes args;
    let leaves' = List.rev_map (substitute_ty env) leaves in
    CCDeque.append_front ~into:stack (CCDeque.of_list leaves')
  in

  let rec aux =
    let open Ast in
    function
    | EId -> ()
    | ECat (e1, e2) ->
        aux e1;
        aux e2
    | EPush (AInt _) -> CCDeque.push_front stack ty_int
    | EPush (AStr _) -> CCDeque.push_front stack ty_str
    | ECall name -> (
        match primitive_of_string name with
        | Some prim -> infer_word (effect_of_primitive prim)
        | None -> (
            match Hashtbl.find_opt env.sigs name with
            | Some eff -> infer_word eff
            | None -> raise (Unknown_word name)))
    | EDef (name, def) -> Hashtbl.add env.sigs name (infer env def)
    | EGroup e -> aux e
    | EQuote _ -> failwith "unimplemented"
  in
  aux expr;
  let inputs =
    CCDeque.to_list vars |> List.map (fun var -> substitute_ty env (TyVar var))
  in
  let outputs = CCDeque.to_list stack |> List.map (substitute_ty env) in
  (inputs, outputs)
