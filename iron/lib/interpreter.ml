type value = VInt of int | VStr of string
type ty_r = RInt | RStr | RAny

exception Mismatch
exception Dirty
exception Unknown_word of string

let string_of_value = function
  | VInt i -> string_of_int i
  | VStr s -> "\"" ^ String.escaped s ^ "\""

type prim =
  | PrimAdd
  | PrimMul
  | PrimConcat
  | PrimSwap
  | PrimDup
  | PrimDrop
  | PrimBury
  | PrimUnbury
  | PrimShow
  | PrimPrint

type word_def = Infer.ty list * Infer.ty list * Parsing.expr
type thunk = ThPrim of prim | ThWord of word_def

type env = {
  data : value Stack.t;
  tenv : Infer.ty_env;
  defs : (string, word_def) Hashtbl.t;
  latent : thunk Stack.t Stack.t;
}

let make_env tenv =
  {
    data = Stack.create ();
    tenv;
    defs = Hashtbl.create 16;
    latent = Stack.of_seq (Seq.singleton (Stack.create ()));
  }

let prim_of_string_opt = function
  | "+" -> Some PrimAdd
  | "*" -> Some PrimMul
  | "^" -> Some PrimConcat
  | "swap" -> Some PrimSwap
  | "dup" -> Some PrimDup
  | "drop" -> Some PrimDrop
  | "bury" -> Some PrimBury
  | "unbury" -> Some PrimUnbury
  | "show" -> Some PrimShow
  | "print" -> Some PrimPrint
  | _ -> None

let prim_shape = function
  | PrimAdd | PrimMul -> ([ RInt; RInt ], [ RInt ])
  | PrimConcat -> ([ RStr; RStr ], [ RStr ])
  | PrimSwap -> ([ RAny; RAny ], [ RAny; RAny ])
  | PrimDrop -> ([ RAny ], [])
  | PrimDup -> ([ RAny ], [ RAny; RAny ])
  | PrimUnbury -> ([ RAny; RAny; RAny ], [ RAny; RAny; RAny ])
  | PrimBury -> ([ RAny; RAny; RAny ], [ RAny; RAny; RAny ])
  | PrimShow -> ([ RInt ], [ RStr ])
  | PrimPrint -> ([ RStr ], [])

let ty_of_value = function VInt _ -> RInt | VStr _ -> RStr

let unify a b =
  let aux a b =
    match (a, b) with
    | RInt, RInt -> true
    | RStr, RStr -> true
    | RAny, _ | _, RAny -> true
    | _, _ -> false
  in
  List.compare_lengths a b == 0 && List.for_all2 aux a b

let do_prim env =
  let arith_op op stk =
    let b = Stack.pop stk in
    let a = Stack.pop stk in
    match (a, b) with
    | VInt a, VInt b -> Stack.push (VInt (op a b)) stk
    | _ -> raise Mismatch
  in
  function
  | PrimAdd -> arith_op ( + ) env.data
  | PrimMul -> arith_op ( * ) env.data
  | PrimConcat -> (
      let b = Stack.pop env.data in
      let a = Stack.pop env.data in
      match (a, b) with
      | VStr a, VStr b -> Stack.push (VStr (a ^ b)) env.data
      | _ -> raise Mismatch)
  | PrimSwap ->
      let a = Stack.pop env.data in
      let b = Stack.pop env.data in
      Stack.push a env.data;
      Stack.push b env.data
  | PrimBury ->
      let c = Stack.pop env.data in
      let b = Stack.pop env.data in
      let a = Stack.pop env.data in
      Stack.push c env.data;
      Stack.push a env.data;
      Stack.push b env.data
  | PrimUnbury ->
      let c = Stack.pop env.data in
      let b = Stack.pop env.data in
      let a = Stack.pop env.data in
      Stack.push b env.data;
      Stack.push c env.data;
      Stack.push a env.data
  | PrimDup -> Stack.push (Stack.top env.data) env.data
  | PrimDrop -> Stack.drop env.data
  | PrimShow -> (
      match Stack.pop env.data with
      | VInt i -> Stack.push (VStr (string_of_int i)) env.data
      | _ -> raise Mismatch)
  | PrimPrint -> (
      match Stack.pop env.data with
      | VStr s -> print_endline s
      | _ -> raise Mismatch)

let unify_shape env (takes, _) =
  let shape_of_stk =
    Stack.to_seq env.data
    |> Seq.take (List.length takes)
    |> Seq.map ty_of_value |> List.of_seq
  in
  unify takes shape_of_stk

let reify_ty = function
  | Infer.TyCon "Int" -> RInt
  | Infer.TyCon "Str" -> RStr
  | Infer.TyVar _ -> RAny
  | t ->
      failwith
        (Printf.sprintf "type %s couldn't be reified" (Infer.string_of_ty t))

let rec latent_check env =
  let lstk = Stack.top env.latent in
  match Stack.top_opt lstk with
  | Some (ThPrim p) ->
      if unify_shape env (prim_shape p) then (
        Stack.drop lstk;
        do_prim env p)
  | Some (ThWord (takes, leaves, defn)) ->
      if unify_shape env (List.map reify_ty takes, List.map reify_ty leaves)
      then (
        Stack.drop lstk;
        interpret env defn)
  | None -> ()

and with_latent_scope env fn =
  let ls = Stack.create () in
  Stack.push ls env.latent;
  fn ();
  latent_check env;
  if Stack.length ls > 0 then raise Dirty else Stack.drop env.latent

and interpret env =
  let rec aux next =
    let open Parsing in
    match next with
    | EAtom (AInt i) :: xs ->
        Stack.push (VInt i) env.data;
        aux xs
    | EAtom (AString s) :: xs ->
        Stack.push (VStr s) env.data;
        aux xs
    | EAtom (AWord "def") :: EAtom (AWord name) :: (EGroup _ as defn) :: xs ->
        (match Hashtbl.find_opt env.tenv.sigs name with
        | Some (takes, leaves) -> Hashtbl.add env.defs name (takes, leaves, defn)
        | None -> failwith "should not happen");
        aux xs
    | EAtom (AWord w) :: xs ->
        (match prim_of_string_opt w with
        | Some prim ->
            if prim_shape prim |> unify_shape env then do_prim env prim
            else Stack.push (ThPrim prim) (Stack.top env.latent)
        | _ -> (
            match Hashtbl.find_opt env.defs w with
            | Some ((takes, leaves, defn) as d) ->
                if
                  unify_shape env
                    (List.map reify_ty takes, List.map reify_ty leaves)
                then aux [ defn ]
                else Stack.push (ThWord d) (Stack.top env.latent)
            | None -> raise (Unknown_word w)));
        aux xs
    | EGroup grp :: xs ->
        with_latent_scope env (fun () -> aux grp);
        aux xs
    | [] -> ()
  in
  Fun.compose aux List.singleton
