type value = VInt of int | VStr of string
type ty = TyInt | TyString | TyVar of int

exception Mismatch
exception Dirty
exception Unknown_word of string

let string_of_value = function
  | VInt i -> string_of_int i
  | VStr s -> Printf.sprintf "%S" s

type prim =
  | PrimAdd
  | PrimMul
  | PrimConcat
  | PrimSwap
  | PrimDup
  | PrimDrop
  | PrimShow
  | PrimPrint

type thunk = ThPrim of prim
type env = { data : value Stack.t; latent : thunk Stack.t Stack.t }

let make_env () =
  {
    data = Stack.create ();
    latent = Stack.of_seq (Seq.singleton (Stack.create ()));
  }

let prim_of_string_opt = function
  | "+" -> Some PrimAdd
  | "*" -> Some PrimMul
  | "^" -> Some PrimConcat
  | "swap" -> Some PrimSwap
  | "dup" -> Some PrimDup
  | "drop" -> Some PrimDrop
  | "show" -> Some PrimShow
  | "print" -> Some PrimPrint
  | _ -> None

let prim_shape = function
  | PrimAdd | PrimMul -> ([ TyInt; TyInt ], [ TyInt ])
  | PrimConcat -> ([ TyString; TyString ], [ TyString ])
  | PrimSwap -> ([ TyVar 0; TyVar 1 ], [ TyVar 1; TyVar 0 ])
  | PrimDrop -> ([ TyVar 0 ], [])
  | PrimDup -> ([ TyVar 0 ], [ TyVar 0; TyVar 0 ])
  | PrimShow -> ([ TyInt ], [ TyString ])
  | PrimPrint -> ([ TyString ], [])

let ty_of_value = function VInt _ -> TyInt | VStr _ -> TyString

let unify a b =
  let aux a b =
    match (a, b) with
    | TyInt, TyInt -> true
    | TyString, TyString -> true
    | TyVar a, TyVar b -> Int.equal a b
    | TyVar _, _ | _, TyVar _ -> true
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

let unify_shape env (shape, _) =
  let shape_of_stk =
    Stack.to_seq env.data
    |> Seq.take (List.length shape)
    |> Seq.map ty_of_value |> List.of_seq
  in
  unify shape shape_of_stk

let rec latent_check env =
  let lstk = Stack.top env.latent in
  match Stack.top_opt lstk with
  | Some (ThPrim p) ->
      if unify_shape env (prim_shape p) then (
        ignore (Stack.pop lstk);
        do_prim env p)
      else ()
  | None -> ()

and with_latent_scope env fn =
  let lstk = Stack.create () in
  Stack.push lstk env.latent;
  fn ();
  latent_check env;
  if Stack.length lstk > 0 then raise Dirty else Stack.drop env.latent

(* A dumb tree-walking interpreter. *)
let rec interpret : env -> Parsing.tree -> unit =
 fun env ->
  let open Parsing in
  let rec step = function
    | TAtom (AInt i) -> Stack.push (VInt i) env.data
    | TAtom (AString s) -> Stack.push (VStr s) env.data
    | TAtom (AWord w) -> (
        match prim_of_string_opt w with
        | Some prim ->
            if prim_shape prim |> unify_shape env then do_prim env prim
            else Stack.push (ThPrim prim) (Stack.top env.latent)
        | _ -> raise (Unknown_word w))
    | TGroup _ as grp -> interpret env grp
  and iter = function
    | [] -> ()
    | x :: xs ->
        step x;
        (match x with TAtom _ -> latent_check env | _ -> ());
        iter xs
  in
  function
  | TGroup xs -> with_latent_scope env (fun () -> iter xs)
  | x -> step x
