open Primitive

type value = VInt of int | VStr of string
type reified_ty = RInt | RStr | RAny

exception Mismatch
exception Dirty
exception Unknown_word of string

let string_of_value = function
  | VInt i -> string_of_int i
  | VStr s -> "\"" ^ String.escaped s ^ "\""

type word_def = reified_ty list * reified_ty list * Ast.expr
type thunk = ThPrim of primitive | ThWord of word_def

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

let reify_ty =
  let open Typing in
  function
  | TyCon "Int" -> RInt
  | TyCon "Str" -> RStr
  | TyVar _ -> RAny
  | t ->
      failwith (Printf.sprintf "type %s couldn't be reified" (string_of_ty t))

let shape_of_primitive prim =
  let reify_all = List.map reify_ty in
  Pair.map reify_all reify_all (effect_of_primitive prim)

let ty_of_value = function VInt _ -> RInt | VStr _ -> RStr

let unify a b =
  match (a, b) with
  | RInt, RInt -> true
  | RStr, RStr -> true
  | RAny, _ | _, RAny -> true
  | _, _ -> false

let unify_list a b = List.compare_lengths a b == 0 && List.for_all2 unify a b

let run_primitive env =
  let arith_op op stk =
    let b = Stack.pop stk in
    let a = Stack.pop stk in
    match (a, b) with
    | VInt a, VInt b -> Stack.push (VInt (op a b)) stk
    | _ -> raise Mismatch
  in
  function
  | PAdd -> arith_op ( + ) env.data
  | PSub -> arith_op ( - ) env.data
  | PMul -> arith_op ( * ) env.data
  | PDiv -> (
      let b = Stack.pop env.data in
      let a = Stack.pop env.data in
      match (a, b) with
      | VInt _, VInt 0 ->
          Stack.push (VInt 0) env.data;
          Stack.push (VInt 0) env.data
      | VInt a, VInt b ->
          Stack.push (VInt (a / b)) env.data;
          Stack.push (VInt (a mod b)) env.data
      | _ -> raise Mismatch)
  | PConcat -> (
      let b = Stack.pop env.data in
      let a = Stack.pop env.data in
      match (a, b) with
      | VStr a, VStr b -> Stack.push (VStr (a ^ b)) env.data
      | _ -> raise Mismatch)
  | PSwap ->
      let a = Stack.pop env.data in
      let b = Stack.pop env.data in
      Stack.push a env.data;
      Stack.push b env.data
  | PBury ->
      let c = Stack.pop env.data in
      let b = Stack.pop env.data in
      let a = Stack.pop env.data in
      Stack.push c env.data;
      Stack.push a env.data;
      Stack.push b env.data
  | PDig ->
      let c = Stack.pop env.data in
      let b = Stack.pop env.data in
      let a = Stack.pop env.data in
      Stack.push b env.data;
      Stack.push c env.data;
      Stack.push a env.data
  | PDup -> Stack.push (Stack.top env.data) env.data
  | PDrop -> Stack.drop env.data
  | PItoa -> (
      match Stack.pop env.data with
      | VInt i -> Stack.push (VStr (string_of_int i)) env.data
      | _ -> raise Mismatch)
  | PPrint -> (
      match Stack.pop env.data with
      | VStr s -> print_endline s
      | _ -> raise Mismatch)

let unify_shape env (takes, _) =
  let shape_of_stk =
    Stack.to_seq env.data
    |> Seq.take (List.length takes)
    |> Seq.map ty_of_value |> List.of_seq
  in
  unify_list takes shape_of_stk

let rec latent_check env =
  let lstk = Stack.top env.latent in
  match Stack.top_opt lstk with
  | Some (ThPrim p) ->
      if unify_shape env (shape_of_primitive p) then (
        Stack.drop lstk;
        run_primitive env p)
  | Some (ThWord (takes, leaves, defn)) ->
      if unify_shape env (takes, leaves) then (
        Stack.drop lstk;
        interpret env defn)
  | None -> ()

and with_latent_scope env fn =
  let ls = Stack.create () in
  Stack.push ls env.latent;
  fn ();
  latent_check env;
  if Stack.length ls > 0 then raise Dirty else Stack.drop env.latent

and interpret env expr =
  let rec aux next =
    let open Ast in
    match next with
    | EId -> ()
    | ECat (e1, e2) ->
      aux e1;
      aux e2;
    | EPush (AInt i) ->
        Stack.push (VInt i) env.data;
    | EPush (AStr s) ->
        Stack.push (VStr s) env.data;
    | ECall name ->
        (match primitive_of_string name with
        | Some prim ->
            if unify_shape env (shape_of_primitive prim) then
              run_primitive env prim
            else Stack.push (ThPrim prim) (Stack.top env.latent)
        | _ -> (
            match Hashtbl.find_opt env.defs name with
            | Some ((takes, leaves, defn) as d) ->
                if unify_shape env (takes, leaves) then aux defn
                else Stack.push (ThWord d) (Stack.top env.latent)
            | None -> raise (Unknown_word name)));
    | EDef (name, def) ->
        (match Hashtbl.find_opt env.tenv.sigs name with
        | Some (takes, leaves) ->
            Hashtbl.add env.defs name
              (List.map reify_ty takes, List.map reify_ty leaves, def)
        | None -> failwith "should not happen");
    | EGroup grp ->
        with_latent_scope env (fun () -> aux grp);
    | EQuote _ -> failwith "unimplemented"
  in
  aux expr
