[@@@ocaml.warning "-37-69"]

type token = Word of string | String of string | Int of int | LParen | RParen

let lex s =
  let handle_escape b = function
    | 'n' -> Buffer.add_char b '\n'
    | 't' -> Buffer.add_char b '\t'
    | 'r' -> Buffer.add_char b '\r'
    | ('"' | '\\') as c -> Buffer.add_char b c
    | _ -> failwith "unrecognized escape sequence"
  in
  let is_delim c = String.contains "()\"" c || Char.Ascii.is_white c in
  let rec aux i acc =
    if i >= String.length s then List.rev acc
    else
      match s.[i] with
      | '#' ->
          let j = ref i in
          while !j < String.length s && not (Char.equal s.[!j] '\n') do
            incr j
          done;
          aux (!j + 1) acc
      | '(' -> aux (i + 1) (LParen :: acc)
      | ')' -> aux (i + 1) (RParen :: acc)
      | '"' ->
          let j = ref (i + 1) in
          let b = Buffer.create 16 in
          while !j < String.length s && not (Char.equal s.[!j] '"') do
            (match s.[!j] with
            | '\\' ->
                incr j;
                handle_escape b s.[!j]
            | c -> Buffer.add_char b c);
            incr j
          done;
          if !j >= String.length s then failwith "unterminated string literal"
          else aux (!j + 1) (String (Buffer.contents b) :: acc)
      | c when Char.Ascii.is_white c -> aux (i + 1) acc
      | _ -> (
          let j = ref i in
          while !j < String.length s && not (is_delim s.[!j]) do
            incr j
          done;
          let tok = String.sub s i (!j - i) in
          match int_of_string_opt tok with
          | Some i -> aux !j (Int i :: acc)
          | None -> aux !j (Word tok :: acc))
  in
  aux 0 []

type value = VInt of int | VStr of string

let string_of_value = function
  | VInt i -> Int.to_string i
  | VStr s -> Printf.sprintf "%S" s

type prim = Add | Mul | Swap | Dup | Drop | Print
type word = { arity : int * int; tokens : token list }
type callable = CPrim of prim | CWord of string

module StringTable = Hashtbl.Make (String)

exception Unknown_word of string

type env = {
  data : value Stack.t;
  mutable latent : callable Stack.t;
  latent_conts : callable Stack.t Stack.t;
  dictionary : word StringTable.t;
}

let make_env () =
  {
    data = Stack.create ();
    latent = Stack.create ();
    latent_conts = Stack.create ();
    dictionary = StringTable.create 16;
  }

let string_of_prim = function
  | Add -> "+"
  | Mul -> "*"
  | Swap -> "swap"
  | Dup -> "dup"
  | Drop -> "drop"
  | Print -> "print"

let prim_of_string_opt = function
  | "+" -> Some Add
  | "*" -> Some Mul
  | "swap" -> Some Swap
  | "dup" -> Some Dup
  | "drop" -> Some Drop
  | "print" -> Some Print
  | _ -> None

let string_of_callable = function
  | CPrim p -> Printf.sprintf "<primitive %S>" (string_of_prim p)
  | CWord w -> Printf.sprintf "<word %S>" w

let prim_arity = function
  | Add | Mul | Swap -> (2, 2)
  | Print | Drop -> (1, 0)
  | Dup -> (1, 2)

let arity env = function
  | CPrim p -> prim_arity p
  | CWord n -> (
      match StringTable.find_opt env.dictionary n with
      | None -> raise (Unknown_word n)
      | Some { arity; _ } -> arity)

let rec times n f =
  if n == 0 then ()
  else (
    f ();
    times (n - 1) f)

let run_prim env vs = function
  | (Add | Mul) as p -> (
      let op =
        match p with
        | Add -> ( + )
        | Mul -> ( * )
        | _ -> raise_notrace (Failure "unreachable")
      in
      let b = Stack.pop vs in
      let a = Stack.pop vs in
      match (a, b) with
      | VInt a, VInt b -> Stack.push (VInt (op a b)) env.data
      | _ -> failwith "type mismatch")
  | Dup ->
      let a = Stack.pop vs in
      times 2 (fun () -> Stack.push a env.data)
  | Swap ->
      let a = Stack.pop vs in
      let b = Stack.pop vs in
      Stack.push b env.data;
      Stack.push a env.data
  | Print -> Printf.printf "%s\n" (string_of_value (Stack.pop vs))
  | _ -> failwith "unimplemented"

let rec run_word env ws word =
  (* Create new stack continuation *)
  Stack.push env.latent env.latent_conts;
  env.latent <- Stack.create ();
  Stack.add_seq env.data (Stack.to_seq ws);
  run env word.tokens;
  if Stack.length env.latent > 0 then
    failwith "word execution left dirty latent stack"
  else env.latent <- Stack.pop env.latent_conts;

and run_callable env vs cb =
  Printf.eprintf "calling %s\n" (string_of_callable cb);
  match cb with
  | CPrim p -> run_prim env vs p
  | CWord n -> (
      match StringTable.find_opt env.dictionary n with
      | None -> raise (Unknown_word n)
      | Some w -> run_word env vs w)

(* The latent check *)
and latent_check' env cb =
  let arity, _ = arity env cb in
  if Stack.length env.data >= arity then (
    let vs = Stack.to_seq env.data |> Seq.take arity |> Stack.of_seq in
    times arity (fun () -> Stack.drop env.data);
    run_callable env vs cb;
    true)
  else false

and latent_check env =
  match Stack.top_opt env.latent with
  | None -> ()
  | Some cb -> if latent_check' env cb then Stack.drop env.latent

and step env token =
  (match token with
  | Int i -> Stack.push (VInt i) env.data
  | String s -> Stack.push (VStr s) env.data
  | Word w -> (
      match prim_of_string_opt w with
      | Some prim -> (
          match latent_check' env (CPrim prim) with
          | false -> Stack.push (CPrim prim) env.latent
          | _ -> ())
      | None -> (
          match StringTable.find_opt env.dictionary w with
          | Some _ -> (
              match latent_check' env (CWord w) with
              | false -> Stack.push (CWord w) env.latent
              | _ -> ())
          | None -> raise (Unknown_word w)))
  | LParen ->
      Stack.push env.latent env.latent_conts;
      env.latent <- Stack.create ()
  | RParen -> (
      if Stack.length env.latent > 0 then
        failwith "closed latent scope without resolving latent stack"
      else
        match Stack.pop_opt env.latent_conts with
        | None -> failwith "unmatched latent scope close"
        | Some s -> env.latent <- s));
  latent_check env

and run env tokens =
  (match tokens with
  | [] -> ()
  | x :: xs ->
      step env x;
      run env xs);
  (* TODO: try to do this as much as possible, some cycle limit shenanigans *)
  latent_check env;
  if Stack.length env.latent > 0 then failwith "latent stack left dirty at end of execution"

let () =
  let code = {|square 9 print-twice|} in
  let tokens = lex code in
  let env = make_env () in
  StringTable.add env.dictionary "square"
    { tokens = [ Word "dup"; Word "*"; ]; arity = (1, 1) };
  StringTable.add env.dictionary "print-twice"
    { tokens = [ Word "dup"; Word "print"; Word "print" ]; arity = (1, 0) };
  Printf.printf "Code: %s\nOutput:\n" code;
  run env tokens
