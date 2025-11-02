let string_of_ty =
  let open Infer in
  function TyCon n -> n | TyVar (TypeId v) -> Printf.sprintf "'%d" v

let string_of_eff (takes, leaves) =
  let show_list = function
    | [] -> "[]"
    | xs -> "[" ^ String.concat " " (List.rev_map string_of_ty xs) ^ "]"
  in
  show_list takes ^ " -> " ^ show_list leaves

let () =
  if Array.length Sys.argv < 2 then Printf.eprintf "give me some code pls\n"
  else
    let code = Sys.argv.(1) in
    let tokens = Lexing.lex code in
    let tree = Parsing.parse tokens in
    let eff = Infer.infer tree in
    Printf.eprintf "Inferred effect: %s\n" (string_of_eff eff);
    let env = Interpreter.make_env () in
    Out_channel.flush stderr;
    Interpreter.interpret env tree;
    Out_channel.flush_all ();
    if Stack.length env.data > 0 then
      Printf.eprintf "Resulting stack: [%s]\n"
        (Stack.to_seq env.data |> List.of_seq
        |> List.rev_map Interpreter.string_of_value
        |> String.concat " ")
