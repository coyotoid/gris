let () =
  if Array.length Sys.argv < 2 then Printf.eprintf "give me some code pls\n"
  else
    let code = Sys.argv.(1) in
    let tokens = Lexing.lex code in
    let tree = Parsing.parse tokens in
    let env = Interpreter.make_env () in
    Printf.eprintf "Code: %s\nOutput:\n" (Parsing.string_of_tree tree);
    Out_channel.flush stderr;
    Interpreter.interpret env tree;
    Out_channel.flush_all ();
    if Stack.length env.data > 0 then
      Printf.eprintf "Stack: [%s]\n"
        (Stack.to_seq env.data |> List.of_seq
        |> List.map Interpreter.string_of_value
        |> String.concat " ")
