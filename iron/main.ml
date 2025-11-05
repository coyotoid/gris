open Iron

let () =
  if Array.length Sys.argv < 2 then Printf.eprintf "give me some code pls\n"
  else
    let code = Sys.argv.(1) in
    let tokens = Lexing.lex code in
    let tree = Parsing.parse tokens in
    let tenv = Infer.make_ty_env () in
    let eff = Infer.infer tenv tree in
    (match eff with
    | [], [] -> ()
    | _, _ ->
        Printf.eprintf "Inferred effect: %s\n" (Infer.string_of_eff_pretty eff));
    prerr_endline "User definitions:";
    Hashtbl.iter
      (fun name eff ->
        match Interpreter.prim_of_string_opt name with
        | None ->
            Printf.eprintf "    %s : %s\n" name (Infer.string_of_eff_pretty eff)
        | Some _ -> ())
      tenv.sigs;
    let env = Interpreter.make_env tenv in
    Out_channel.flush stderr;
    try
      Interpreter.interpret env tree;
      Out_channel.flush_all ();
      if Stack.length env.data > 0 then
        Printf.eprintf "Resulting stack: [%s]\n"
          (Stack.to_seq env.data |> List.of_seq
          |> List.rev_map Interpreter.string_of_value
          |> String.concat " ")
    with Interpreter.Dirty -> ()
