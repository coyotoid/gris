open Iron

let () =
  if Array.length Sys.argv < 2 then
    Printf.eprintf "usage: %s program...\n" (Sys.argv.(0))
  else
    let code = String.concat " " (Array.to_seq Sys.argv |> Seq.drop 1 |> List.of_seq) in
    let tokens = Lexing.lex code in
    let tree = Parsing.parse tokens in
    let tenv = Infer.make_ty_env () in
    let eff = Infer.infer tenv tree in

    (* Print inferred effects *)
    (match eff with
    | [], [] -> ()
    | _, _ ->
        Printf.eprintf "Inferred effect: %s\n" (Infer.string_of_eff_pretty eff));

    (* Print user definitions with their inferred effects *)
    if Hashtbl.length tenv.sigs <> 0 then (
      prerr_endline "User definitions:";
      Hashtbl.iter
        (fun name eff ->
          Printf.eprintf "    %s : %s\n" name (Infer.string_of_eff_pretty eff))
        tenv.sigs);

    (* Execute the code *)
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
