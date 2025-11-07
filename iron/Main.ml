open Iron
open Printf

let () =
  let lexer = Lexing.from_channel stdin in
  let expr = Parser.program Lexer.token lexer in
  let tenv = Infer.make_ty_env () in
  let eff = Infer.infer tenv expr in

  (* Print inferred effects *)
  (match eff with
  | [], [] -> ()
  | _, _ -> eprintf "Inferred effect: %s\n" (Typing.string_of_eff eff));

  (* Print user definitions with their inferred effects *)
  if Hashtbl.length tenv.sigs <> 0 then (
    prerr_endline "User definitions:";
    Hashtbl.iter
      (fun name eff -> eprintf "  %s : %s\n" name (Typing.string_of_eff eff))
      tenv.sigs);

  (* Execute the code *)
  let env = Interpreter.make_env tenv in
  Out_channel.flush stderr;
  try
    Interpreter.interpret env expr;
    Out_channel.flush_all ();
    if Stack.length env.data > 0 then
      eprintf "Resulting stack: [%s]\n"
        (Stack.to_seq env.data |> List.of_seq
        |> List.rev_map Interpreter.string_of_value
        |> String.concat " ")
  with Interpreter.Dirty -> ()
