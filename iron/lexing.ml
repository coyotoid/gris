type token = Word of string | String of string | Int of int | LParen | RParen

let lex s =
  let handle_escape b = function
    | 'n' -> Buffer.add_char b '\n'
    | 't' -> Buffer.add_char b '\t'
    | 'r' -> Buffer.add_char b '\r'
    | ('"' | '\\') as c -> Buffer.add_char b c
    | _ -> failwith "unrecognized escape sequence"
  in
  let is_delim c = String.contains "()\";" c || Char.Ascii.is_white c in
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
      | ';' -> aux (i + 1) acc
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
