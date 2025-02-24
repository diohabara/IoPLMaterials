{
  let reservedWords = [
    (* Keywords *)
    ("else", Parser.ELSE);
    ("false", Parser.FALSE);
    ("if", Parser.IF);
    ("then", Parser.THEN);
    ("true", Parser.TRUE);
    ("in", Parser.IN);
    ("let", Parser.LET);
    ("fun", Parser.FUN);
    ("rec", Parser.REC);
  ]
}

  rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\r' '\t' '\n']+     { main lexbuf }

| "-"? ['0'-'9']+
  { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }
| "(*" { comment lexbuf }
| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| "&&" { Parser.AND }
| "||" { Parser.OR }
| "=" { Parser.EQ }
| "->" { Parser.RARROW }
| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
  { let id = Lexing.lexeme lexbuf in
    try
      List.assoc id reservedWords
    with
      _ -> Parser.ID id
  }
| eof { exit 0 }
and comment = parse
  | "*)" { main lexbuf }
  | _ { comment lexbuf }


