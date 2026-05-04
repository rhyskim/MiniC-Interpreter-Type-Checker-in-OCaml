let parse (s : string) : Ast.prog = 
  let lbuf = Lexing.from_string s in
  Parser.parse Lexer.read lbuf
