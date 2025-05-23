{
(* header *)
open Parser
}
rule token = parse
        [' ' '\t' '\r' '\n']+   { token lexbuf } (* skip blanks *)
      | '.'                     { ENDPROG }
      | "const"                 { CONST }
      | '='                     { EQ }
      | ','                     { COMMA }
      | ';'                     { ENDSTMT }
      | "var"                   { VAR }
      | "procedure"             { PROCEDURE }
      | ":="                    { ASSIGN }
      | "call"                  { CALL }
      | '?'                     { QUERY }
      | '!'                     { DISPLAY }
      | "begin"                 { BEGIN }
      | "end"                   { END }
      | "if"                    { IF }
      | "then"                  { THEN }
      | "while"                 { WHILE }
      | "do"                    { DO }
      | "odd"                   { ODD }
      | '#'                     { NE }
      | '<'                     { LT }
      | "<="                    { LE }
      | '>'                     { GT }
      | ">="                    { GE }
      | '+'                     { PLUS }
      | '-'                     { MINUS }
      | '*'                     { TIMES }
      | '/'                     { DIVIDE }
      | '^'                     { POWER }
      | '('                     { LPAREN }
      | ')'                     { RPAREN }
      | ['0'-'9']+ as lxm       { NUMBER(int_of_string lxm) }
      | ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9' '_'] * as id { IDENT id}
{
(* trailer *)
}
