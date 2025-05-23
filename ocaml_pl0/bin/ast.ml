type prefix_operator = 
  | Plus
  | Minus

type infix_operator = 
  | Plus
  | Minus
  | Times
  | Divide

type comparison = 
  | Equals
  | NotEquals
  | LessThan
  | LessEqual
  | GreaterThan
  | GreaterEqual

type expression =
  | Prefix of prefix_operator * expression
  | Infix of expression * infix_operator * expression
  | Literal of int
  | Variable of string

type condition = 
  | Odd of expression
  | Comparison of expression * comparison * expression

type statement = 
  | Assignment of string * expression
  | Call of string
  | Query of string
  | Display of expression
  | Begin of statement list
  | If of condition * statement
  | While of condition * statement
  | Empty

type procedure = {
  name : string;
  body : block;
} and block = {
  constdef : (string * int) list;
  vardef : string list;
  procdef : procedure list;
  stmt : statement;
}

let string_of_constdef ((a : string), (b : int)) : string =
    a ^ " = " ^ string_of_int b
  
let string_of_expr: expression -> string = function
  | Literal i -> string_of_int i
  | _ -> "unknown expression"

let string_of_stmt: statement -> string = function
  | Display e -> "! " ^ string_of_expr e ^ ";"
  | Empty -> ""
  | _ -> "unknown statement"

let rec string_of_block (b : block) : string = 
  "const " ^ String.concat ", " (List.map string_of_constdef b.constdef) ^ ";\n" ^
  "var " ^ String.concat ", " b.vardef ^ ";\n" ^
  String.concat "\n" (List.map string_of_proc b.procdef) ^ "\n" ^
  string_of_stmt b.stmt ^ "\n."

and string_of_proc (_ : procedure) : string = 
  "(PROCEDURE)"