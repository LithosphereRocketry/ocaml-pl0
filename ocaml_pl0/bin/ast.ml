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
  
let rec string_of_expr: expression -> string = function
  | Prefix (Plus, exp) -> getPre "+" exp
  | Prefix (Minus, exp) -> getPre "-" exp
  | Infix (exp1, Plus, exp2) -> getIn "+" exp1 exp2
  | Infix (exp1, Minus, exp2) -> getIn "-" exp1 exp2
  | Infix (exp1, Times, exp2) -> getIn "*" exp1 exp2
  | Infix (exp1, Divide, exp2) -> getIn "/" exp1 exp2
  | Literal i -> string_of_int i
  | _ -> "unknown expression"

and getPre (op : string) (exp : expression) : string = "(" ^ op ^ string_of_expr exp ^ ")"
and getIn (op : string) (exp1 : expression) (exp2 : expression) : string = "(" ^ string_of_expr exp1 ^ op ^ string_of_expr exp2 ^ ")"

let string_of_stmt: statement -> string = function
  | Display e -> "! " ^ string_of_expr e
  | Empty -> ""
  | _ -> "unknown statement"

let rec string_of_block (b : block) : string = 
  "const " ^ String.concat ", " (List.map string_of_constdef b.constdef) ^ ";\n" ^
  "var " ^ String.concat ", " b.vardef ^ ";\n" ^
  String.concat "\n" (List.map string_of_proc b.procdef) ^ "\n" ^
  string_of_stmt b.stmt ^ "\n"

and string_of_proc (p : procedure) : string = 
  "procedure " ^ p.name ^ ";\n" ^
  string_of_block p.body
