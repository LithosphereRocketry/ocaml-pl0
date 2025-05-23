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