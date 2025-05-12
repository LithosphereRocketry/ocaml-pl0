type token =
  | Plus
  | Minus
  | Times
  | Divide
  | Value of float
 
let tokenize (s : string) : string list =
  String.split_on_char ' ' s 
     
let lex_token : string -> token = function
  | "+" -> Plus
  | "-" -> Minus
  | "*" -> Times
  | "/" -> Divide
  | s when Option.is_some (Float.of_string_opt s) -> Value (Float.of_string s)
  | s -> invalid_arg s 
           
let eval_binary (func : float -> float -> float) : float list -> float list = function
  | a :: b :: tail -> func a b :: tail
  | _ -> failwith "Not enough elements on stack!" 

let eval_rpn_helper (stack : float list) : token -> float list = function
  | Value n -> n :: stack
  | Plus -> eval_binary ( +. ) stack
  | Minus -> eval_binary ( -. ) stack
  | Times -> eval_binary ( *. ) stack
  | Divide -> eval_binary ( /. ) stack
               
      
let eval_rpn (input : token list) : float = 
  List.hd @@ List.fold_left eval_rpn_helper [] input
 
let eval (s : string) : float =
  eval_rpn @@ List.map lex_token @@ tokenize s 