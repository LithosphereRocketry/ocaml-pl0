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
  | a :: b :: tail -> func b a :: tail
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

(* ==================== TEST CODE STARTS HERE ==================== *)
(* Do not modify code below this point *)

type test_result =
  | Result of float
  | InvalidArg
  | Fail

let tests : (string * test_result) list = [
  "1 2 +", Result 3.;
  "5 3 -", Result 2.;
  "3 5 -", Result (-2.);
  "3 2 *", Result 6.;
  "6 2 /", Result 3.;
  "3 6 /", Result 0.5;
  "1 2 + 3 *", Result 9.;
  "3 2 1 + *", Result 9.;
  "-4 5 +", Result 1.;
  "5 1 2 + 4 * 3 - +", Result 14.;
  "4 2 5 * + 1 3 2 * + /", Result 2.;
  "", InvalidArg;
  "1 a +", InvalidArg;
  "7 0 !", InvalidArg;
  "+", Fail;
  "1 + 2", Fail;
  "1 +", Fail;
  "1 2 + *", Fail;
  "8 7 6 5 4 3 2 1 + + +", Result 10.;
]

let results_equal : test_result * test_result -> bool = function
  | Result a, Result b -> a == b
  | InvalidArg, InvalidArg -> true
  | Fail, Fail -> true
  | _ -> false

let string_of_test_result : test_result -> string = function
  | Result f -> string_of_float f
  | InvalidArg -> "invalid_arg ..."
  | Fail -> "failwith ..."

let run_test : string * test_result -> int = function
  | (expr, expected) -> let result = (try Result (eval expr)
      with | Invalid_argument _ -> InvalidArg
           | Failure _ -> Fail
      ) in
    if results_equal (result, expected) then 1
    else (Printf.printf "Test %s failed: got %s, expected %s"
      expr
      (string_of_test_result result)
      (string_of_test_result expected)
    ; 0)

let test () : unit = 
  Printf.printf "%i of %i tests passed"
  (List.fold_left (+) 0 @@ List.map run_test tests)
  (List.length tests)
