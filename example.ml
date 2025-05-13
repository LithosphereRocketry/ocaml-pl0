(*
You will be writing a small interpreted four-function Reverse Polish Notation calculator
If you are unfamiliar with RPN:
https://www.computersciencebytes.com/array-variables/reverse-polish-notation/

Your program should accept a string made up of numbers and symbols, separated
by single spaces. (Don't worry about handling different amounts or types of
whitespace.) For each number encountered, it should add it to a stack; for
each operator, it should perform the requested operation on the top two elements
and place the result back on the stack. After all input has been used, the
program should return the top value of the stack.

If an invalid symbol is given, the program should call the standard function
invalid_arg with some useful message to throw an illegal-argument exception.
Also, if not enough values are available on the stack to complete an operation,
it should call the standard function failwith with some useful message to throw
a runtime error.

Head to https://try.ocaml.pro
Paste your code in the editor
Click "Eval code"

Test cases are run automatically when you press the Eval Code button. To
manually run your code, type in the console:
eval "<RPN sequence>"
For example,
eval "3 2 1 + *"
should evaluate to 9.

As an extra feature, add a new operator ? to the calculator. When ? is called,
it prints the current contents of the stack to the console without modifying
any values. You may implement this in any way you prefer as long as the function
signature of eval is not changed and the printout is reasonably usable.

*)

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

let run_test : string * float -> bool = function
  | (expr, expected) -> eval expr == expected

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

let () = 
  Printf.printf "%i of %i tests passed"
  (List.fold_left (+) 0 @@ List.map run_test tests)
  (List.length tests)
