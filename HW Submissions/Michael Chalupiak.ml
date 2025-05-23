(*
You will be writing a small interpreted four-function Reverse Polish Notation
calculator. If you are unfamiliar with RPN:
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

To get started, head to https://try.ocaml.pro, paste your code in the editor,
and click "Eval code".

To run the test cases for the final program, type `test ()` into the console.
To manually run your code, run the eval function from the the console:
eval "<RPN sequence>"
For example,
eval "3 2 1 + *"
should evaluate to 9. Additonally, all functions you create are accessible from
the console so you can test individual parts of your implementation.

As an extra feature, add a new operator ? to the calculator. When ? is called,
it prints the current contents of the stack to the console without modifying
any values. You may implement this in any way you prefer as long as the function
signature of eval is not changed and the printout is reasonably usable.

In total, we expect this assignment to take about 50 lines of code to implement
not including test cases or comments. There is no limit on code length, but if
you find yourself writing hundreds of lines of code you may want to reconsider.
*)

(*
This type contains all of the possible varieties of tokens that may appear
in the input. You will probably want to fill this in as you write lex_token.
*)
type token =
  | Num of float
  | Op of string

(*
Breaks a string into space-separated pieces. String.split_on_char may be
useful.
Ex. tokenize "1 234 +" = ["1"; "234"; "+"]
*)
let tokenize (s : string) : string list =
  String.split_on_char ' ' s

(*
For a given token string, produce a member of the token typeclass that
represents it. You may want to modify the stub here to allow use of the function
keyword for slightly nicer pattern-matching, although it's not required.
Ex. lex_token "1234" = Num 1234 (or whatever you end up using)
*)
let lex_token (s : string) : token = 
    match s with
    | "+" -> Op "+"
    | "-" -> Op "-"
    | "*" -> Op "*"
    | "/" -> Op "/"
    | x -> try Num (Float.of_string x) with _ -> invalid_arg x

(*
Given the current state of the RPN stack, apply the next token to it. Similar to
lex_token, modifying the stub to allow function matching may be helpful. You may
also find it useful to write a helper function to abstract away the common parts
of each operator. (One weirdness: OCaml uses a different operator for each
numeric type; floating point operators are all suffixed with ., like '+.'.)
Ex. eval_rpn_helper [Num 1.; Num 2.] (Num 3.) = [Num 1.; Num 2.; Num 3.]
Ex. eval_rpn_helper [Num 3.; Num 6.] Plus = [Num 9.]
*)
let eval_rpn_helper (stack : float list) (input : token) : float list =
  match input with
  | Op "+" -> List.hd (List.tl stack)  +. List.hd stack :: List.tl (List.tl stack)
  | Op "-" -> List.hd (List.tl stack)  -. List.hd stack :: List.tl (List.tl stack)
  | Op "*" -> List.hd (List.tl stack)  *. List.hd stack :: List.tl (List.tl stack)
  | Op "/" -> List.hd (List.tl stack)  /. List.hd stack :: List.tl (List.tl stack)
  | Num x -> x :: stack
  | _ -> stack

(*
Given a list of tokens, evaluate each in turn on a brand new stack, and then
return the top of the resulting stack. You can do this in a very clean way with
List.fold_left, or just use plain old recursion.
Ex. eval_rpn [Num 1.; Num 2.; Num 3.; Plus; Plus] = 6.
*)
let eval_rpn (input : token list) : float = 
  List.hd @@ List.fold_left eval_rpn_helper [] input

(*
This is the user-facing interface for this program. Given a string, resolve it
into tokens and then compute and return its result. You shouldn't need much more
than the functions you've already written, although List.map will make your life
a little easier.
Ex. eval "3 2 1 + *" = 9.
*)
let eval (s : string) : float =
  eval_rpn @@ List.map lex_token (tokenize s)

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
  | Result a, Result b -> Float.equal a b
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
    else (Printf.printf "Test %s failed: got %s, expected %s\n"
      expr
      (string_of_test_result result)
      (string_of_test_result expected)
    ; 0)

let test () : unit = 
  Printf.printf "%i of %i tests passed"
  (List.fold_left (+) 0 @@ List.map run_test tests)
  (List.length tests)
