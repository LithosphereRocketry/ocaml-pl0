(*
Variables are created immutably with let. They may be type-annotated or allowed
to figure it out for themselves:
*)
let three = 3
let four : int = 4

(*
Let can also be used in expressions to create local variables, much like Scheme
and Haskell:
*)
let seven = let two = 2 in two + 5

(*
Functions are also defined with let, with extra parameters placed before the
colon; likewise, they can be either explicit or implicit. 
*)
let add3 (n : int) : int = n + 3

(*
Functions can also be created anonymously, like lambdas in Scheme. The type
signature for these functions looks a lot like Haskell's:
*)
let add4 : int -> int = (fun n -> n + 4)

(*
The @@ operator can be used to nest functions similar to $ in Haskell:
*)
let add7 (n : int) : int  = add4 (add3 n)
let add7 (n : int) : int = add4 @@ add3 n

(*
Like in Haskell, functions can be partially evaluated:
*)
let addtriplet (a : int) (b : int) (c : int) = a + b + c
let add9: int -> int = addtriplet 4 5

(*
One of the most common contstructs in OCaml is the match statement. This works
similarly to Haskell's pattern-matching:
*)
let counting (n : int) : string =
  match n with
  | 0 -> "Zero"
  | 1 -> "One"
  | _ -> "I don't know"

(*
Since you'll commonly be using this to match function parameters, OCaml provides
a shorthand syntax for doing this in one go:
*)
let counting : int -> string = function
  | 0 -> "Zero"
  | 1 -> "One"
  | _ -> "I don't know"
(*
Note that the type signature changed slightly: we're no longer using the
parameter form of let, since function itself produces a lambda.
*)

(*
Like most functional languages, OCaml lends itself well to recursive functions.
Similar to Scheme, recursive functions can be defined with the let rec syntax:
*)

let rec squareList (l : int list) : int list =
  match l with
  | (current::rest) -> current * current :: squareList rest
  | [] -> []

let rec myMap (func : int -> int) (l : int list) : int list =
  match l with
  | (current::rest) -> func current :: myMap func rest
  | [] -> []

let rec myMap (func : int -> int) : int list -> int list = function
  | (current::rest) -> func current :: myMap func rest
  | [] -> []

let squareList (l : int list) : int list =
  myMap (fun x -> x * x) l

let rec controlMap (func : string -> string) (strings : string list) : string list =
  match strings with
  | ("#"::rest) -> controlMap func rest
  | ("$"::next::rest) -> func next :: func next :: controlMap func rest
  | (current::rest) -> func current :: controlMap func rest
  | [] -> ["."]


let globalCounter = ref 0

let incGC (c : int) =
  globalCounter := !globalCounter + 1

let checkGC (n : int) = !globalCounter == n
