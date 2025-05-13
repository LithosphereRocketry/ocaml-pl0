(*
To get started, head to https://try.ocaml.pro, paste your code in the editor,
and click "Eval code". To run a function, type the function name followed by its arguments.
Ex.
add3 2
*)

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
signature for these functions looks a lot like Haskell's. Note that int -> int
is a type just like any other:
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
Matches can also have conditions applied using the when keyword:
*)
let counting : int -> string = function
  | 0 -> "Zero"
  | 1 -> "One"
  | x when x > 100 -> "really big"
  | _ -> "I don't know"

(*
Like most functional languages, OCaml lends itself well to recursive functions.
Similar to Scheme, recursive functions can be defined with the let rec syntax:
*)
let rec squareList (l : int list) : int list =
  match l with
  | (current::rest) -> current * current :: squareList rest
  | [] -> []
(*
Note the mildly unusual way OCaml displays list types.
*)

(*
Since this is a common pattern, we might want to make a function that works
generally across any operation. Take a shot at implementing myMap, which takes
a function from integer to integer and maps it across all elements of a list.
(Solution below the break.)
*)

















(*
Here's one way to do it:
*)
let rec myMap (func : int -> int) (l : int list) : int list =
  match l with
  | (current::rest) -> func current :: myMap func rest
  | [] -> []
let squareList (l : int list) : int list =
  myMap (fun x -> x * x) l
(*
Note that when we convert this to the function syntax, just the l parameter gets
consumed into the result's type. Our function now produces a lambda which
transforms an int list - thanks to partial evaluation, this doesn't actually
change anything in practice.
*)
let rec myMap (func : int -> int) : int list -> int list = function
  | (current::rest) -> func current :: myMap func rest
  | [] -> []

(*
As you may guess, OCaml provides an implementation of this already, along with
the usual complement of other list-procesing primitives:
*)
let squareList (l : int list) : int list =
  List.map (fun x -> x * x) l
let sum (l : int list) : int = 
  List.fold_left (fun a b -> a + b) 0 l
(*
(Note: a fun shortcut with this particular task is that operators may be turned
into functions with parentheses, so our lambda can just be replaced with (+):)
*)
let sum (l : int list) : int = 
  List.fold_left (+) 0 l

(*
OCaml supports custom types which are very reminiscent of Haskell. The most
important to us are variants, which represent a varied collection of things.
Entries may either be self-contained units or contain some data:
*)
type referrer = 
  | WordOfMouth
  | Friend of string
  | Popularity of int

(*
Of course, these can be pattern-matched:
*)
let welcome = function
  | WordOfMouth -> "Glad you heard about us!"
  | Friend f -> "Glad " ^ f ^ " told you!"
  | Popularity p -> "Congrats on becoming one of the " ^ string_of_int p

(*
One last useful type is option, which works as you'd expect:
*)
let read_out: string option -> string = function
  | Some x -> x
  | None -> "Oops nothing there"

(*
More stuff (this isn't needed for the homework, but it's good to know):
*)

(*
Mutability is one of the major differences from Haskell. This is implemented
using the ref datatype, similar to Scheme's boxes:
*)
let globalCounter = ref 0

let incGC (c : int) =
  globalCounter := !globalCounter + 1

let checkGC (n : int) = !globalCounter == n

(*
Generics work a lot like Haskell, and are frequently used for functions that
involve tying other functions together:
*)
let compose (f : 'a -> 'b) (g : 'b -> 'c) (x : 'a) : 'c = 
  g (f x)

(*
While existing operators can't be overloaded, new ones can be defined; any 
function name matching a certain character set can be an operator:
*)
let ( >^^> ) f g = compose f g

(*
Record types are also similar to Haskell, but have their own member access
syntax, meaning no name conflicts:
*)
type book = {
  pages : int
}
type royal_guard = {
  pages : string list
}
