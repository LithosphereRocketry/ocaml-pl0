
let counting (n : int) : string =
  match n with
  | 0 -> "Zero"
  | 1 -> "One"
  | _ -> "I don't know"

let counting : int -> string = function
  | 0 -> "Zero"
  | 1 -> "One"
  | _ -> "I don't know"

let add3 (n : int) : int = n + 3
let add4 (n : int) : int = n + 4
let add7 (n : int) : int = add4 @@ add3 n


let rec myMap (func : int -> int) (l : int list) : int list =
  match l with
  | (current::rest) -> func current :: myMap func rest
  | [] -> []

let rec myMap (func : int -> int) : int list -> int list = function
  | (current::rest) -> func current :: myMap func rest
  | [] -> []

let rec controlMap (func : string -> string) (strings : string list) : string list =
  match strings with
  | ("#"::rest) -> controlMap func rest
  | ("$"::next::rest) -> func next :: func next :: controlMap func rest
  | (current::rest) -> func current :: controlMap func rest
  | [] -> ["."]
