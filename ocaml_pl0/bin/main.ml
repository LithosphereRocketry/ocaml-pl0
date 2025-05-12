type token = 
  | Period
  | Write
  | Literal of int
  ;;

type aaa = {
  name : string
}

type bbb = {
  name : string
}

let string_of_token: token -> string = function
  | Period -> "."
  | Write -> "WRITE"
  | Literal i -> string_of_int i

let tokenize: string -> token list = function
  | _ -> []

let handle_code (code : string) = 
  print_endline @@ string_of_token Period; print_endline code

let handle_path (path : string) = 
  let f = open_in path in
    let text = In_channel.input_all(f) in
      handle_code text  

let rec handle_all: string list -> unit = function
  | path :: rest -> (handle_path path); (handle_all rest)
  | [] -> ()

let () = handle_all @@ List.tl @@ Array.to_list Sys.argv