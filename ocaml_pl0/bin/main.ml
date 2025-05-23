let handle_code (code : string) = 
  print_endline code

let handle_path (path : string) = 
  let f = open_in path in
    let lexbuf = Lexing.from_channel f in
      ignore (Parsing.set_trace true);
      let ast = Parser.program Lexer.token lexbuf in
        print_endline @@ Ast.string_of_block ast

let rec handle_all: string list -> unit = function
  | path :: rest -> (handle_path path); (handle_all rest)
  | [] -> ()

let () = ignore @@ handle_all @@ List.tl @@ Array.to_list Sys.argv
