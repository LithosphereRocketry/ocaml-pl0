let handle_code (code : string) = 
  print_endline code

let handle_path (path : string) = 
  let f = open_in path in
    let text = In_channel.input_all(f) in
      handle_code text  

let rec handle_all: string list -> unit = function
  | path :: rest -> (handle_path path); (handle_all rest)
  | [] -> ()

let () = ignore @@ handle_all @@ List.tl @@ Array.to_list Sys.argv
