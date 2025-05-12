let handle_file file = 
  print_endline file;;

let handle_all files = 
  (Array.map(handle_file) files);;

let () = (ignore @@ handle_all Sys.argv)