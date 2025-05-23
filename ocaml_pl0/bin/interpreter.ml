module StringMap = Map.Make(String)

type environment = {
  constants : int StringMap.t;
  variables : int option StringMap.t;
  procedures : Ast.block StringMap.t;
  parent : environment option
}

let rec lookup_var (env : environment) (v : string) : int = 
  match StringMap.find_opt v env.constants with
  | Some i -> i
  | None -> 
    match StringMap.find_opt v env.variables with
  | Some Some i -> i
  | Some None -> (failwith ("Variable " ^ v ^ " used but never assigned"))
  | None ->
    match env.parent with
  | Some e -> lookup_var e v
  | None -> (failwith ("Undefined variable " ^ v))

let rec lookup_proc (env : environment) (v : string) : Ast.block = 
  match StringMap.find_opt v env.procedures with
  | Some p -> p
  | None -> 
    match env.parent with
  | Some e -> lookup_proc e v
  | None -> (failwith ("Undefined procedure " ^ v))

let rec set_var (env : environment) (var : string) (num : int) : environment =
  if StringMap.mem var env.variables then { env with
      variables = StringMap.update var (fun _ -> Some (Some num) ) env.variables
    }
  else match env.parent with 
  | Some penv -> { env with parent = Some (set_var penv var num) }
  | None -> (failwith ("Tried to assign undefined variable " ^ var))

let rec eval_exp (env : environment) : Ast.expression -> int = function
  | Literal i -> i
  | Prefix (op, i) -> (match op with 
    | Plus -> (eval_exp env i)
    | Minus -> (-(eval_exp env i)))
  | Infix (a, op, b) -> let ra = (eval_exp env a) and rb = (eval_exp env b) in 
    (match op with
      | Plus -> ra + rb
      | Minus -> ra - rb
      | Times -> ra * rb
      | Divide -> ra / rb)
  | Variable v -> lookup_var env v

let eval_cond (env : environment) : Ast.condition -> bool = function
  | Odd expr -> (eval_exp env expr) land 1 = 1
  | Comparison (a, op, b) -> let ra = (eval_exp env a) and rb = (eval_exp env b) in match op with
    | LessThan -> ra < rb
    | GreaterThan -> ra > rb
    | LessEqual -> ra <= rb
    | GreaterEqual -> ra >= rb
    | Equals -> ra = rb
    | NotEquals -> ra != rb

let rec interpret_statement (env : environment) : Ast.statement -> environment = function
  | Display e -> print_int (eval_exp env e); print_endline ""; env
  | Assignment (var, exp) -> set_var env var (eval_exp env exp)
  | Begin stmts -> List.fold_left interpret_statement env stmts
  | Call name -> Option.get (interpret_block (Some env) (lookup_proc env name))
  | Query name -> let v = read_int () in set_var env name v
  | If (cond, body)-> if eval_cond env cond then interpret_statement env body else env
  | While (cond, body) -> if eval_cond env cond then
      let env_after = interpret_statement env body in interpret_statement env_after (While (cond, body))
      else env
  | Empty -> env
  
and interpret_block (parent : environment option) (blk : Ast.block) : environment option = 
  let env = {
    constants = StringMap.of_list blk.constdef;
    variables = StringMap.of_list @@ List.map (fun a -> (a, None)) blk.vardef;
    procedures = StringMap.of_list @@ List.map (fun (a : Ast.procedure) -> (a.name, a.body)) blk.procdef;
    parent = parent;
  } in (interpret_statement env blk.stmt).parent

let interpret (blk : Ast.block) : unit = 
  ignore (interpret_block None blk)
