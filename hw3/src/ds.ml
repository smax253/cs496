(* This file defines expressed values and environments *)




(* operations on expressed values *)


type 'a tree = Empty | Node of 'a * 'a tree * 'a tree


type exp_val =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of string*Ast.expr*env
  | UnitVal
  | ListVal of exp_val list
  | TreeVal of exp_val tree
and
   env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env


(* Environment Abstracted Result *)

type 'a result = Ok of 'a | Error of string

type 'a ea_result = env -> 'a result

let return (v:'a) : 'a ea_result =
  fun _env ->
  Ok v

let error (s:string) : 'a ea_result =
  fun _env ->
  Error s

let (>>=) (c:'a ea_result) (f: 'a -> 'b ea_result) : 'b ea_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok v -> f v env

let (>>+) (c:env ea_result) (d:'a ea_result): 'a ea_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok newenv -> d newenv

let run (c:'a ea_result) : 'a result =
  c EmptyEnv

let lookup_env : env ea_result =
  fun env ->
  Ok env

(* Operations on environments *)

let empty_env : unit -> env ea_result =
  fun () ->
    return EmptyEnv

let extend_env : string -> exp_val -> env ea_result =
  fun id v env ->
    Ok (ExtendEnv(id,v,env))

let rec apply_env : string -> exp_val ea_result =
  fun id env ->
  match env with
  | EmptyEnv -> Error (id^" not found!")
  | ExtendEnv(v,ev,tail) ->
    if id=v
    then Ok ev
    else apply_env id tail





(* operations on expressed values *)

let int_of_numVal : exp_val -> int ea_result =  function
  |  NumVal n -> return n
  | _ -> error "Expected a number!"

let bool_of_boolVal : exp_val -> bool ea_result =  function
  |  BoolVal b -> return b
  | _ -> error "Expected a boolean!"

let list_of_listVal =  function
  | ListVal l -> return l
  | _ ->  error "Expected a list!"

let tree_of_treeVal =  function
  | TreeVal t -> return t
  | _ -> error "Expected a tree!"

let is_listVal = function
  | ListVal(_) ->  true
  | _ ->  false

let has_elements : exp_val -> (exp_val list) ea_result = fun l ->
  list_of_listVal l >>= fun n ->
  match n with
  | [] -> error "List is empty!"
  | a -> return (a)


let rec string_of_list_of_strings = function
  | [] -> ""
  | [id] -> id
  | id::ids -> id ^ "," ^ string_of_list_of_strings ids


let string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | UnitVal  -> "UnitVal"
  | _ -> failwith "string_of_expval: undefined"

let rec string_of_env'  = function
  | EmptyEnv -> ""
  | ExtendEnv(id,v,env) -> string_of_env' env^" ("^id^","^string_of_expval v^")\n"

let string_of_env : string ea_result =
  fun env ->
  Ok ("Environment:\n"^ string_of_env' env)
