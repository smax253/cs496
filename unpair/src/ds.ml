(* This file defines expressed values and environments *)


(* expressed values and environments are defined mutually recursively *)


type exp_val =
  | NumVal of int
  | BoolVal of bool
  | PairVal of exp_val*exp_val
    
type env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env


type 'a exp_result = env -> ('a,string) result
  
let return v = fun env -> Ok v

let error s = fun env -> Error s

let (>>=) (c:'a exp_result) (f: 'a -> 'b exp_result) : 'b exp_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok v -> f v env

let (>>+) (c:env exp_result) (d:'a exp_result): 'a exp_result =
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok newenv -> d newenv


let run c =
  fun env ->
    c env



(* Operations on environments *)

let empty_env : unit -> env =
  fun () ->
    EmptyEnv

let extend_env : string -> exp_val -> env exp_result =
  fun id v env ->
    Ok (ExtendEnv(id,v,env))

let rec apply_env : string -> exp_val exp_result =
  fun id env ->
  match env with
  | EmptyEnv -> Error (id^" not found!")
  | ExtendEnv(v,ev,tail) ->
    if id=v
    then Ok ev
    else apply_env id tail



(* operations on expressed values *)

let num_of_numVal : exp_val -> int exp_result =  function
  |  NumVal n -> return n
  | _ -> error "Expected a number!"

let bool_of_boolVal : exp_val -> bool exp_result =  function
  |  BoolVal b -> return b
  | _ -> error "Expected a boolean!"


let pair_of_pairVal : exp_val -> (exp_val*exp_val) exp_result = function
  | PairVal(v1,v2) -> return (v1,v2)
  | _ -> error "Expected a pair!"


let rec string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | PairVal(v1,v2) -> "PairVal("^string_of_expval
                        v1^","^string_of_expval v2^")"

let rec string_of_env'  = function
  | EmptyEnv -> ""
  | ExtendEnv(id,v,env) -> string_of_env' env^"\n("^id^","^string_of_expval v^")"

let string_of_env : string exp_result =
  fun env ->
  Ok ("Environment:\n"^ string_of_env' env)
