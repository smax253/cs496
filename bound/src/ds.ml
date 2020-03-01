(* This file defines expressed values and environments *)

(* expressed values and environments are defined mutually recursively *)


type exp_val =
  | NumVal of int
  | BoolVal of bool
  | UnitVal
type env =
  | EmptyEnv
  | ExtendEnv of string*exp_val*env


(* Environment Abstracted Result *)

type 'a result = Ok of 'a | Error of string

type 'a ea_result = env -> 'a result

let return : 'a -> 'a ea_result = fun v ->
  fun _env ->
  Ok v

let error : string -> 'a ea_result = fun s ->
  fun _env ->
  Error s

let (>>=) : 'a ea_result -> ('a -> 'b ea_result) -> 'b ea_result = fun c f ->
  fun env ->
  match c env with
  | Error err -> Error err
  | Ok v -> f v env

let (>>+) : env ea_result -> 'a ea_result -> 'a ea_result = fun c d env ->
  match c env with
  | Error err -> Error err
  | Ok newenv -> d newenv

let run : 'a ea_result -> 'a result = fun c ->
  c EmptyEnv

let lookup : env ea_result = fun env ->
  Ok env

(* Operations on environments *)

let empty_env : unit -> env ea_result = fun () ->
  return EmptyEnv

let extend_env : string -> exp_val -> env ea_result = fun id v env ->
  Ok (ExtendEnv(id,v,env))

let rec unbind_env : string -> env ea_result = fun id env ->
  match env with
  | EmptyEnv -> Error (id^" is not bound!")
  | ExtendEnv(v, _, tail) ->
    if v=id
    then Ok tail
    else unbind_env id tail

let rec apply_env : string -> exp_val ea_result = fun id env ->
  match env with
  | EmptyEnv -> Error (id^" not found!")
  | ExtendEnv(v,ev,tail) ->
    if id=v
    then Ok ev
    else apply_env id tail

let rec check_env_alt : string -> exp_val ea_result = fun id env ->
  Ok (BoolVal (match (apply_env id env) with
      | Ok _ -> true
      | Error _ -> false))

let rec check_env : string -> exp_val ea_result = fun id env ->
  match env with
  | EmptyEnv -> Ok (BoolVal false)
  | ExtendEnv(v, _, tail) ->
    if id=v
    then Ok (BoolVal true)
    else check_env id tail

(* operations on expressed values *)

let int_of_numVal : exp_val -> int ea_result =  function
  |  NumVal n -> return n
  | _ -> error "Expected a number!"

let bool_of_boolVal : exp_val -> bool ea_result =  function
  |  BoolVal b -> return b
  | _ -> error "Expected a boolean!"

let string_of_expval = function
  | NumVal n -> "NumVal " ^ string_of_int n
  | BoolVal b -> "BoolVal " ^ string_of_bool b
  | UnitVal  -> "UnitVal"

let rec string_of_env'  = function
  | EmptyEnv -> ""
  | ExtendEnv(id,v,env) -> string_of_env' env^" ("^id^","^string_of_expval v^")\n"

let string_of_env : string ea_result = fun env ->
  Ok ("Environment:\n"^ string_of_env' env)
