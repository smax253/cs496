open Ast
open Ds

let rec eval_expr : expr -> exp_val exp_result = fun e ->
  match e with
  | Int(n) ->
    return @@ NumVal n
  | Var(id) ->
    apply_env id
  | Add(e1,e2) ->
    eval_expr e1 >>=
    num_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    num_of_numVal >>= fun n2 ->
    return @@ NumVal (n1+n2)
  | Sub(e1,e2) ->
    eval_expr e1 >>=
    num_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    num_of_numVal >>= fun n2 ->
    return @@ NumVal (n1-n2)
  | Mul(e1,e2) ->
    eval_expr e1 >>=
    num_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    num_of_numVal >>= fun n2 ->
    return @@ NumVal (n1*n2)
  | Div(e1,e2) ->
    eval_expr e1 >>=
    num_of_numVal >>= fun n1 ->
    eval_expr e2 >>=
    num_of_numVal >>= fun n2 ->
    return @@ NumVal (n1/n2)
  | Let(v,def,body) ->
    eval_expr def >>=
    extend_env v >>+
    eval_expr body
  | ITE(e1,e2,e3) ->
    eval_expr e1 >>=
    bool_of_boolVal >>= fun b ->
    if b
    then eval_expr e2
    else eval_expr e3
  | IsZero(e) ->
    eval_expr e >>=
    num_of_numVal >>= fun n ->
    return @@ BoolVal (n = 0)
  | Pair(e1,e2) ->
    eval_expr e1 >>= fun ev1 ->
    eval_expr e2 >>= fun ev2 ->
    return @@ PairVal(ev1,ev2)
  | Unpair(id1,id2,def,body) ->
    eval_expr def >>=
    pair_of_pairVal >>= fun (v1,v2) ->
    extend_env id1 v1 >>+
    extend_env id2 v2 >>+
    eval_expr body
  | Debug(_e) ->
    string_of_env >>= fun str ->
    print_endline str;
    return @@ NumVal 28
  | _ -> error "Not implemented yet!"


let eval_prog (AProg e) = eval_expr e


(* Parse a string into an ast *)

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* Interpret an expression *)
let interp (s:string) : (exp_val,string) result =
  let c = s |> parse |> eval_prog
  in run c (empty_env ())
