type 'a tree = Empty | Node of 'a *'a tree *'a tree

let rec foldl f a l =
  match l with
  | [] -> a
  | h::t -> foldl f (f a h) t 


let rec mt = fun f l ->
    match l with 
    | [] -> Empty
    | [x] -> Node(f x, Empty, Empty)
    | h::t -> Node( (foldl f 0 l)  , mt f (split_left l), mt f (split_right l) )
    

let rec split_left_helper ((h::t) : int list) (num:int) : int list = 
    match h::t with 
    | h::t ->
    if num = 0
    then []
    else List.cons h (split_left_helper t (num-1))
    | _ -> failwith "ops"

let rec split_left l =
    split_left_helper l ((List.length l)/2)

let rec split_right_helper  ((h::t) : int list) (num:int): int list = 
    if num = 0
    then h::t
    else split_right_helper t (num-1)

let split_right = fun l ->
    split_right_helper l ((List.length l)/2)