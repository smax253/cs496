let to_upper c = Char.uppercase_ascii c

let rec succl(l: int list) : int list = 
  match l with
  | [] -> []
  | h::t -> (h+1) :: succl t

let rec to_upperl (l: char list):char list = 
  match l with
  | [] -> []
  | h::t -> to_upper h :: to_upperl t

let rec all_zero (l: int list): bool =
  match l with
  | [] -> true
  | h::t -> h=0 && all_zero t

let rec map f l =
  match l with
  | [] -> []
  | h::t -> f h :: map f t

let positive i = i>0
let is_uppercase c = Char.uppercase_ascii c = c
let is_null l = l=[]

let rec greater_than_zero (l : int list) : int list = 
  match l with
  | [] -> []
  | h::t ->
    if positive h
    then h::greater_than_zero t
    else greater_than_zero t
let rec uppercase(l: char list): char list = 
  match l with
  | [] -> []
  | h::t ->
    if is_uppercase h
    then h::uppercase t
    else uppercase t

let rec non_empty(l : 'a list list) : 'a list list = 
  match l with 
  | [] -> []
  | h::t ->
    if not(is_null h)
    then h::non_empty t
    else non_empty t

let rec filter = fun p l ->
  match l with 
  | [] -> []
  | h::t ->
    if p h
    then h :: filter p t
    else filter p t
