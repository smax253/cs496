type dir = North | South | East | West
type snake = dir list
type event = Apple | Move of dir
type run = event list

let rec dropLast = function
  | [] -> failwith "not possible"
  | [d] -> []
  | d1::t -> d1 :: dropLast (t)

let move s = List.hd s :: dropLast s

let eat_apple s = List.hd s :: s

let conflicting d1 d2 =
  match d1, d2 with
  | North, South -> true
  | South, North -> true
  | East, West -> true
  | West, East -> true
  | _ -> false

let change_dir s newdir = 
  if not(conflicting newdir (List.hd s)) 
  then newdir :: dropLast s 
  else move s

let rec coverage ((x,y): int*int) (s:snake) : (int*int) list =
  match s with
  | [] -> [(x,y)]
  | North::t -> (x,y) :: coverage (x,y-1) t
  | South::t -> (x,y) :: coverage (x,y+1) t
  | East::t -> (x, y) :: coverage (x-1, y) t
  | West::t -> (x, y) :: coverage (x+1, y) t

let rec has_duplicates = function
  | [] -> false
  | h::t -> List.mem h t || has_duplicates t

let rec bites_tail s = 
  not( has_duplicates (coverage (0,0) s))
  