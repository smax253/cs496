

type coord = int*int
type coded_pic = coord list

let rec map: ('a -> 'b)  -> 'a list -> 'b list = fun f l ->
  match l with
  | [] -> []
  | h::t -> f h ::  map f t

let cp1:coded_pic = [(0,0);(2,0);(2,2);(0,2);(0,0)]
let cp2:coded_pic = [(0,0);(4,0);(4,4);(0,0)]
          
let rec stretch (p:coded_pic) (factor:int) : coded_pic =
  match p with
  | [] -> []
  | (x,y)::t -> (x*factor, y*factor) :: stretch t factor
    
let stretch_m (p:coded_pic) (factor:int) : coded_pic = 
  map (fun (x,y) -> (x*factor, y*factor)) p

let rec segment (cx,cy) (nx,ny) : coord list =
  if cx == nx && cy == ny 
    then [] 
    else (cx+compare nx cx, cy+compare ny cy) :: segment (cx+compare nx cx,cy+compare ny cy) (nx,ny)
    

let rec coverage ((start::p):coded_pic) : coord list =
  match p with
  | [] -> []
  | h::t -> List.append (segment start h) (coverage p)

let rec foldr: ('a -> 'b  -> 'b)  -> 'b -> 'a list -> 'b = fun f a l ->
  match l with
  | [] -> a
  | h::t -> f h (foldr f a t)

let rec foldl f a l =
  match l with
  | [] -> a
  | h::t -> foldl f (f a h) t

let coverage_f (p:coded_pic) : coord list =
  foldl (fun (h::t) -> segment h (List.hd t)) [] start::p 

let equivalent_pics (cp1:coded_pic) (cp2:coded_pic):bool =
  failwith "Implement"


let height (p:coded_pic):int =
  failwith "Implement"

let width (p:coded_pic):int =
  failwith "Implement"

let tile ((dx,dy):coord) (p:coded_pic) : coded_pic list list =
  failwith "Implement"


let tri_aligned ((x1,y1):coord) ((x2,y2):coord) ((x3,y3):coord):bool =
  failwith "Implement"


let rec compress (p:coded_pic):coded_pic =
  failwith "Implement"

