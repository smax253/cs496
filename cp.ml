(*
Max Shi
2/9/2020
I pledge my honor that I have abided by the Stevens Honor System.
*)

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

let rec segment (cx,cy) (nx,ny) : coord list = if cx == nx && cy == ny
  then []
  else (cx+compare nx cx, cy+compare ny cy) :: segment (cx+compare nx cx,cy+compare ny cy) (nx,ny)

let stretch_m (p:coded_pic) (factor:int) : coded_pic =
  map (fun (x,y) -> (x*factor, y*factor)) p

let rec coverage_helper (p:coded_pic) : coord list =
  match p with
  | start::mid::t -> List.append (segment start mid) (coverage_helper (mid::t))
  | _ -> []

let coverage (pic:coded_pic) : coord list =
  (List.hd pic) :: coverage_helper pic

let coverage_f_helper (point: coord) (p: coded_pic) : coded_pic =
  match p with
  | [] -> [point]
  | h::t -> point::segment point (List.hd p) @ t

let coverage_f (p:coded_pic) : coord list =
  List.fold_right coverage_f_helper p []

let rec equivalent_pics (cp1:coded_pic) (cp2:coded_pic):bool =
  match cp1 with
  | [] -> true
  | h::t -> List.mem h cp2 && equivalent_pics t cp2

let rec height_helper (p:coded_pic) ((high, low): int*int) : int =
  match p with
  | [] -> high-low
  | (x,y)::t -> height_helper t (max high y, min low y)

let height (p:coded_pic):int =
  let (x,y) = List.hd p in
  height_helper p (y,y)

let rec width_helper (p:coded_pic) ((high, low) : int*int) : int =
  match p with
  | [] -> high-low
  | (x,y)::t -> width_helper t (max high x, min high x)

let width (p:coded_pic):int =
  let (x,y) = List.hd p in
  width_helper p (x,x)

let rec shift ((x0,y0): coord) (p:coded_pic) : coded_pic =
  match p with
  | [] -> []
  | (x,y)::t -> (x0+x,y0+y):: shift (x0,y0) t

let rec tile_helper_row ((dx, dy): coord) ((xoff, yoff): coord) (p:coded_pic) : coded_pic list =
  match dx with
  | 0 -> []
  | n -> shift (xoff, yoff) p :: tile_helper_row (dx-1, dy) (xoff + (width p), yoff) p

let rec tile_helper_col ((dx, dy): coord) ((xoff, yoff):coord) (p:coded_pic): coded_pic list list =
  match dy with
  | 0 -> []
  | n -> tile_helper_row (dx,dy) (xoff, yoff) p :: tile_helper_col (dx,dy-1) (xoff, yoff + (height p)) p

let rec tile ((dx,dy):coord) (p:coded_pic) : coded_pic list list =
  tile_helper_col (dx, dy) (0,0) p

let tri_aligned ((x1,y1):coord) ((x2,y2):coord) ((x3,y3):coord):bool =
  List.mem (x2,y2) (segment (x1,y1) (x3,y3)) || List.mem (x1,y1) (segment (x2,y2) (x3,y3)) || List.mem (x3,y3) (segment (x1,y1) (x2,y2))

let rec compress (p:coded_pic):coded_pic =
  match p with
  | [] -> []
  | first::second::third::tl ->
    if tri_aligned first second third
    then compress (first::third::tl)
    else first::compress(second::third::tl)
  | _->p
