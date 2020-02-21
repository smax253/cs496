(*
Max Shi
2/10/2020
I pledge my honor that I have abided by the Stevens Honor System.
*)
type ('a, 'b) dTree = Leaf of 'b | Node of 'a * ('a, 'b) dTree * ('a, 'b) dTree

let tLeft = Node('w',
                  Node('x',
                        Leaf(2), Leaf (5)),
                  Leaf(8))

let tRight = Node('w',
                  Node('x',
                        Leaf(2), Leaf (5)),
                  Node('y',
                        Leaf(7), Leaf(5)))

let rec dTree_height (tree: ('a, 'b) dTree) : int =
  match tree with
  | Leaf(_) -> 0
  | Node(_, lt, rt) -> 1 + max (dTree_height lt) (dTree_height rt)

let rec dTree_size = function
  | Leaf(_) -> 1
  | Node(_, lt, rt) -> 1 + dTree_size lt + dTree_size rt

let rec dTree_paths = function
  | Leaf(_) -> [[]]
  | Node(_, lt, rt) ->
    let (leftpaths, rightpaths) = (dTree_paths lt, dTree_paths rt)
    in let leftT = List.map (fun l -> 0::l) leftpaths
    in let rightT = List.map (fun l -> 1::l) rightpaths
    in leftT @ rightT

let rec dTree_is_perfect = function
  | Leaf(_) -> true
  | Node(_, lt, rt) ->
    (dTree_height lt) = (dTree_height rt) && dTree_is_perfect lt && dTree_is_perfect rt

let rec dTree_map (f:'a -> 'a) (g: 'b -> 'b) = function
  | Leaf(x) -> Leaf(g x)
  | Node(x, lt, rt) -> Node(f x, dTree_map f g lt, dTree_map f g rt)

let rec list_to_tree = function
  | [] -> Leaf(0)
  | [x] -> Node(x, Leaf(0), Leaf(0))
  | h::t -> Node(h, list_to_tree t, list_to_tree t)

let rec replace_leaf_helper (tree: ('a,'b) dTree) ((path, n) : int list * int) : ('a,'b) dTree =
  match tree, path with
  | _, [] -> Leaf(n)
  | Node(x, lt, rt), h::t ->
    if h=0
    then Node(x, replace_leaf_helper lt (t, n), rt)
    else Node(x, lt, replace_leaf_helper rt (t, n))
  | Leaf(_), _ -> failwith "Invalid tree or path"

let rec replace_leaf_at tree = function
  | [] -> tree
  | h::t -> replace_leaf_at (replace_leaf_helper tree h) t

let rec bf_to_dTree (chars, graph) =
  let tree = list_to_tree chars
  in replace_leaf_at tree graph
