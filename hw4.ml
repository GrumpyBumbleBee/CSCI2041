type tree =
  | Leaf
  | Node of int * tree * tree

(* with_default : int -> int option -> int
   Strips an int option type of the Some constructor. If the input
int option is None, return the int input as the default. *)
let with_default x y =
  match x, y with
  | (_, None) -> x
  | (_, Some y) -> y


module Tree_ops =
struct 
(* sum : tree -> int 
  Input: tree.
  Output: int representing sum of all nodes in the tree.*)
  let rec sum t = match t with 
    | Leaf -> 0
    | Node (value, left, right) -> value + (sum (left)) + (sum (right))
    
(* tmax : tree -> int option
   Input: tree.
   Output: maximum value in the tree in the form of an int option type.*)                           
  let rec tmax t = match t with
  | Leaf -> None
  | Node (value, left_t, right_t) -> let lval = tmax (left_t) in let rval = tmax (right_t) in
  if (max lval rval) > (Some value) then (max lval rval)
  else Some (value)

  (* flatten : tree -> int list
     Input: tree
     Output: flattened tree (an int list). For a given node, values in the 
     left tree will always be left of the parent node and the values in the
     right tree will always be right of the parent node. *)
  let rec flatten t =
    match t with
    | Leaf -> []
    | Node (value, left_t, right_t) -> flatten left_t @ [value] @ flatten right_t

  (* Helper function for is_tree_sorted. 
     Input: a' list (flattened tree passed into is_tree_sorted)
     Output: boolean (true if list is sorted from low to high and false otherwise)*)
  let rec list_is_sorted lst =
    match lst with
    | [] -> true
    | h::[] -> true
    | h::h2::tl -> if h <= h2 then list_is_sorted (h2::tl) else false 

  (* is_tree_sorted : tree -> bool
     Input: tree.
     Output: returns boolean true if all subtrees are sorted such that
     tmax left_tree <= parent node value <= tmin right_tree. Returns boolean
     false otherwise. *)
  let rec is_tree_sorted t = 
    list_is_sorted (flatten t)

end