(*Coded by: Alexandra Postolaki (posto022, 5510867)*)

(* REDO OF HOMEWORK 3 NON-RECURSIVE *)


(* leftover_count 
   INPUT: int list, int
   OUTPUT: int
   as long as the list is non-empty, each time the counter decrements,
   the first element is discarded and the counter is reset with the value of the 
   current element. If the list is empty, it returns the current value of the 
   counter. *)

let leftover_count (lst: int list) (x: int) : (int) =   
  List.fold_left (
    fun accu h -> 
      match h, accu with
      | _, accu -> if accu = 0 then (h-1) 
      else (accu - 1)
  ) (x) lst

(* last_card
   INPUT: int list, inst
   OUTPUT: int
   similar to leftover_count, but this function returns the value of the last
   position where the counter reset. If the initial counter value does not point
   to a valid position in the list, this function returns -1 *)

let last_card (lst: int list) (x: int) : (int) =
  let _, result = List.fold_left
    (fun (accu, prev) h ->
      if accu = 0 then ((h-1), h)              
      else (accu-1, prev))                                                     
  (x, (-1)) lst
  in result

(* num_greater_than_sum 
   INPUT: int list
   OUTPUT: int
   this function moves along the list while keeping a sum total of all previously
   encountered elements. Returns the total number of elements greater than
   the sum of the previous elements added together. *)
   
let num_greater_than_sum (lst: int list) : (int) = 
  let _, result = List.fold_left
    (fun (accu, prev) h ->
      if accu > h then (accu + h, prev)
      else if accu = h then (accu + h, prev)
      else (accu + h, prev + 1)
      )
  (0, 0) lst
  in result
