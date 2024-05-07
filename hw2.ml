(* Alexandra Postolaki (posto022) CSCI2041*)

type exercise = 
| Int of int
| Mult of exercise * int
| Plus of exercise * int

let exercise_one = (Mult (Plus (Plus (Mult (Int 3, 15), 0), 2), 1))
let exercise_two = (Mult (Plus (Mult (Plus (Int 0, 4), 1), 0), 6))
let exercise_three = (Int 64)

(* solution takes in an exercise and returns the total outcome (an int) 
   after completing each operation. *)

let rec solution (prob : exercise) : int =     
        match prob with
        | Int i -> i
        | Mult (i, i') -> solution i * solution (Int i') 
        | Plus (i, i') -> solution i + solution (Int i') 

(*string_of_exercises shows the intended exercises. 
   Type: exercise -> string*)

let rec string_of_exercise (prob : exercise) : string =                     
        match prob with
        | Int i -> string_of_int (i)
        | Mult (i, i') -> (string_of_exercise i) ^ " * " ^ (string_of_exercise (Int i')) ^ " -> ... "
        | Plus (i, i') -> (string_of_exercise i) ^ " + " ^ (string_of_exercise (Int i')) ^ " -> ... "

(* string_of_solution takes in an exercise and shows the string of everything 
plus the final output int as a string
Type: exercise -> string *)    

let rec string_of_solution (prob : exercise) : string =
        match prob with
        | Int i -> string_of_int (i)
        | Mult (i, i') -> (string_of_solution i) ^ " * " ^ (string_of_solution (Int i')) ^ " -> " ^ (string_of_solution (Int (solution (Mult (i , i')))))
        | Plus (i, i') -> (string_of_solution i) ^ " + " ^ (string_of_solution (Int i')) ^ " -> " ^ (string_of_solution (Int (solution (Plus (i, i')))))

(* from_random takes two intput lists and generates the exercise. First list is int list to determine exercise.
 The second list is a list of booleans that determine the operation ('True' = Multiplication, 'False' = Addition) *)

let rec from_random  (nums : int list) (signs : bool list) : exercise =
        match (nums, signs) with                                                      (* Match the two list inputs *)
        | (numshd :: [], []) -> Int numshd                                            (* Boolean list should be one less than nums list *)
        | (numshd :: numstl, signshd :: signstl) -> 
                if signshd = false then Plus(from_random numstl signstl, numshd)      (* Do addition if boolean is 'false', otherwise do multiplication*)
                else Mult (from_random numstl signstl, numshd)         

(* filterNonTrivial takes in an exercise, filters the nontrivial problems (The operations
   that don't do anything) and returns an exercise that doesn't contain trivial problems. *)

let rec filterNonTrivial (prob : exercise) : exercise =   
        match prob with
        | Int _ -> prob
        | Mult (Int 1, n) -> Int n
        | Mult (prob1, 1) -> filterNonTrivial prob1
        | Mult (prob1, prob2) -> Mult (filterNonTrivial prob1, prob2)
        | Plus (prob1, 0) -> filterNonTrivial prob1
        | Plus (prob1, prob2) -> Plus (filterNonTrivial prob1, prob2)

(* splitOnMultZero takes in an exercise splits the problem in two if there is a 0 (or, more 
   specifically, " ... * 0 -> ...") in order to prevent errors from propagating.*)

let rec splitOnMultZero (prob : exercise) : (exercise * exercise) option =                              
        match prob with
        | Int i -> None
        | Mult (prob1, 0) -> Some (prob1, Int 0)
        | Plus(prob1, i) -> (match splitOnMultZero prob1 with
                | None -> None
                | Some (prob2, prob3) -> Some (prob2, Plus (prob3,i)))
        | Mult(prob1, i) -> (match splitOnMultZero prob1 with 
                | None -> None
                | Some (prob2, prob3) -> Some (prob2, Mult (prob3, i)))

(* keepSplitting splits the input exercise to a lsit of different exercises. Returns an exercise
   list. *)

let rec keepSplitting (prob : exercise) : exercise list =                                            
        match splitOnMultZero prob with
        | None -> [prob]
        | Some (prob1, prob2) -> keepSplitting(prob1) @ keepSplitting(prob2)


 let rec printExercise (probs : exercise list) : unit =
  match probs with
  | h :: tl -> print_string ((string_of_exercise h)^"\n"^(string_of_solution h)^"\n\n"); printExercise tl
  | [] -> ()

let rec genProblems (nums : int list) (signs : bool list) : unit =
  (printExercise (keepSplitting (filterNonTrivial (from_random nums signs)))) 
