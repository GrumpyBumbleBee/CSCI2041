(*hw1.ml by: Alexandra Postolaki (posto022) *)

(* generate_duck_helper function takes in 2 integers 'i' and 'n'. 'i' used to decrement the number of ducks left while 'n' is used
   to store the original number of ducklings at the end of the entire song. Returns a list of strings with each element representing
   a stanza in the song.*)

let rec generate_duck_helper(i: int) (n:int) : string list = 
  if n = 0 then ["Mama duck went swimming one day
    Over the hills and far away
    The mama duck said, \"Quack, quack, quack, quack\"
    And then no little ducks came back"]
  else if i = 0 && n = 1 then ["Mama duck went swimming one day
    Over the hills and far away
    The mama duck said, \"Quack, quack, quack, quack\"
    And all 1 little duck came back"]
  else if i = 0 then ["Mama duck went swimming one day
    Over the hills and far away
    The mama duck said, \"Quack, quack, quack, quack\"
    And all " ^ (string_of_int n) ^ " little ducks came back"]
  else if i = 1 then "1 little duck went swimming one day
    Over the hills and far away
    The mama duck said, \"Quack, quack, quack, quack\"
    And then no more little ducks came back" :: generate_duck_helper (i - 1) n
  else if i = 2 then "2 little ducks went swimming one day
    Over the hills and far away
    The mama duck said, \"Quack, quack, quack, quack\"
    And only 1 little duck came back" :: generate_duck_helper (i-1) n
  else ((string_of_int i) ^ " little ducks went swimming one day
  Over the hills and far away
  The mama duck said, \"Quack, quack, quack, quack\"
  And only " ^ (string_of_int (i-1)) ^ " little ducks came back") :: generate_duck_helper (i-1) n

(* generate_duck_verse funciton takes in an int 'n' representing the number of ducks to start the duck song and concatinates each string element 
in the string list given by calling the recursive generate_duck_helper function with the given number of ducks. Concatinates each string element with
a newline character and returns a string containing each stanza of the song.*)

let generate_duck_verse (n: int) : string = 
  String.concat "\n" (generate_duck_helper n n);;
  
(*print_duck_verse function takes in an int 'n' representing the number of ducks to start the song and calls generate_duck_verse function
   to create the string output that gets printed out.*)

let print_duck_verse (n: int) = generate_duck_verse (n) 