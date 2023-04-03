(* Coursera Programming Languages, Homework 3, Provided Code *)


exception NoAnswer

(**** you can put all your code here ****)
(* 1 *)
fun other_curry f x y = f (y,x)
val only_capitals = List.filter (Char.isUpper o (other_curry String.sub 0))

(* 2 *)
val longest_string1 =
  foldl (fn (x, acc) => if (String.size x > String.size acc) then x else acc) ""

(* 3 *)
val longest_string2 =
  foldl (fn (x, acc) => if (String.size x >= String.size acc) then x else acc) ""

(* 4 *)
fun longest_string_helper f = 
  foldl (fn (x, acc) => if f (String.size x, String.size acc) then x else acc) ""

val longest_string3 = longest_string_helper (fn (a,b) => a > b)

val longest_string4 = longest_string_helper (fn (a,b) => a >= b)

(* 5 *)
val longest_capitalized = longest_string1 o only_capitals 

(* 6 *)
val rev_string = String.implode o rev o String.explode

(* 7 *)
fun first_answer f xs = 
  case xs of 
      [] => raise NoAnswer
    | x::xs' => if isSome(f x) then valOf(f x) else first_answer f xs'

(* 8 *)
fun all_answers f xs =
  case xs of 
      [] => SOME []
    | x::xs' => 
      case (f x) of 
          NONE => NONE
        | SOME v1 => 
          let val next = all_answers f xs'
          in 
            case next of 
                NONE => NONE
              | SOME v2 => SOME (v1@valOf(all_answers f xs'))
          end


datatype pattern = 
    Wildcard
  | Variable of string
  | UnitP
  | ConstP of int
  | TupleP of pattern list
  | ConstructorP of string * pattern

datatype valu = 
    Const of int
  | Unit
  | Tuple of valu list
  | Constructor of string * valu

fun g f1 f2 p =
  let 
	  val r = g f1 f2 
  in
    case p of
        Wildcard          => f1 ()
      | Variable x        => f2 x
      | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
      | ConstructorP(_,p) => r p
      | _                 => 0
  end

(**** for the challenge problem only ****)

datatype typ = Anything
  | UnitT
  | IntT
  | TupleT of typ list
  | Datatype of string

(* 9 *)
val count_wildcards = g (fn _ => 1) (fn _ => 0) 

val count_wild_and_variable_lengths = g (fn _ => 1) (fn x => String.size x)

fun count_some_var (str, p) = g (fn _ => 0) (fn x => if x = str then 1 else 0) p

(* 10 *)
fun all_str_variable p =
  case p of
        Variable x => [x]
      | TupleP ps => List.foldl (fn (x,acc) => (all_str_variable x) @ acc) [] ps
      | ConstructorP(_,p) => all_str_variable p
      | _ => []

fun uni lst = 
  case lst of 
      [] => true
    | (x::xs) => not (List.exists (fn x' => x' = x) xs) andalso (uni xs)

val check_pat = uni o all_str_variable

(* 11 *)
fun match (v,p) = 
  case (v,p) of
      (Unit, UnitP) => SOME []
    | (Const x, ConstP y) => 
        if (x = y) then SOME [("ConstP",Const x)] else SOME []
    | (Tuple x', TupleP y') => all_answers match (ListPair.zip (x',y'))
    | (Constructor (a,b), ConstructorP (a',b')) =>
        if (a = a') then match(b,b') else NONE
    | _ => NONE

(* 12 *)
fun first_match v ps = 
  SOME (first_answer match (map (fn x => (v,x)) ps))
  handle NoAnswer => NONE
