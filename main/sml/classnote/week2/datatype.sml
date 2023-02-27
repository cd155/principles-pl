datatype suit = Club | Diamond | Heart | Spade
datatype rank = Jack | Queen | King | Ace| Num of int
datatype id = StudentNum of int
            | Name of string * (string option) * string

(* each of type vs one-of type *)
type a = {
    student_num: int,
    first: string,
    middle: string option,
    last: string
}

datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp 

fun eval e =
    case e of
          Constant i        => i
        | Negate e2         => ~ (eval e2)
        | Add (e1, e2)      => (eval e1) + (eval e2)
        | Multiply (e1, e2) => (eval e1) * (eval e2)

(* other version *)
fun eval' (Constant i) = i
  | eval' (Negate e2) = ~ (eval' e2)
  | eval' (Add(e1,e2)) = (eval' e1) + (eval' e2)
  | eval' (Multiply(e1,e2)) = (eval' e1) + (eval' e2)

(* Add (Constant (10+9), Negate (Constant 4)) *)

fun max_constant e = 
    case e of
        Constant i => i
        | Negate e2 => max_constant e2
        | Add (e1, e2) => Int.max (max_constant e1, max_constant e2)
        | Multiply (e1, e2) => Int.max (max_constant e1, max_constant e2)

val test_exp = Add (Constant 19, Negate (Constant 4))
val nineteen = max_constant test_exp

type card = suit * rank
type name_record = {
    student_num: int option,
    first: string,
    middle: string option,
    last: string
}

fun is_Queen_of_Spades (c: card) =
    #1 c = Spade andalso #2 c = Queen

val c1: card = (Diamond, Ace)
val c2: suit * rank = (Heart, Ace)
val c3 = (Spade, Ace)

(* 
    list, option are type constructor, 
    they take type parameters to produce types
*)

(* type is int list -> int *)
fun sum_list xs =
    case xs of
          [] => 0
        | x::xs' => x + sum_list xs'

(* type is 'a list * 'a list -> 'a list *)
fun append (xs,ys) =
    case xs of
          [] => ys
        | x::xs' => x:: append(xs', ys)

datatype 'a option = NONE | SOME of 'a
datatype 'a mylist = Empty | Cons of 'a * 'a mylist
datatype ('a,'b) tree = 
      Node of 'a * ('a,'b) tree * ('a,'b) tree 
    | Leaf of 'b

(* (int,int) tree -> int *)
fun sum_tree tr =
    case tr of
          Leaf i => i
        | Node(i, lft, rgt) => i + sum_tree lft + sum_tree rgt

(* ('a,int) tree -> int *)
fun sum_leaves tr =
    case tr of
        Leaf i => i
        | Node(i, lft, rgt) => sum_leaves lft + sum_leaves rgt

(* ('a,'b) tree -> int *)
fun num_leaves tr =
    case tr of
          Leaf i => 1
        | Node(i, lft, rgt) => num_leaves lft + num_leaves rgt

fun fun_name {first=x, middle=y, last=z} =
    x ^ "" ^ y ^ "" ^ z

fun sum_triple (x,y,z) =
    x + y + z

fun fun_name2 (r: {first:string, middle:string, last:string}) =
    #first r ^ " " ^ #middle r ^ " " ^ #last r

(* 
    the following version will not type-check without type annotations
    because the type-checker cannot figure out if there might be other
    field. In other words, the type-checker not sure if there will be 3 int
    tuple or more.

    fun sum_triple2 triple = 
        #1 triple + #2 triple + #3 triple 
*)

(*
    int * 'a * int -> int

    it is a polymorphic function which y can be any type
*)
fun partial_sum (x, y, z) = 
    x + z

fun partial_name {first=x, middle=y, last=z} =
    x ^ "" ^ z

(* ''a * ''a -> string *)
fun same_thing (x, y) =
    if x=y then "yes" else "no"

(* int -> string *)
fun is_three x =
    if x=3 then "yes" else "no"

fun append ([],ys) = ys
  | append (x::xs',ys) = x :: append(xs', ys)
  