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

(* Add (Constant (10+9), Negate (Constant 4)) *)

fun max_constant e = 
    case e of
        Constant i => i
        | Negate e2 => max_constant e2
        | Add (e1, e2) => Int.max (max_constant e1, max_constant e2)
        | Multiply (e1, e2) => Int.max (max_constant e1, max_constant e2)

val test_exp = Add (Constant 19, Negate (Constant 4))
val nineteen = max_constant test_exp
