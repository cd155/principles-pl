(* work only if y >= 0 *)
fun pow(x: int, y: int) =
    if y=0
    then 1
    else x * pow(x,y-1) (* pow itself has int*int -> int 
                           pow fortunately take two arguments, 
                            x: int, y-1: int, the result type is int
                        *)

fun cube(x: int) =
    pow(x,3)

val sixtyfour = cube 4

val fortytwo = pow(2,2+2) + pow(4,2) + cube 2 + 2

(*
    Syntax:
        fun x0 (x1: t1, ..., xn: tn) = e (also call function body)
    
    Evaluation: a function is a value
        Adds x0 to environment so later expressions can call it
    
    Type-checking:
        - Add binding x0: (t1 *...* tn) -> t if:
        - check type-checking body e to have type t in the static environment
          1. check early binding
          2. x1: t1, ..., xn:tn (arguments with their types)
          3. x0: (t1 *...* tn) ->  (recursion)

    Function Call Type checking:
        Syntax: e0 (e1,..,en)
        Type-checking:
            if: 
                e0 has some type(t1 *...* tn) -> t
                e1 has type t1,..., en has type tn
            then:
                e0(e1,...,en) has type t
    
    e0 (e1,..,en)

    Function Call Evaluation:
    1. evaluate e0 to a function fun x0(x1: t1, ..., xn: tn) = e
        find the right function binding, result will be a function
    2. eagerly evaluate arguments to v1, ..., vn
    3. map x1 to v1, ..., xn to vn to get evaluation of e
        - this include evaluate x0 for recursion
*)