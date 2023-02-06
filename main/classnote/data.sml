(*
    Pair(2-tuples)

    Build:
        Syntax: (e1,e2)
        Evaluation: 
            evaluate e1 to v1 and e2 to v2, result is (v1,v2)
        Type-checking:
            if e1 has type ta and e2 has type tb, 
            then 
                the pair expression has type ta * tb (new kind type)
    
    Access:
        Syntax #1 e and #2 e
        Evaluation: 
            evaluate e to a pair of values and turn first or second piece
        Type-checking:
            if e has type ta * tb
            then
                #1 e has type ta and #2 e has type tb
*)

fun swap(pr: int*bool) =
    (#2 pr, #1 pr)

(* (int*int) * (int*int) -> int *)
fun sum_two_pairs(pr1: int*int, pr2: int*int) =
    (#1 pr1) + (#2 pr1) + (#1 pr2) + (#2 pr2)

fun div_mod(x: int, y: int) =
    (x div y, x mod y)

fun sort_pair(pr: int*int) =
    if(#1 pr) < (#2 pr)
    then pr
    else (#2 pr, #1 pr)
