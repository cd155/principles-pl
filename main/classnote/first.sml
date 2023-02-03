(*
    this is a comment. this is our first program.
*)

val x = 34;
(* 
    static environment: x: int
    dynamic environment: x-->34
*)

val y = 17;
(* 
    static environment: x: int, y: int
    dynamic environment: x-->34, y-->17
*)

(* use earlier binding x and y *)
val z =(x + y) + (y + 2)
(* 
    static environment: x: int, y: int, z: int
    dynamic environment: x-->34, y-->17, z-->70
*)

val abs_of_z = if z<0 then 0-z else z;
(* 
    static environment: z<0: bool, 0-z and z need the same type: int, 
    dynamic environment: ..., abs_of_z-->70
*)

val abs_of_z_simpler = abs z

(* 
    Addition
    1. Syntax: 
        e1 + e2 where e1 and e2 are sub expressions
    2. Type-checking:
        if e1 and e2 has type int,
        then e1 + e2 has type int
    3. Evaluation: 
        if e1 evaluates to v1 and e2 evaluates to v2,
        then e1 + e2 evaluates to sum of v1 and v2


    Conditional expression
    1. Syntax: 
        if e1 then e2 else e3 
        where if, then, and else are keyword and
        e1, e2, and e3 are sub expressions
    2. Type-checking:
        first e1 must have type bool
        e2 and e3 can have any type (t), but they must have the same type t
        the type of the entire expression is also t
    3. Evaluation: 
        first evaluate e1 to a value call it v1
        if v1 is true, 
            evaluate e2 and that result is the whole expression's result
        else
            evaluate e3 and that result is the whole expression's result
*)
