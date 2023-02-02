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
