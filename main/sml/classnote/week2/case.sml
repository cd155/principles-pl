(* 
    One of building type concept 
    Purple syntax (tag) is constructors, it works like a function
*)
datatype mytype = TwoInts of int*int
                | Str of string
                | Pizza

(* 
    Two aspects to accessing a datatype value 
    1. check what variant it is
    2. extract the data
*)

(* 
    explicit declare mytype is optional
    mytype -> int
*)
fun mytype_to_int (x: mytype) = 
    case x of
          Pizza => 3
        | Str s => 8
        | TwoInts(i1,i2) => i1+i2
     (* | Pizza => 4 (* redundant case: error *) *)

(* fun g x = case x of Pizza => 3 (* missing cases: warning *) *)
