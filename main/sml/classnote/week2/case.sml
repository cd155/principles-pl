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