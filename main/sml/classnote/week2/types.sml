(* 
    basic type: describe basic values, int, bool, unit, char
    compound type: build new type with other types inside of them
*)

(* record *)
val x = {bar=(1+2, true andalso true), foo=3+4, baz=(false,9)}
(* #bar x *)

(* 
    Each of
    One of
    Self-reference 
*)

(* 
    reference things by name or by position 
    tuples, by name
    record, by position

    function: 
        caller use position, first/second/third argument
        callee use name, access them by name

    tuple syntax is just a different way to write certain records
    (e1,...,en) is another way of writing {1=e1,...,2=en}
*)

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