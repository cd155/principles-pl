
(* n+x *)
fun increment_n_times_lame(n,x) = 
    if n=0
    then x
    else 1 + increment_n_times_lame(n-1,x)

(* 2^n * x *)
fun double_n_times_lame(n,x) = 
    if n=0
    then x
    else x * double_n_times_lame(n-1,x)

(* example 3, [4,8,12,16] -> [16] *)
fun nth_tail_lame(n,xs) = 
    if n=0
    then xs
    else tl(nth_tail_lame(n-1,xs))

(* f(f(f(f...(x)))) *)
fun n_times(f,n,x) = (* ('a -> 'a) * int * 'a -> 'a *)
    if n = 0
    then x
    else f (n_times(f,n-1,x))

fun increment x = x+1
fun double x = x+x

val x1 = n_times(double,4,7)
val x2 = n_times(increment,4,7)
val x3 = n_times(tl,2,[4,8,12,16])

fun addition(n,x) = n_times(increment,n,x)    (* instantiates 'a with int *)
fun double_n_times(n,x) = n_times(double,n,x) (* instantiates 'a with int *)
fun nth_tail(n,x) = n_times(tl,n,x)           (* instantiates 'a with int list *)

fun triple x = 3*x
fun triple_n_times(n,x) = n_times(triple,n,x)

(* 
    higher-order functions are often polymorphic based on "whatever" type
    of function is passed" but not always
*)
fun times_until_zero (f,x) =
    if x=0 then 0 else 1 + times_until_zero(f, f x)

fun len xs =
    case xs of
          []     => 0
        | _::xs' => 1 + len xs'

fun triple_n_times' (n,x) = n_times(fn x => 3*x,n,x)

(* fun triple x = 3*x *)
(* fun triple is syntactic sugar of val triple, as long as fun triple not use recursion *)
val triple' = fn y => 3*y

val triple_n_times'' = fn (n,x) => n_times(fn y => 3*y,n,x)

(* 
    fun rev xs = List.rev xs
    val rev = fn xs => List.rev xs 
*)
val rev = List.rev
