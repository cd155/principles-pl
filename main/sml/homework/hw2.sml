(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
fun all_except_option(s1: string, slst: string list) =
    case slst of
          [] => NONE
        | x::xs =>
            if same_string(s1,x)
            then SOME (xs)
            else
                let val res = all_except_option (s1, xs)
                in
                    case res of
                          NONE => NONE
                        | SOME n => SOME (x::n)
                end

fun get_substitutions1(sllst: string list list, s: string ) =
    case sllst of
          [] => []
        | lx::lxs => 
            let val res = all_except_option(s, lx)
            in
                case res of
                      NONE => get_substitutions1(lxs, s)
                    | SOME n => n @ get_substitutions1(lxs, s)
            end

fun get_substitutions2_aux(sllst: string list list, s: string, acc:string list) =
    case sllst of
          [] => acc
        | lx::lxs => 
            let val res = all_except_option(s, lx)
            in
                case res of
                      NONE => get_substitutions2_aux(lxs, s, acc)
                    | SOME n => get_substitutions2_aux(lxs, s, (acc @ n))
            end

fun get_substitutions2(sllst: string list list, s: string ) =
    get_substitutions2_aux(sllst, s, [])

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)