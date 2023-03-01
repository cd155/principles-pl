(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)
(* 1 - a *)
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

(* 1 - b *)
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

(* 1 - c *)
fun get_substitutions2(sllst: string list list, s: string ) =
    get_substitutions2_aux(sllst, s, [])

(* 1 - d *)
fun similar_names(sllst: string list list, 
                  {first=f_name,middle=m_name,last=l_name}) =
    
    let val names = get_substitutions1(sllst, f_name)
        fun similar_names_aux(names: string list) =
            case names of
                  [] => []
                | x::xs => 
                    {first=x, middle=m_name,last=l_name}::(similar_names_aux xs)
    in 
       {first=f_name,middle=m_name,last=l_name}::similar_names_aux names
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
(* 2 - a *)
fun card_color(mycard: card) =
    case mycard of
          (Clubs,_) => Black
        | (Spades,_) => Black
        | (Diamonds,_) => Red
        | (Hearts,_) => Red

(* 2 - b *)
fun card_value(mycard: card) =
    case mycard of
          (_, Ace) => 11
        | (_, Num n) => n
        | (_,_) => 10

(* 2 - c *)
fun remove_card(cs: card list, c: card, exn) =
    case cs of 
          [] => raise exn
        | (x::xs) => 
            if x = c 
            then xs 
            else x::remove_card(xs, c, exn)

(* 2 - d *)
fun all_same_color(cs: card list) = 
    case cs of 
          [] => true
        | [x] => true 
        | (x1::x2::xs) => 
            if (card_color(x1) = card_color(x2)) 
            then true andalso all_same_color(x2::xs)
            else false

(* 2 - e *)
fun sum_cards(cs: card list) =
    case cs of 
        [] => 0
        | (x::xs) => card_value(x) + sum_cards(xs)

(* 2 - f *)
fun score(cs: card list, g: int) = 
    let val res = sum_cards cs
    in
        let val pre = if res > g then 3 * (res - g) else (g-res)
        in
            if all_same_color cs
            then pre div 2
            else pre
        end
    end

(* 2 - g *)
fun officiate_aux(cs: card list, ms: move list, hcs: card list, g: int) =
    case (cs,ms) of
          ([],_) => score(hcs,g)
        | (_,[]) => score(hcs,g)
        | ((x::xs),(Draw::ys)) =>
            if (sum_cards(x::hcs) > g)
            then score((x::hcs),g)
            else officiate_aux(xs, ys, (x::hcs), g)
        | (xs,((Discard c)::ys)) => 
            officiate_aux (xs, ys, (remove_card(hcs, c, IllegalMove)), g)

fun officiate(cs: card list, ms: move list, g: int) = 
    officiate_aux(cs, ms, [], g)

fun card_value'(mycard: card) =
    case mycard of
          (_, Ace) => 1
        | (_, Num n) => n
        | (_,_) => 10

fun sum_cards'(cs: card list) =
    case cs of 
        [] => 0
        | (x::xs) => card_value'(x) + sum_cards'(xs)

(* c - a *)
fun score_challenge'(cs: card list, g: int) =
    let val res1 = sum_cards cs
        val res2 = sum_cards' cs
        val pre1 = if res1 > g then 3 * (res1 - g) else (g-res1)
        val pre2 = if res2 > g then 3 * (res2 - g) else (g-res2)
        fun min(x, y) = if x < y then x else y 
    in
        if all_same_color cs
        then min(pre1 div 2, pre2 div 2)
        else min(pre1, pre2)
    end

fun possible_zero_move_aux(c: card, hcs: card list, passed_cs: card list, g:int) =
    case hcs of
          [] => NONE
        | (x::xs) => if (score(passed_cs@(c::xs), g)=0) 
                     then SOME x
                     else possible_zero_move_aux(c,xs,x::passed_cs,g)

fun possible_zero_move(c: card, hcs: card list, g:int) =
    possible_zero_move_aux(c,hcs,[],g)

fun careful_player_aux(cs: card list, hcs: card list, g: int) =
    case (cs,hcs) of 
          ([],_) => []
        | ((x::xs),[]) => Draw::(careful_player_aux(xs,(x::hcs),g))
        | ((x::xs),(y::ys)) =>
            let val pc = possible_zero_move(x,hcs,g)
            in 
                if score(hcs,g) <= g
                then
                    if score(hcs,g) < (g-10)
                    then Draw::(careful_player_aux(xs,(x::hcs),g))
                    else if score(hcs,g) = 0
                    then []
                    else if (isSome pc)
                    then 
                        let val removed_cs = remove_card(hcs, valOf pc, IllegalMove)
                        in
                            Draw::(Discard (valOf pc))::(careful_player_aux(xs,removed_cs,g))
                        end
                    else 
                        (Discard y)::(careful_player_aux(cs,tl hcs,g))
                else []
            end

(* c - b *)
fun careful_player(cs: card list, g: int) = 
    careful_player_aux(cs,[],g)
