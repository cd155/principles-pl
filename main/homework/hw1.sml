(* 
    Write a function is_older that takes two dates and evaluates to true or 
    false. It evaluates to true if the first argument is a date that comes 
    before the second argument. (If the two dates are the same, the result 
    is false.) 
*)
fun is_older (date1: int*int*int, date2: int*int*int) = 
    if #1 date1 < #1 date2
    then true
    else if #1 date1 > #1 date2
    then false
    else (* when year equal *)
        if #2 date1 < #2 date2
        then true
        else if #2 date1 > #2 date2
        then false
        else (* when month equal *)
            if #3 date1 < #3 date2
            then true
            else if #2 date1 > #2 date2
            then false
            else false (* when day equal *)

fun is_older' (date1: int*int*int, date2: int*int*int) = 
    if #1 date1 = #1 date2
    then
        if #2 date1 = #2 date2
        then 
            if #3 date1 = #3 date2
            then
                false
            else
                #3 date1 < #3 date2
        else
            #2 date1 < #2 date2
    else 
        #1 date1 < #1 date2

fun is_older'' (date1: int*int*int, date2: int*int*int) = 
    let
        val year = (#1 date1, #1 date2)
        val month = (#2 date1, #2 date2)
        val date = (#3 date1, #3 date2)
        val xs = [year, month, date]

        fun is_older_Helper (xs: (int*int) list) =
            if null xs 
            then false
            else
                if #1 (hd xs) = #2 (hd xs) 
                then true andalso is_older_Helper (tl xs)
                else #1 (hd xs) < #2 (hd xs)
    in
        is_older_Helper xs
    end

(* 
    Write a function number_in_month that takes a list of dates and 
    a month (i.e., an int) and returns how many dates in the list are 
    in the given month.
*)
fun number_in_month (dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else
        if #2 (hd dates) = month
        then 1 + number_in_month (tl dates, month)
        else number_in_month (tl dates, month)

(* 
    Write a function number_in_months that takes a list of dates and 
    a list of months (i.e., an int list) and returns the number of dates 
    in the list of dates that are in any of the months in the list of months.
    Assume the list of months has no number repeated.
*)
fun number_in_months (dates: (int*int*int) list, month: int list) =
    if null month
    then 0
    else number_in_month (dates, hd month) + number_in_months (dates, tl month)
