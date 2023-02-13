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

(* 
    Write a function dates_in_month that takes a list of dates and a month 
    (i.e., an int) and returns a list holding the dates from the argument 
    list of dates that are in the month. The returned list should contain 
    dates in the order they were originally given.
*)
fun dates_in_month (dates: (int*int*int) list, month: int) =
    if null dates
    then []
    else 
        if #2 (hd dates) = month
        then hd dates :: dates_in_month (tl dates, month)
        else dates_in_month (tl dates, month)

(* 
    Write a function dates_in_months that takes a list of dates and a list 
    of months (i.e., an int list) and returns a list holding the dates from 
    the argument list of dates that are in any of the months in the list of 
    months. Assume the list of months has no number repeated. 
    Hint: Use your answer to the previous problem and 
          SML’s list-append operator (@).
*)
fun dates_in_months (dates: (int*int*int) list, month: int list) =
    if null month
    then []
    else dates_in_month (dates, hd month) @ dates_in_months (dates, tl month)

(* 
    Write a function get_nth that takes a list of strings and an int n and 
    returns the nth element of the list where the head of the list is 1st. 
    Do not worry about the case where the list has too few elements: your 
    function may apply hd or tl to the empty list in this case, which is 
    okay.
*)
fun get_nth (chars: string list, n: int) = 
    let
        (* xs can not be empty *)
        fun get_nth_helper (xs: string list, n: int, i: int) = 
            if n = i 
            then hd xs
            else get_nth_helper (tl xs, n, i+1)
    in
        get_nth_helper (chars, n, 1)
    end

(* 
    Write a function date_to_string that takes a date and returns a string 
    of the form January 20, 2013 (for example). Use the operator ^ for 
    concatenating strings and the library function Int.toString for converting 
    an int to a string. For producing the month part, do not use a bunch of 
    conditionals. Instead, use a list holding 12 strings and your answer to 
    the previous problem. For consistency, put a comma following the day and 
    use capitalized English month names: January, February, March, April, May, 
    June, July, August, September, October, November, December.
*)

