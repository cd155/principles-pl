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

fun number_in_month (dates: (int*int*int) list, month: int) =
    if null dates
    then 0
    else
        if #2 (hd dates) = month
        then 1 + number_in_month (tl dates, month)
        else number_in_month (tl dates, month)
