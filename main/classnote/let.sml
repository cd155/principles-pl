(*
    Let expression
*)
fun silly1(z: int) =
    let
        val x = if z > 0 then z else 34
        val y = x+z+9
    in
        if x > y then x * 2 else y * y
    end

fun silly2() =
    let 
        val x = 1
    in
        (let val x = 2 in x+1 end) + 
        (* shadow the x *)
        (let val y = x+2 in y+1 end)
        (* this x is 1 *)
    end

(* move this to local scope *)
fun count(from: int, to: int) =
    if from=to
    then to::[]
    else from :: count(from+1, to)

fun countup_from1(x: int) =
    let
        fun count(from: int) =
            if from=x
            then x::[]
            else from :: count(from+1)
    in
        count(1)
    end

fun bad_max(xs: int list) =
    if null xs
    then 0
    else if null(tl xs)
    then hd xs
    else if hd xs > bad_max(tl xs)
    then hd xs
    else bad_max(tl xs)

fun good_max(xs: int list) =
    if null xs
    then 0 
    else if null (tl xs)
    then hd xs
    else
        let val tl_ans = good_max(tl xs)
        in
            if hd xs > tl_ans
            then hd xs
            else tl_ans
        end

fun max1(xs: int list) =
    if null xs
    then NONE
    else
        let val tl_ans = max1(tl xs)
        in
            if isSome tl_ans andalso valOf tl_ans > hd xs
            then tl_ans
            else SOME(hd xs)
        end

fun max2(xs: int list) =
    if null xs
    then NONE
    else
        let
            fun max_nonempty(xs: int list) =
                if null(tl xs)
                then hd xs
                else
                    let val tl_ans = max_nonempty(tl xs)
                    in
                        if hd xs > tl_ans
                        then hd xs
                        else tl_ans
                    end
        in
            SOME (max_nonempty xs)
        end
