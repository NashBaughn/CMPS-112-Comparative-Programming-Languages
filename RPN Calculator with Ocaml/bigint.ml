(* 
nbaughn@ucsc.edu
Nash Baughn
CS 112 Sping Quarter 2017 *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])


    let trimzeros number =
        let rec trimzeros' number' = match number' with
            | []       -> []
            | [0]      -> []
            | car::cdr ->
                 let cdr' = trimzeros' cdr
                 in  match car, cdr' with
                    | 0, [] -> []
                    | car, cdr' -> car::cdr'
        in trimzeros' number

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))
   
   

    (*recursive function for cmp*)
    let rec cmp' list1 list2 = match (list1, list2) with
        | [], []                 ->  0
        | list1, []              ->  1
        | [], list2              -> -1
        | car1::cdr1, car2::cdr2 -> 
            let return_value = cmp' cdr1 cdr2
            in if return_value = 0 && car1 != car2 then 
                if car1 > car2 then 1
                else if car1 < car2 then -1
                else 0
            else return_value

    (*Compares to bigints and returns 1 or -1, doesnt handle equality*)
    let cmp (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        if neg1 = neg2 then cmp' val1 val2
        else if neg1 = Neg 
            then -1
            else 1

    (*recursive function for addition*)
    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1 
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry -> 
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    (*recursive subtraction function*)
    let rec sub' list1 list2 steal = match (list1, list2, steal) with
        | list1, [], 0           -> list1
        | [], list2, 0           -> failwith "error in bigint.sub: list2 > list"
        | car1::cdr1, [], steal  -> 
          if car1 = 0
          then 9 :: (sub' cdr1 [] 1)
          else let dif = car1 - steal*1
              in dif :: (sub' cdr1 [] 0)
        | [], list2, steal              -> failwith "error in bigint.sub: non exhaustive pattern encountered"
        | car1::cdr1, car2::cdr2, steal ->
          
          if car2 > (car1 - steal*1) then 
            let dif = ((car1 + 10) - steal*1) - car2
              in dif :: (sub' cdr1 cdr2 1)
          else 
            let dif = (car1 - steal*1) - car2
              in dif :: (sub' cdr1 cdr2 0)
    
    (*adds two bigints together*)
    let add (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        if neg1 = neg2
        then 
            Bigint (neg1, add' val1 val2 0)
        else 
            if (cmp' val1 val2) = 1
                then Bigint (neg1, trimzeros(sub' val1 val2 0))
            else Bigint (neg2, trimzeros(sub' val2 val1 0))

    (*Subtracts a bigint from another bigint depending on abosulte value*)
    let sub (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        if neg1 = neg2 then 
            if cmp' val2 val1 = 1 then 
                Bigint (Neg, trimzeros(sub' val2 val1 0))
            else 
                Bigint (Pos, trimzeros(sub' val1 val2 0))
        else 
            if cmp' val1 val2 = 1 then 
                Bigint (neg2, add' val1 val2 0)
            else 
                Bigint (neg1, add' val2 val1 0)

     
    (* Doubles bigint  *)
    let double value =
        add' value value 0

    (*recursive mul function*)
    let rec mul' multiplier powerof2 multiplicand' =
        if (cmp' powerof2 multiplier) = 1 then 
            multiplier, []
        else let remainder, product =
            mul' multiplier (double powerof2) (double multiplicand')
            in if (cmp' powerof2 remainder) = 1 then 
                remainder, product
            else 
                trimzeros (sub' remainder powerof2 0), (add' product multiplicand' 0)

    (*multiplies two bigints*)
    let mul (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        let  _, product = mul' val1 [1] val2 in
                if neg1 = neg2 then 
                    Bigint (Pos, product)
                else 
                    Bigint (Neg, product)
    
    (*recursive division fucntion*)
    let rec divrem' (dividend, powerof2, divisor) =
        if cmp' divisor dividend = 1 then 
            [0], dividend
        else let quotient, remainder = divrem' (dividend, double powerof2, double divisor)
            in  if (cmp' divisor remainder) = 1 then 
                quotient, remainder
            else (add' quotient powerof2 0), (trimzeros (sub' remainder divisor 0))

    (*reutnrs tuple of bigint divsion: (quotient, remainder)*)
    let divrem ((Bigint (neg1, val1)), (Bigint (neg2, val2))) =
        let quotient, remainder = divrem' (val1, [1], val2)
        in if neg1 = neg2 then 
            Bigint (Pos, quotient), Bigint (Pos, remainder)
        else 
            Bigint (Neg, quotient), Bigint (Pos, remainder)

    (*bool value for whether or not bigint is even*)
    let even (Bigint (neg, number)) =
        if car number mod 2 == 0 then
            true
        else
            false

    (*returns quotient of bigint divisoni*)
    let div (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        let quotient,_ = divrem ((Bigint (neg1, val1)), (Bigint (neg2, val2)))
            in quotient

    (*returns reaminder of bigint division*)
    let rem (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        let _, remainder = divrem ((Bigint (neg1, val1)), (Bigint (neg2, val2)))
            in remainder

    (* recursive pow fucntion *)
    let rec pow' ((Bigint (neg1, base)), (Bigint (neg2, expt)),
        (Bigint (neg3, result))) =
        match (Bigint (neg2, expt)) with
            | (Bigint (neg2, expt)) when
                        (cmp (Bigint (neg2, expt)) zero) = 0  ->
                (Bigint (neg3, result))
            | (Bigint (neg2, expt)) when even (Bigint (neg2, expt)) ->
                pow' (mul (Bigint (neg1, base)) (Bigint (neg1, base)),
                    (div (Bigint (neg2, expt)) (Bigint (Pos, [2]))),
                    (Bigint (neg3, result)))
            | (Bigint (neg2, expt)) ->
                pow' ((Bigint (neg1, base)),
                    (sub (Bigint (neg2, expt)) (Bigint (Pos, [1]))),
                    (mul (Bigint (neg1, base)) (Bigint (neg3, result))))

    (*power function that raises first val to power of second val*)
    let pow (Bigint (neg1, val1)) (Bigint (neg2, val2)) =
        if (cmp (Bigint (neg2, val2)) zero) = -1 then 
            pow' ( (div (Bigint (Pos, [1])) (Bigint (neg1, val1))), (Bigint (neg2, val2)), (Bigint (Pos, [1])) )
            else pow' ((Bigint (neg1, val1)), (Bigint (neg2, val2)), (Bigint (Pos, [1])))

end
