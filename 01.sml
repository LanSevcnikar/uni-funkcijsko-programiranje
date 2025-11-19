fun gcd (a : IntInf.int, b : IntInf.int) : IntInf.int =
  if b = 0 then a
  else gcd (b, IntInf.mod (a, b))

fun len (xs : int list) : int =
  case xs of
    [] => 0
  | _::rest => 1 + len rest

fun last (xs : int list) : int option =
  case xs of
    [] => NONE
  | [x] => SOME x
  | _::rest => last rest

fun nth (xs : int list, n : int) : int option =
    case (xs, n) of
        ([], _) => NONE
    | (x::rest, 0) => SOME x
    | (_::rest, n) => nth (rest, n - 1)

fun insert (xs : int list, n : int, x : int) : int list = 
    case (xs, n) of
        ([], 0) => [x]
    | ([], _) => raise Fail "ob"
    | (y::rest, 0) => x :: y :: rest
    | (y::rest, n) => y :: insert (rest, n - 1, x)

fun delete (xs : int list, x : int) : int list =
    case xs of
        [] => []
    | y::rest => if y = x then delete (rest, x) else y :: delete (rest, x)

fun reverse (xs : int list) : int list =
    case xs of
        [] => []
    | y::rest => reverse rest @ [y]

fun factors (n : IntInf.int) : IntInf.int list =
  let
    fun loop (n, d, acc) =
      if n = 1 then List.rev acc
      else if n mod d = 0 then loop (n div d, d, d :: acc)
      else if d * d > n then List.rev (n :: acc)
      else loop (n, d + 1, acc)
  in
    loop (n, 2, [])
  end;

fun primes (n : int) : int list
    =
    let
        fun gen_candidates (m : int) : int list =
            if m < 2 then []
            else gen_candidates (m - 1) @ [m]

        fun del_all_multiples (candidates : int list, p : int) : int list =
            case candidates of
                [] => []
              | x::xs =>
                  if x mod p = 0 then del_all_multiples (xs, p)
                  else x :: del_all_multiples (xs, p)      

        fun sieve (candidates : int list) : int list =
            case candidates of
                [] => []
              | p::xs =>
                    sieve (del_all_multiples (xs, p)) @ [p] 
    in
        sieve (gen_candidates (n - 1))
    end

fun palindrome (xs : int list) : bool =
    let
        fun is_palindrome ([], []) = true
          | is_palindrome (x::xs, y::ys) = if x = y then is_palindrome (xs, ys) else false
    in
        is_palindrome (xs, reverse xs)
    end

fun aa_lookup (key : string, aa : (string * string) list) : string option =
    case aa of
        [] => NONE
      | (k,v)::rest => if k = key then SOME v else aa_lookup (key, rest)


fun aa_insert (key : string, value : string, aa : (string * string) list) : (string * string) list =
    case aa of
        [] => [(key, value)]
      | (k,v)::rest =>
          if k = key then (key, value) :: rest
          else (k,v) :: aa_insert (key, value, rest)  

fun aa_remove (key : string, aa : (string * string) list) : (string * string) list =
    case aa of
        [] => []
      | (k,v)::rest =>
          if k = key then rest
          else (k,v) :: aa_remove (key, rest)

fun period (xs : int list) : int list = 
    let
        fun check_if_period (prefix : int list, temp_prefix : int list, other_list : int list) : bool =
            if prefix = [] then false (* This is done so that it does not assume empty prefix*)
            else
                case (temp_prefix, other_list) of
                    ([], _) => check_if_period (prefix, prefix, other_list)
                | (_, []) => true
                | (p::ps, o'::os) =>
                    if p = o' then check_if_period (prefix, ps, os)
                    else false

        fun find_period (prefix : int list, whole_list : int list) : int list =
            if check_if_period (prefix, prefix, whole_list) then prefix
            else find_period (prefix @ [hd whole_list], tl whole_list)
    in
        if (xs = []) then []
        else find_period ([], xs)
    end

(*TEST:*)
fun assert (cond: bool) : unit =
  if not cond then raise Fail "Assertion failed"
  else print "Test passed\n"

(* Gcd *)
val _ =
  let
    val _ = assert (gcd (48, 18) = 6)
    val _ = assert (gcd (14932580758540567828011676545491041052243656730243666633531296131932103903026426841775985642,
                         15241578780673682728395029728395029728395029728395039836610159071180833) =
                      123456789123456789123456789123456789123456789123456871)
  in () end

(* Len *)
val _ =
  let
    val _ = assert (len [] = 0)
    val _ = assert (len [1,2,3,4,5] = 5)
  in () end 

(* Last *)
val _ =
  let
    val _ = assert (last [] = NONE)
    val _ = assert (last [1,2,3] = SOME 3)
  in () end

(* Nth *)
val _ =
  let
    val _ = assert (nth ([], 0) = NONE)
    val _ = assert (nth ([10,20,30,40], 2) = SOME 30)
    val _ = assert (nth ([10,20,30,40], 4) = NONE)
  in () end 

(* Insert *)
val _ =
  let
    val _ = assert (insert ([], 0, 5) = [5])
    val _ = assert (insert ([1,2,3], 1, 10) = [1,10,2,3])
  in () end

(* Delete *)
val _ =
  let
    val _ = assert (delete ([], 5) = [])
    val _ = assert (delete ([1,2,3,2,4], 2) = [1,3,4])
  in () end 

(* Reverse *)
val _ =
  let
    val _ = assert (reverse [] = [])
    val _ = assert (reverse [1,2,3] = [3,2,1])
  in () end

(* Factors *)
val _ =
  let
    val _ = assert (factors 60 = [2,2,3,5])
    val _ = assert (factors 134124121251251235 = [3,3,5,13,73,20063,156542509])
  in () end

(* Primes *)
val _ =
  let
    val _ = assert (primes 10 = [7,5,3,2])
    val _ = assert (primes 30 = [29,23,19,17,13,11,7,5,3,2])
  in () end

(* Palindrome *)
val _ =
  let
    val _ = assert (palindrome [] = true)
    val _ = assert (palindrome [1,2,3,2,1] = true)
    val _ = assert (palindrome [1,2,3,4,5] = false)
  in () end

(* Dictionary *)
val _ =
  let
    val aa = [("a", "1"), ("b", "2"), ("c", "3")]
    val _ = assert (aa_lookup ("b", aa) = SOME "2")
    val _ = assert (aa_lookup ("d", aa) = NONE)
    val aa2 = aa_insert ("b", "20", aa)
    val _ = assert (aa_lookup ("b", aa2) = SOME "20")
    val aa3 = aa_remove ("a", aa2)
    val _ = assert (aa_lookup ("a", aa3) = NONE)
  in () end

(* Perioda *)
val _ =
  let
    val _ = assert (period [1,2,3,1,2,3,1,2,3] = [1,2,3])
    val _ = assert (period [1,2,3,1,2,3,1,2,3,1] = [1,2,3])
    val _ = assert (period [1,2,3,1,2,3,1,2,3,1000] = [1,2,3,1,2,3,1,2,3,1000])
    val _ = assert (period [1,2,3,1,2,3,4,1,2,3,1,2,3] = [1,2,3,1,2,3,4])
    val _ = assert (period [4,1,2,3,1,2,3,1,2,3] = [4,1,2,3,1,2,3,1,2,3])
  in () end
