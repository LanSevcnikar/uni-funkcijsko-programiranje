fun reduce f z xs =
  case xs of
    [] => z
  | y::ys => reduce f (f z y) ys


fun squares(xs: int list) : int list =
  List.map (fn x => x * x) xs

fun onlyEven(xs: int list) : int list =
  List.filter (fn x => x mod 2 = 0) xs

fun bestString f xs =
  List.foldl (fn (s, best) => if f (s, best) then s else best) "" xs


fun largestString xs : string =
  bestString (fn (s1, s2) => s1 > s2) xs

fun longestString xs : string =
  bestString (fn (s1, s2) => String.size s1 > String.size s2) xs


fun quicksort cmp xs =
  case xs of
    [] => []
    | pivot::rest =>
      let
        val lessOrEqual = List.filter (fn x => cmp (x, pivot) <> GREATER) rest
        val greater = List.filter (fn x => cmp (x, pivot) = GREATER) rest
      in
        quicksort cmp lessOrEqual @ (pivot :: quicksort cmp greater)
      end

fun dot xs ys =
  case (xs, ys) of
    ([], []) => 0
    | (x::xt, y::yt) => x * y + dot xt yt
    | _ => 0

fun transpose m =
  case m of
    [] => []
    | []::_ => []
    | _ =>
      let
        val firstCol = List.map List.hd m
        val restRows = List.map List.tl m
      in
        firstCol :: transpose restRows
      end


fun multiply m1 m2 =
  let
    val m2t = transpose m2
  in
    List.map (fn row => List.map (fn col => dot row col) m2t) m1
  end

fun group (xs: ''a list) : (''a * int) list =
  let
    fun helper (zs: ''a list, (current: ''a, count: int)) : (''a * int) list =
      case zs of
        [] => [(current, count)]
        | y::ys =>
          if y = current then
            helper (ys, (current, count + 1))
          else
            (current, count) :: helper (ys, (y, 1))
  in
    case xs of
      [] => []
      | y::ys => helper (ys, (y, 1))
  end

fun equivalenceClasses f xs =
  let
    fun insertIntoClasses xss x =
      case xss of
        [] => [[x]]
        | ys::yss =>
          if f (List.hd ys, x) then
            (x :: ys) :: yss
          else
            ys :: insertIntoClasses yss x
    in
      reduce insertIntoClasses [] xs
    end