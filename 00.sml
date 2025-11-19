(* Vrne naslednika števila `n`. *)
fun next (n : int) : int = n + 1

(* Vrne vsoto števil `a` in `b`. *)
fun add (a : int, b : int) : int = a + b

(* Vrne true, če sta vsaj dva argumenta true, drugače vrne false. *)
fun majority (a : bool, b : bool, c : bool) : bool =
  (a andalso b) orelse (a andalso c) orelse (b andalso c)

(* Vrne mediano argumentov - števila tipa real brez (inf : real), (~inf : real), (nan : real) in (~0.0 : real)
   namig: uporabi Real.max in Real.min *)
fun median (a : real, b : real, c : real) : real =
  Real.max (
    Real.max (
        Real.min (a, b), 
        Real.min (a, c)
    ),
    Real.min (b, c)
  )

(* https://en.cppreference.com/w/cpp/algorithm/clamp.html *)
fun clamp (value, low, high) : int = 
  if value < low then low
  else if value > high then high
  else value

(* Preveri ali so argumenti veljavne dolžine stranic nekega trikotnika - trikotnik ni izrojen. *)
fun triangle (a : int, b : int, c : int)  : bool =
  a + b > c andalso a + c > b andalso b + c > a

(* Izračuna binomski koeficient (n choose k). 0 <= k <= n *)
infix 5 choose
fun (n : IntInf.int) choose (k : IntInf.int) : IntInf.int =
  if k = 0 orelse k = n then 1
  else (n - k + 1) * (n choose (k - 1)) div k

(* Deli in zaokroži proč od ničle (10/3 -> 3, 3/2 -> 2, ~3/2 -> ~2), divisor > 0. *)
fun div_and_round (dividend : IntInf.int, divisor : IntInf.int) : IntInf.int =
  if (dividend >= 0) = (divisor >= 0) then
    IntInf.quot (dividend + IntInf.quot (divisor, 2), divisor)
  else
    IntInf.quot (dividend - IntInf.quot (divisor, 2), divisor)


(* 
Testiraj 
*)
fun assert (cond: bool) : unit =
  if not cond then raise Fail "Assertion failed"
  else print "Test passed\n"

fun areApproxEqual (x: real, y: real, eps: real) : bool =
  Real.abs (x - y) < eps

val _ =
  let
    val _ = assert (next 5 = 6)
    val _ = assert (add (3, 4) = 7)
    val _ = assert (majority (true, false, true) = true)
    val _ = assert (majority (false, false, true) = false)
    val _ = assert (areApproxEqual (median (3.0, 1.0, 2.0), 2.0, 0.0001))
    val _ = assert (clamp (5, 1, 10) = 5)
    val _ = assert (clamp (0, 1, 10) = 1)
    val _ = assert (clamp (15, 1, 10) = 10)
    val _ = assert (triangle (3, 4, 5) = true)
    val _ = assert (triangle (1, 2, 3) = false)
    val _ = assert (5 choose 2 = 10)
    val _ = assert (div_and_round (10, 3) = 3)
    val _ = assert (div_and_round (3, 2) = 2)
    val _ = assert (div_and_round (~3, 2) = ~2)
    val _ = assert (div_and_round (10, 3) = 3)
    val _ = assert (div_and_round (3, 2) = 2)
    val _ = assert (div_and_round (~3, 2) = ~2)
  in
    ()
  end