signature RATIONAL =
sig
    eqtype rational
    exception BadRational
    val makeRational: int * int -> rational
    val neg: rational -> rational
    val inv: rational -> rational
    val add: rational * rational -> rational
    val mul: rational * rational -> rational
    val toString: rational -> string
end


structure Rational :> RATIONAL =
struct
    type rational = int * int
    exception BadRational

    fun simplify(n, d) =
        let
            fun gcd(a, b) =
                if b = 0 then abs a
                else gcd(b, a mod b)

            val g = gcd(n, d)
            val n' = n div g
            val d' = d div g
        in
            if d' < 0 then (~n', ~d') else (n', d')
        end

    fun makeRational (n, d) =
        if d = 0 then raise BadRational
        else simplify(n, d)

    fun neg ((n, d): rational) = (~n, d)

    fun inv ((n, d): rational) =
        if n = 0 then raise BadRational
        else simplify(d, n)
    
    fun add ((n1, d1): rational, (n2, d2): rational) =
        simplify(n1 * d2 + n2 * d1, d1 * d2)

    fun mul ((n1, d1): rational, (n2, d2): rational) =
        simplify(n1 * n2, d1 * d2)

    fun toString ((n, d): rational) : string =
        Int.toString n ^ "/" ^ Int.toString d
end

