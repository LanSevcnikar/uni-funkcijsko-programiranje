datatype natural = Succ of natural | One;
exception NotNaturalNumber;

datatype 'a bstree = br of 'a bstree * 'a * 'a bstree | lf;
datatype direction = L | R;

fun zip (xs : 'a list, ys : 'b list) : ('a * 'b) list =
    case (xs, ys) of
        ([], _) => []
      | (_, []) => []
      | (x::rest_xs, y::rest_ys) => (x, y) :: zip (rest_xs, rest_ys)

fun unzip (pairs : ('a * 'b) list) : ('a list * 'b list) =
    case pairs of
        [] => ([], [])
      | (x, y)::rest =>
            let
                val (xs, ys) = unzip rest
            in
                (x::xs, y::ys)
            end

fun assert (cond: bool) : unit =
    if not cond then
        raise Fail "Assertion failed"
    else
        print "Test passed\n"

fun subtract(a : natural, b : natural) : natural =
    case (a, b) of
        (Succ n1, One) => n1
      | (Succ n1, Succ n2) => subtract (n1, n2)
      | (One, _) => raise NotNaturalNumber;

fun any(f : 'a -> bool, s : 'a list) : bool =
    case s of
        [] => false
      | x::rest => if f x then true else any (f, rest);

fun map(f : 'a -> 'b, s : 'a list) : 'b list =
    case s of
        [] => []
      | x::rest => f x :: map (f, rest);

fun filter(f : 'a -> bool, s : 'a list) : 'a list =
    case s of
        [] => []
      | x::rest => if f x then x :: filter (f, rest) else filter (f, rest);

fun fold(f: ('a * 'b) -> 'a, z : 'a, s : 'b list) : 'a =
    case s of
        [] => z
      | x::rest => fold (f, f (z, x), rest);

fun rotate (drevo : 'a bstree, smer : direction) : 'a bstree =
    case smer of
        L =>
            (case drevo of
                lf => lf
              | br (lf, x, r) => drevo   (* idk if I can skip here but ok *)
              | br (br (ll, y, lr), x, r) => br (ll, y, br (lr, x, r)))
      | R =>
            (case drevo of
                lf => lf
              | br (l, x, lf) => drevo   (* Same here *)
              | br (l, x, br (rl, y, rr)) => br (br (l, x, rl), y, rr));
    
fun rebalance(drevo : 'a bstree) : 'a bstree =
    let
        fun height t =
            case t of
                lf => 0
              | br (l, _, r) => 1 + Int.max (height l, height r)

        val balance =
            case drevo of
                lf => 0
              | br (l, _, r) => height l - height r
    in
        case drevo of
            lf => lf
          | br (l, x, r) =>
                if balance > 1 then
                    rotate (drevo, R)  
                else if balance < ~1 then
                    rotate (drevo, L)
                else
                    drevo
    end



fun avl (cmp : 'a * 'a -> order, tree : 'a bstree, value : 'a) : 'a bstree =
    let
        fun insert_avl (cmp : 'a * 'a -> order, tree : 'a bstree, value : 'a) : 'a bstree =
            case tree of
                lf => br (lf, value, lf)
              | br (l, x, r) =>
                    (case cmp (value, x) of
                         LESS => rebalance (br (insert_avl (cmp, l, value), x, r))
                       | GREATER => rebalance (br (l, x, insert_avl (cmp, r, value)))
                       | EQUAL => tree)

    in
        rebalance (insert_avl (cmp, tree, value))
    end

fun rotate (drevo : 'a bstree, smer : direction) : 'a bstree =
    case smer of
        L =>
            (case drevo of
                br (l, x, br (rl, y, rr)) => br (br (l, x, rl), y, rr)
              | _ => drevo)
      | R =>
            (case drevo of
                br (br (ll, y, lr), x, r) => br (ll, y, br (lr, x, r))
              | _ => drevo)

fun rebalance drevo =
    let
        fun height t =
            case t of
                lf => 0
              | br (l, _, r) => 1 + Int.max (height l, height r)
        val balance =
            case drevo of
                lf => 0
              | br (l, _, r) => height l - height r
    in
        case drevo of
            lf => lf
          | _ =>
                if balance > 1 then rotate (drevo, R)
                else if balance < ~1 then rotate (drevo, L)
                else drevo
    end

fun avl (cmp, tree, value) =
    let
        fun insert_avl (cmp, tree, value) =
            case tree of
                lf => br (lf, value, lf)
              | br (l, x, r) =>
                    (case cmp (value, x) of
                        LESS => rebalance (br (insert_avl (cmp, l, value), x, r))
                      | GREATER => rebalance (br (l, x, insert_avl (cmp, r, value)))
                      | EQUAL => tree)
    in
        rebalance (insert_avl (cmp, tree, value))
    end
