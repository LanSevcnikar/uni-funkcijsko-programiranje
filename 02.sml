datatype number = Zero | Succ of number | Pred of number;

fun neg (a : number) : number =
    case a of
        Zero => Zero
        | Succ n => Pred (neg n)
        | Pred n => Succ (neg n)

fun add (a : number, b : number) : number =
    case (a, b) of
        (Zero, _) => b
      | (Succ n1, _) => Succ (add (n1, b))
      | (Pred n1, _) => Pred (add (n1, b))

fun simp (n : number) : number =
    case n of
        Zero => Zero
      | Succ n1 =>
          (case simp n1 of
              Zero => Succ Zero
            | Succ n2 => Succ (Succ n2)
            | Pred n2 => n2)
      | Pred n1 =>
          (case simp n1 of
              Zero => Pred Zero
            | Succ n2 => n2
            | Pred n2 => Pred (Pred n2))

fun comp (a : number, b : number) : order =
    case (simp a, simp b) of
        (Zero, Zero) => EQUAL
      | (Succ n1, Zero) => GREATER
      | (Zero, Succ n2) => LESS
      | (Pred n1, Zero) => LESS
      | (Zero, Pred n2) => GREATER
      | (Succ n1, Succ n2) => comp (n1, n2)
      | (Pred n1, Pred n2) => comp (n1, n2)
      | (Succ n1, Pred n2) => GREATER
      | (Pred n1, Succ n2) => LESS


(*TEST*)
fun assert (cond: bool) : unit =
  if not cond then raise Fail "Assertion failed"
  else print "Test passed\n"


(*Neg*)
val _ =
    let
        val _ = assert (neg (Succ (Succ Zero)) = Pred (Pred Zero))
        val _ = assert (neg (Pred (Pred Zero)) = Succ (Succ Zero))
        val _ = assert (neg Zero = Zero)
    in
        ()
    end

(*Simp*)    
val _ = 
    let
        val _ = assert (simp (Pred (Succ (Succ (Pred (Pred (Succ (Pred Zero))))))) = Pred Zero)
        val _ = assert (simp (Succ Zero) = Succ Zero)
        val _ = assert (simp (Pred (Succ Zero)) = Zero)
        val _ = assert (simp (Succ (Pred Zero)) = Zero)
        val _ = assert (simp (Succ (Succ (Pred (Pred Zero)))) = Zero)
    in
        ()
    end


(*Add*)
val _ =
    let
        val _ = assert (simp (add (Succ (Succ Zero), Succ (Succ (Succ Zero)))) = Succ (Succ (Succ (Succ (Succ Zero)))))
        val _ = assert (simp (add (Pred (Pred Zero), Pred (Pred (Pred Zero)))) = Pred (Pred (Pred (Pred (Pred Zero)))))
        val _ = assert (simp (add (Succ (Succ Zero), Pred (Pred Zero))) = Zero)
        val _ = assert (simp (add (Succ (Succ (Succ Zero)), Pred (Pred Zero))) = Succ Zero)
    in
        ()
    end

(*Comp*)
val _ =
    let
        val _ = assert (comp (Succ (Succ Zero), Succ (Succ (Succ Zero))) = LESS)
        val _ = assert (comp (Pred (Pred Zero), Pred (Pred (Pred Zero))) = GREATER)
        val _ = assert (comp (Succ (Succ Zero), Pred (Pred Zero)) = GREATER)
        val _ = assert (comp (Pred (Pred Zero), Succ (Succ Zero)) = LESS)
        val _ = assert (comp (Succ (Pred Zero), Succ (Pred Zero)) = EQUAL)
        val _ = assert (comp (Pred (Succ Zero), Pred (Succ Zero)) = EQUAL)
        val _ = assert (comp (Zero, Zero) = EQUAL)
    in
        ()
    end


(*=======================================*)

datatype tree = Node of int * tree * tree | Leaf of int;

fun contains (tree : tree, x : int) : bool =
    case tree of
        Leaf v => v = x
      | Node (v, left, right) =>
          if v = x then true
          else contains (left, x) orelse contains (right, x)

fun countLeaves (tree : tree) : int =
    case tree of
        Leaf v => 1
      | Node (v, left, right) =>
          countLeaves left + countLeaves right

fun countBranches (tree : tree) : int =
    case tree of
        Leaf v => 0
      | Node (v, left, right) =>
          2 + countBranches left + countBranches right

fun height (tree : tree) : int =
    case tree of
        Leaf v => 1
      | Node (v, left, right) =>
          1 + Int.max (height left, height right)

fun toList (tree : tree) : int list =
    case tree of
        Leaf v => [v]
      | Node (v, left, right) =>
          toList left @ [v] @ toList right

fun isBalanced (tree : tree) : bool =
    case tree of
        Leaf v => true
      | Node (v, left, right) =>
          let
              val hl = height left
              val hr = height right
          in
              isBalanced left andalso isBalanced right andalso
              Int.abs (hl - hr) <= 1
          end

fun isBST (tree : tree) : bool =
    let
        fun minValue (Leaf v) = v
          | minValue (Node (v, left, right)) = Int.min (v, minValue left)

        fun maxValue (Leaf v) = v
          | maxValue (Node (v, left, right)) = Int.max (v, maxValue right)

        fun isBSTHelper (Leaf v) = true
          | isBSTHelper (Node (v, left, right)) =
              isBSTHelper left andalso isBSTHelper right andalso
              (case left of
                  Leaf lv => lv < v
                | Node (_, _, _) => maxValue left < v) andalso
              (case right of
                  Leaf rv => rv > v
                | Node (_, _, _) => minValue right > v)
    in
        isBSTHelper tree
    end

(*TEST*)
(*Contains*)
val _ = 
    let 
        val _ = assert (contains (Node (5, Leaf 3, Leaf 7), 3) = true)
        val _ = assert (contains (Node (5, Leaf 3, Leaf 7), 5) = true)
        val _ = assert (contains (Node (5, Leaf 3, Leaf 7), 8) = false)
        val _ = assert (contains (Leaf 10, 10) = true)
        val _ = assert (contains (Leaf 10, 5) = false)
    in
        ()
    end

(*CountLeaves*)
val _ = 
    let 
        val _ = assert (countLeaves (Node (5, Leaf 3, Leaf 7)) = 2)
        val _ = assert (countLeaves (Node (5, Node (3, Leaf 1, Leaf 4), Leaf 7)) = 3)
        val _ = assert (countLeaves (Leaf 10) = 1)
        val _ = assert (countLeaves (Node (5, Leaf 3, Leaf 7)) = 2)
    in
        ()
    end

(*CountBranches*)
val _ = 
    let 
        val _ = assert (countBranches (Node (5, Leaf 3, Leaf 7)) = 2)
        val _ = assert (countBranches (Node (5, Node (3, Leaf 1, Leaf 4), Leaf 7)) = 4)
        val _ = assert (countBranches (Leaf 10) = 0)
    in
        ()
    end

(*Height*)
val _ = 
    let 
        val _ = assert (height (Node (5, Leaf 3, Leaf 7)) = 2)
        val _ = assert (height (Node (5, Node (3, Leaf 1, Leaf 4), Leaf 7)) = 3)
        val _ = assert (height (Leaf 10) = 1)
    in
        ()
    end

(*ToList*)
val _ = 
    let 
        val _ = assert (toList (Node (5, Leaf 3, Leaf 7)) = [3, 5, 7])
        val _ = assert (toList (Node (5, Node (3, Leaf 1, Leaf 4), Leaf 7)) = [1, 3, 4, 5, 7])
        val _ = assert (toList (Leaf 10) = [10])
    in
        ()
    end

(*IsBalanced*)
val _ = 
    let 
        val _ = assert (isBalanced (Node (5, Leaf 3, Leaf 7)) = true)
        val _ = assert (isBalanced (Node (5, Node (3, Leaf 1, Leaf 4), Leaf 7)) = true)
        val _ = assert (isBalanced (Node (5, Node (3, Node (1, Leaf 0, Leaf 2), Leaf 4), Leaf 7)) = false)
    in
        ()
    end

(*IsBST*)
val _ = 
    let 
        val _ = assert (isBST (Node (5, Leaf 3, Leaf 7)) = true)
        val _ = assert (isBST (Node (5, Node (3, Leaf 1, Leaf 4), Leaf 7)) = true)
        val _ = assert (isBST (Node (5, Node (6, Leaf 1, Leaf 4), Leaf 7)) = false)
    in
        ()
    end


(*=======================================*)

(* Dve "težji" nalogi. *)
val _ = Control.Print.printDepth := 2000;
val _ = Control.Print.printLength := 2000;

datatype set = S of set list

fun dfs (S []) = [0]
  | dfs (S l) = 
    let
      fun dfs' [] = []
        | dfs' (h :: t) = dfs h @ dfs' t
    in
      List.length l :: dfs' l 
    end

(*Print out examples of this function to std output.*)
(* Safe head extraction function *)
fun safe_hd lst = if null lst then ~1 else hd lst


(* Inverz funkcje dfs.
    - `dfs (fromDFS t) = t`
    - (opcijsko) Če ima seznam premalo števil, se predpostavi, da so manjkajoča števila enaka 0.
    - (opcijsko) Če ima seznam preveč števil, se ta ignorirajo.
*)


fun fromDFS (lst : int list) : set =
    let
        fun outer_rec_loop (in_list : int list) : (set * int list) =
            case in_list of
                [] => (S [], [])
              | h :: t =>
                  inner_rec_loop (t, h)

        and inner_rec_loop (in_list : int list, index: int) : (set * int list) =
            if index = 0 then (S [], in_list)
            else
                case in_list of
                    [] => (S [], [])
                  | _ =>
                      let
                          val (child_set, rest_list) = outer_rec_loop in_list
                          val (S siblings, final_list) = inner_rec_loop (rest_list, index - 1)
                      in
                          (S (child_set :: siblings), final_list)
                      end

        fun start (in_list : int list) : set =
            #1 (outer_rec_loop in_list)
    in
        start lst
    end


val _ = 
    let
        (* Test examples *)
        val example1 = dfs (S [])
        val example2 = dfs (S [S []])
        val example3 = dfs (S [S [], S []])
        val example4 = dfs (S [S [S []], S []])
        val example5 = dfs (S [S [S [S []], S[S []]], S [S []]])

        val rev_example1 = fromDFS example1
        val rev_example2 = fromDFS example2
        val rev_example3 = fromDFS example3
        val rev_example4 = fromDFS example4
        val rev_example5 = fromDFS example5

        val _ = assert (example1 = dfs rev_example1)
        val _ = assert (example2 = dfs rev_example2)
        val _ = assert (example3 = dfs rev_example3)
        val _ = assert (example4 = dfs rev_example4)
        val _ = assert (example5 = dfs rev_example5)
    in
        ()
    end

(* Za primerjavo množic `set` definiramo urejenost `order` na naslednji način.
    - Če |s1| > |s2|, potem `compare (s1, s2)` vrne `GREATER`.
    - Če |s1| < |s2|, potem `compare (s1, s2)` vrne `LESS`.
    - Če |s1| = |s2|, potem rekurzivno padajoče uredimo seznama množic s1 in s2 glede na `order`,
        in če je seznam urejene množice `s1` leksikografsko manjši seznama množice s2,
        potem `compare (s1, s2)` vrne `LESS` (podobno za `GREATER` in `EQUAL`). 

        - compare (S [S [], S [S []]], S [S [S []], S []]);
        val it = EQUAL : order
        - compare (S [S [S []]], S [S [], S [S []]]);
        val it = LESS : order
        - compare (S [S [], S [S []]], S [S []]);
        val it = GREATER : order
*)


fun sort (cmp : set * set -> order) (S lst) : set =
    let
        fun insert (x, []) = [x]
          | insert (x, y::ys) =
                case cmp (x, y) of
                    LESS => x :: y :: ys
                  | _    => y :: insert (x, ys)

        fun insertion_sort [] = []
          | insertion_sort (x::xs) = insert (x, insertion_sort xs)
    in
        S (insertion_sort lst)
    end

(* Hello world*)
val _ = print "Comparing sets tests:\n"

fun compare (s1 : set, s2 : set) : order =
    let
        (* Sort sets recursively *)
        fun sort_sets (S lst) =
            sort compare (S (List.map sort_sets lst))

        val S l1 = s1
        val S l2 = s2

        (* Helper to compare lists of sets lexicographically *)
        fun compare_lists ([], []) = EQUAL
          | compare_lists ([], _) = LESS
          | compare_lists (_, []) = GREATER
          | compare_lists (x::xs, y::ys) =
                case compare (x, y) of
                    EQUAL => compare_lists (xs, ys)
                  | ord => ord
    in
        case Int.compare (List.length l1, List.length l2) of
            LESS => LESS
          | GREATER => GREATER
          | EQUAL =>
                let
                    val S l1' = sort_sets s1
                    val S l2' = sort_sets s2
                in
                    compare_lists (l1', l2')
                end
    end

(*TEST*)
val _ = 
    let
        val _ = assert (compare (S [S [], S [S []]], S [S [S []], S []]) = EQUAL)
        val _ = assert (compare (S [S [S []]], S [S [], S [S []]]) = LESS)
        val _ = assert (compare (S [S [], S [S []]], S [S []]) = GREATER)
        val _ = assert (compare (S [S []], S [S [], S []]) = LESS)
        val _ = assert (compare (S [S [], S []], S [S []]) = GREATER)
    in
        ()
    end