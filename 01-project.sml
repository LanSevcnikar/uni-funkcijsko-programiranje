val _ = Control.Print.printDepth := 10;
val _ = Control.Print.printLength := 10;
val _ = Control.Print.stringDepth := 2000;
val _ = Control.polyEqWarn := false;


fun readFile filename =
  let val is = TextIO.openIn filename
  in 
    String.map (fn c => if Char.isGraph c orelse c = #" " orelse c = #"\n" then c else #" ")
      (TextIO.inputAll is)
    before TextIO.closeIn is
  end

exception NotImplemented;

(* Basically just takes n sized chunks and joins them together, super basic *)
fun split n xs =
  let
    fun first_n (0, _) = []
      | first_n (_, []) = []
      | first_n (n, x::xs) = x :: first_n (n-1, xs)
    fun all_but_first_n (0, xs) = xs
      | all_but_first_n (_, []) = []
      | all_but_first_n (n, _::xs) = all_but_first_n (n-1, xs)

    fun helper xs =
      if length xs < n then []
      else
        let
          val block = first_n (n, xs)
          val rest = all_but_first_n (n, xs)
        in
          block :: helper rest
        end
  in
    if n <= 0 then []
    else helper xs
  end;

(* Taken directly from the pseudocode
function extended_gcd(a, b)
    (old_r, r) := (a, b)
    (old_s, s) := (1, 0)
    (old_t, t) := (0, 1)
    
    while r ≠ 0 do
        quotient := old_r div r
        (old_r, r) := (r, old_r − quotient × r)
        (old_s, s) := (s, old_s − quotient × s)
        (old_t, t) := (t, old_t − quotient × t)
    
    output "Bézout coefficients:", (old_s, old_t)
    output "greatest common divisor:", old_r
Could not be more exact
*)
fun xGCD (a, b) =
  let
    fun helper(old_r, r, old_s, s, old_t, t) =
      if r = 0 then (old_r, old_s, old_t)
      else
        let
          val quotient = old_r div r
          val new_r = old_r - quotient * r
          val new_s = old_s - quotient * s
          val new_t = old_t - quotient * t
        in
          helper(r, new_r, s, new_s, t, new_t)
        end
  in
    helper(a, b, 1, 0, 0, 1)
  end;


signature RING =
sig
  eqtype t
  val zero : t
  val one : t
  val neg : t -> t
  val xGCD : t * t -> t * t * t
  val inv : t -> t option
  val + : t * t -> t
  val * : t * t -> t
  val toString : t -> string
end;

functor Ring (val n : int) :> RING where type t = int =
struct
  type t = int
  val zero = 0
  val one = 1
  fun neg x = ~x mod n
  val xGCD = xGCD
  
  fun inv x =
    case xGCD (x mod n, n) of
      (1, s, _) => SOME (s mod n)
    | _ => NONE

  fun op + a =  Int.+ a mod n
  fun op * p =  Int.* p mod n
  val toString = Int.toString
end;

signature MAT =
sig
  eqtype t
  structure Vec :
    sig
      val dot : t list -> t list -> t
      val add : t list -> t list -> t list
      val sub : t list -> t list -> t list
      val scale : t -> t list -> t list
    end
  val tr : t list list -> t list list
  val mul : t list list -> t list list -> t list list
  val id : int -> t list list
  val join : t list list -> t list list -> t list list
  val inv : t list list -> t list list option
  val showMatrix : t list list option -> string
  val showVec : t list option -> string
end;

functor Mat (R : RING) :> MAT where type t = R.t =
struct
  type t = R.t
  structure Vec =
    struct
      fun dot v u =
        (* Zips together a list and b list into a * b list, then passes that together with 0 *)
        (* to the sum and products with fold function. Very condensed but works *)
        List.foldl (fn ((x, y), acc) => R.+ (R.* (x, y), acc)) R.zero (ListPair.zip (v, u))
      
      fun add v u =
        (* Creates a list a * b and then maps everything using the add function *)
        List.map (R.+) (ListPair.zip (v, u))

      fun sub v u =
        (* Creates a list a * b and then maps everything using the sub function *)
        List.map (R.+) (ListPair.zip (v,  List.map R.neg u))

      fun scale k v =
        (*  -||-  *)
        List.map (fn x => R.* (k, x)) v
    end

  fun tr m =
    (* Creates an array of empty arrays as long as the first one is wide, let us call it mtr *)
    (* That and m are then passed into the function, map, call it F that takes each element of some column and makes it into a row *)
    List.foldl (fn (row, acc) =>
      ListPair.map (fn (x, col) => col @ [x]) (row, acc))
      (List.tabulate (length (hd m), fn _ => [])) m
    (* Inputs and layers *)
    (* 1. calls F  [[],[],[]] [1,2,3] = [[1],[2],[3]] *)
    (* 2. calls F  [[1],[2],[3]] [4,5,6] = [[1,4],[2,5],[3,6]] *)
    
  (* This is exactly how you do it by hand, each cell is just the dot product of the 
    two corresponding row and column and map is used as literally an iterator

    I wonder if it is possible to make it nice like the rest with just some List operations
   *)
  fun mul m1 m2 =
    let
      val m2t = tr m2
    in
      List.map (fn row1 =>
        List.map (fn row2 => Vec.dot row1 row2) m2t) m1
    end

  (* How ugly that we have to test for empty matrices *)
  (* Otherwise, obvious what I did here, makes each pairing to the concat of the pairing *)
  fun join m1 m2 =
    if(length m1 = 0) then m2
    else if(length m2 = 0) then m1
    else ListPair.map (fn (row1, row2) => row1 @ row2) (m1, m2)

  (* This one is pretty. Two tabulates and they only make a one when both are the same *)
  fun id n =
    List.tabulate (n, fn x =>
      List.tabulate (n, fn y => if x = y then R.one else R.zero))

  fun showMatrix NONE = "NONE"
    | showMatrix (SOME m) =
        let
          fun showRow row =
            "[" ^ String.concatWith ", " (List.map R.toString row) ^ "]"
        in
          "[" ^ String.concatWith "\n" (List.map showRow m) ^ "]"
        end

  fun showVec NONE = "NONE"
  | showVec (SOME v) =
      String.concatWith " " (List.map R.toString v)


  (*This took like 4 hours, if I ever learn that this was done at practice, I am finna cry*)
  fun inv m = 
    let
      (*Literally coppied, I do not know what it does*)
      fun pivot ([] :: _ | []) : t list option = NONE 
        | pivot ((v as x :: _) :: m) =
          if x = R.one then SOME v else
          case (R.inv x, m) of
              (SOME x', _) => SOME (Vec.scale x' v)
            | (NONE, (u as y :: _) :: m) => 
                let val (_, s, t) = R.xGCD (x, y) in
                  pivot (Vec.add (Vec.scale s v) (Vec.scale t u) :: m)
                end
            | _ => NONE

      (*Part of the first half where I produce all needed pivot rows, obvious what it does*)
      fun reduce_row row pivot_row =
        let
          val leading = 1
          val factor = hd row
        in
          Vec.sub row (Vec.scale factor pivot_row)
        end

      fun reduce_matrix m pivot_row =
        case pivot_row of
          NONE => NONE
          | SOME pivot_row => SOME (List.map (fn row => reduce_row row pivot_row) m)

      fun remove_leading_column m =
        case m of
          SOME mt => SOME (List.map (fn (_::row) => row) mt)
        | NONE => NONE

      (*This is seperated because i wanted to be able to print out values for debugging*)
      fun do_entire_step m p =
        case p of
          NONE => NONE
        | SOME pivot_row =>
            let
              (* val _ = print ("Pivot row: " ^ showVec (SOME pivot_row) ^ "\n\n") *)
              val reduced = remove_leading_column (reduce_matrix m p)
              (* val _ = print ("Reduced matrix: \n" ^ showMatrix reduced ^ "\n\n") *)
            in
              reduced
            end

      fun get_last_n_elements_of_vector(xs, n) =
        let
            val len = List.length xs
            val k = len - n
        in
            if n <= 0 then []
            else if n >= len then xs
            else List.drop (xs, k)
        end

      fun get_all_pivots(A, i) =
        case (A, i) of
          (NONE, _) => NONE
          | (_, 0) => SOME []
          | (SOME m, _) =>
            let 
              val p = pivot m
              val B = do_entire_step m p
              (* val _ = print ("After step " ^ showMatrix B ^ "\n\n") *)
              val r = get_all_pivots(B, i-1)
            in
              case r of 
                NONE => NONE
                | SOME l => SOME(p :: l)
            end

        fun get_pivots_as_list (A, size_of_m) =
          case get_all_pivots (A, size_of_m) of
            NONE => NONE
          | SOME pivots =>
              let
                val inv = List.foldr (fn (p, acc) =>
                  case (p, acc) of
                    (NONE, _) => NONE
                  | (SOME v, NONE) => SOME [v]
                  | (SOME v, SOME l) => SOME (v :: l)
                ) NONE pivots
              in
                inv
              end

        fun make_all_pivots_length (ps, size_of_m) =
          case ps of
            NONE => NONE
          | SOME pivots => SOME (
              List.map (fn p =>
                (List.tabulate (2 * size_of_m - length p, fn _ => R.zero) @ p)
              ) pivots
            )

        val size_of_m = length m
        val A = join m (id size_of_m)
        val pivots_as_list = make_all_pivots_length (get_pivots_as_list (SOME A, size_of_m), size_of_m)

        (*This is what I consider the first half of the code. After this part, we now know if an inverse exists
        If it does not we are done, if we are not we just need to do some row operations to find the inverse. 
        Reason for this semantic split is so that here we can finally stop using SOME and NONE*)


        fun get_index_of_first_nonzero_in_row row =
          let 
            fun helper ([], idx) = 0
              | helper (x::xs, idx) =
                  if x <> R.zero then idx
                  else helper (xs, idx + 1)
          in
            helper (row, 0)
          end

        fun subtract_bottom_from_top (ps, i, j) =
          if i >= j then ps
          else
            let
              (* val _ = print ("Subtracting row " ^ Int.toString j ^ " from row " ^ Int.toString i ^ "\n") *)
              val top = List.nth (ps, i)
              val bottom = List.nth (ps, j)
              val index_of_first_nonzero_in_bottom = get_index_of_first_nonzero_in_row bottom
              val factor = List.nth (top, index_of_first_nonzero_in_bottom)
              val new_top = Vec.sub top (Vec.scale factor bottom)
              val new_pivots = List.tabulate (length ps, fn k =>
                if k = i then new_top
                else List.nth (ps, k)
              )
            in
              new_pivots
            end


          (*I am sure there exist better ways to do this but as I said I think this problem has stumpped me long enough and I cannot look at it anymore*)
          fun reduce_pivots (ps, size_of_m) =
            let
              (* val _ = print ("Reducing pivots...\n with size " ^ Int.toString size_of_m ^ "\n") *)
              fun loop (pivots, i, j) =
                case (i, j) of
                  (~1,_) => pivots
                | (_, ~1) => loop (pivots, i-1, size_of_m -1)
                | (_, _) => loop (subtract_bottom_from_top (pivots, i, j), i, j-1)
            in
              loop (ps, size_of_m -1, size_of_m -1)
            end


    
    in
      case pivots_as_list of
        NONE => NONE
      | SOME pivots_list =>
        let
          val reduced_pivots = reduce_pivots (pivots_list, size_of_m)
          val cut_pivots = List.map (fn row => get_last_n_elements_of_vector (row, size_of_m)) reduced_pivots
        in
          SOME cut_pivots
        end
    end

end;

signature CIPHER =
sig
  type t
  val encrypt : t list list -> t list -> t list
  val decrypt : t list list -> t list -> t list option
  val knownPlaintextAttack : int -> t list -> t list -> t list list option
end;

functor HillCipherAnalyzer (M : MAT) :> CIPHER
  where type t = M.t
=
struct
  type t = M.t
  
  fun encrypt key plaintext = 
    let
      fun encrypt_chunk chunk =
        let
          val chunk_matrix = List.map (fn x => [x]) chunk
          val encrypted_chunk_matrix = M.mul (M.tr key) chunk_matrix
          val flattened_chunk = List.map (fn xs => hd xs) encrypted_chunk_matrix
        in
          flattened_chunk
        end

        
      val n = length key
      val chunks = split n plaintext
      val encryptedChunks = List.map encrypt_chunk chunks
      val concatenated = List.concat encryptedChunks
    in
      concatenated
    end

  fun decrypt key ciphertext = 
    let
      fun decrypt_chunk chunk =
        let
          val chunk_matrix = List.map (fn x => [x]) chunk
          val key_inv_option = M.inv (M.tr key)
        in
          case key_inv_option of
            NONE => NONE
          | SOME key_inv =>
              let
                val decrypted_chunk_matrix = M.mul key_inv chunk_matrix
                val flattened_chunk = List.map (fn xs => hd xs) decrypted_chunk_matrix
              in
                SOME flattened_chunk
              end
        end

        
      val n = length key
      val chunks = split n ciphertext
      val decryptedChunksOption = List.map decrypt_chunk chunks

      fun combine_chunks NONE = NONE
        | combine_chunks (SOME chunks) =
            SOME (List.concat chunks)

    in
      case length chunks of
        0 => NONE
        | _ => combine_chunks (List.foldr (fn (chunk_option, acc_option) =>
          case (chunk_option, acc_option) of
            (NONE, _) => NONE
          | (_, NONE) => NONE
          | (SOME chunk, SOME acc) => SOME (chunk :: acc)
        ) (SOME []) decryptedChunksOption)
    end
    
  fun knownPlaintextAttack keyLenght plaintext ciphertext = raise NotImplemented
end;


structure Trie :> 
sig
eqtype ''a dict
val empty : ''a dict
val insert : ''a list -> ''a dict -> ''a dict
val lookup : ''a list -> ''a dict -> bool
end
=
struct
  datatype ''a tree = N of ''a * bool * ''a tree list
  type ''a dict = ''a tree list

  val empty = [] : ''a dict

  fun insert w dict = raise NotImplemented
  fun lookup w dict = raise NotImplemented
end;

signature HILLCIPHER =
sig
  structure Ring : RING where type t = int
  structure Matrix : MAT where type t = Ring.t
  structure Cipher : CIPHER where type t = Matrix.t
  val alphabetSize : int
  val alphabet : char list
  val encode : string -> Cipher.t list
  val decode : Cipher.t list -> string
  val encrypt : Cipher.t list list -> string -> string
  val decrypt : Cipher.t list list -> string -> string option
  val knownPlaintextAttack :
      int -> string -> string -> Cipher.t list list option
  val ciphertextOnlyAttack : int -> string -> Cipher.t list list option
end

functor HillCipher (val alphabet : string) :> HILLCIPHER =
struct

(*printable characters*)
val alphabetSize = String.size alphabet
val alphabet = String.explode alphabet

structure Ring = Ring (val n = alphabetSize)
structure Matrix = Mat (Ring)
structure Cipher = HillCipherAnalyzer (Matrix)

fun encode txt = raise NotImplemented
fun decode code = raise NotImplemented

local
  fun parseWords filename =
    let val is = TextIO.openIn filename
      fun read_lines is =
        case TextIO.inputLine is of
          SOME line =>
            if String.size line > 1
            then String.tokens (not o Char.isAlpha) line @ read_lines is
            else read_lines is
          | NONE => []
    in List.map (String.map Char.toLower) (read_lines is) before TextIO.closeIn is end

  val dictionary = List.foldl (fn (w, d) => Trie.insert w d) Trie.empty (List.map String.explode (parseWords "hamlet.txt")) handle NotImplemented => Trie.empty
in
  fun encrypt key plaintext = raise NotImplemented
  fun decrypt key ciphertext = raise NotImplemented
  fun knownPlaintextAttack keyLenght plaintext ciphertext = raise NotImplemented
  fun ciphertextOnlyAttack keyLenght ciphertext = raise NotImplemented
  end
end;

