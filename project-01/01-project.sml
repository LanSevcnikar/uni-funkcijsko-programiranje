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
        fun takeN (0, lst, acc) = (rev acc, lst)
          | takeN (_, [], acc) = (rev acc, [])
          | takeN (k, x::xs, acc) = takeN (k-1, xs, x::acc)

        fun helper [] = []
          | helper lst =
              let
                  val (block, rest) = takeN (n, lst, [])
              in
                  block :: helper rest
              end
    in
      (* This should be faster because length is most likely O(n) *)
        if n <= 0 then [] else List.filter (fn x => length x = n) (helper xs)
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
  (* val showMatrix : t list list option -> string *)
  (* val showVec : t list option -> string *)
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

  (* fun showMatrix NONE = "NONE"
    | showMatrix (SOME m) =
        let
          fun showRow row =
            "[" ^ String.concatWith ", " (List.map R.toString row) ^ "]"
        in
          "[" ^ String.concatWith "\n" (List.map showRow m) ^ "]"
        end

  fun showVec NONE = "NONE"
  | showVec (SOME v) =
      String.concatWith " " (List.map R.toString v) *)


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
  (* For some reason all of this only works if I transpose the key,tf, I am not sure why*)
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
      fun decrypt_chunk (chunk, key_inv_option) =
        let
          val chunk_matrix = List.map (fn x => [x]) chunk
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
      val key_inv_option = M.inv (M.tr key) (* Best not to compute it each time again *)
      val chunks = split n ciphertext
      val decryptedChunksOption = List.map (fn chunk => decrypt_chunk (chunk, key_inv_option)) chunks

      fun combine_chunks NONE = NONE
        | combine_chunks (SOME chunks) =
            SOME (List.concat chunks)

    in
      case length chunks of
      (* Why was this needed, why do the tests give NONE if the input word is of length 0*)
        0 => NONE
        | _ => combine_chunks (List.foldr (fn (chunk_option, acc_option) =>
          case (chunk_option, acc_option) of
            (NONE, _) => NONE
          | (_, NONE) => NONE
          | (SOME chunk, SOME acc) => SOME (chunk :: acc)
        ) (SOME []) decryptedChunksOption)
    end
    
  fun knownPlaintextAttack keyLenght plaintext ciphertext = 
    let
      fun build_matrix_of_height_n n chunks =
        List.map (fn chunk => chunk) (List.take (chunks, n))
      

      (* It makes me anrgy that I cannot make these two functions into one function*)
      (* This function just tries the square, again, I wish I could do it inside of the next function *)
      fun try_n_by_n_invert n plaintext_chunks ciphertext_chunks =
        let
          val X = build_matrix_of_height_n n plaintext_chunks
          val X_inv_opt = M.inv X
          val Y = build_matrix_of_height_n n ciphertext_chunks
        in
          case X_inv_opt of
            NONE => NONE
          | SOME X_inv => SOME (M.mul X_inv Y)
        end

      (* I feel like this is not very quick because of the fact that I do not append but rather I redo everything *)
      (* All of this is exactly as done in the explenation given *)

      fun build_try_first_k_chunks k plaintext_chunks ciphertext_chunks =
        let
          fun try_using_X_and_Y X Y =
            let
              val X_tr = M.tr X
              val X_tr_X_inv = M.inv (M.mul X_tr X)
            in
              case X_tr_X_inv of
                NONE => NONE
              | SOME X_tr_X_inv_mat => SOME (M.mul X_tr_X_inv_mat (M.mul X_tr Y))
            end

          fun helper X Y remaining_plaintext_chunks remaining_ciphertext_chunks =
            let
              val attempt = try_using_X_and_Y X Y
            in
              case (remaining_plaintext_chunks, remaining_ciphertext_chunks) of
                ([], _) => [attempt]
              | (_, []) => [attempt]
              | (pt_chunk::pt_rest, ct_chunk::ct_rest) =>
                  let
                    val new_X = X @ [pt_chunk]
                    val new_Y = Y @ [ct_chunk]
                  in
                    [attempt] @ (helper new_X new_Y pt_rest ct_rest)
                  end
            end

          val X = build_matrix_of_height_n k plaintext_chunks
          val Y = build_matrix_of_height_n k ciphertext_chunks
          val all_but_first_k_plaintext_chunks = List.drop (plaintext_chunks, k)
          val all_but_first_k_ciphertext_chunks = List.drop (ciphertext_chunks, k)
        in
          helper X Y all_but_first_k_plaintext_chunks all_but_first_k_ciphertext_chunks
        end

      (* Checks if the key worked *)
      fun validate_key candidate_key plaintext_chunks ciphertext_chunks =
        if List.all (fn (pt_chunk, ct_chunk) => encrypt candidate_key pt_chunk = ct_chunk) (ListPair.zip (plaintext_chunks, ciphertext_chunks)) 
        then SOME candidate_key
        else NONE

      val plaintext_chunks = split keyLenght plaintext
      val ciphertext_chunks = split keyLenght ciphertext

      (* The enough data here is to check regarding the key length, the addition of is_known_text_longer_than_ciphertext is added later, ergo looks weird *)
      val is_known_text_longer_than_ciphertext = (length (plaintext)) > (length (List.concat ciphertext_chunks))
      val enough_data = (length plaintext_chunks) >= keyLenght
      (* Let us take a moment to look at this. Looking at the description of the problem, there is a small implication
       that the known plaintext is longer than the ciphertext. Note that this does not make sense to me. Now, even more, it should be longer than the
       cypher text after getting cut. I do understand to some extent but when I looked at test4 of knownPlaintextAttack
       I got the wrong response without this here. Looking at it closer, I feel like it is likely that this was the problem because I was able to decipher the rest *)
    
      val return_value =
        case enough_data andalso not is_known_text_longer_than_ciphertext of
          false => NONE
        | true => 
          let
            (* A general comment about the code
              - The reason it is so messy is because it was too slow on one decypher attempt and this should speed it up, no longer building every k individual;ly but rather building them all at once using concat
              - Note that this makes the code ugly
              - it does the n by n attempt 
              - then it creates all of the attempts for larger than that using rec
              - then it validates all of them
              - then it filters
              - it returns the first valid one            
             *)
            val n_by_n_attempt =
              if enough_data andalso not is_known_text_longer_than_ciphertext
              then try_n_by_n_invert keyLenght plaintext_chunks ciphertext_chunks
              else NONE

            (* Is it just me or does this look too much like OOP? Like, this does not feel right *)
            val try_first_k_chunks = 
              case length plaintext_chunks > keyLenght of
                false => [] 
              | true => build_try_first_k_chunks (keyLenght + 1) plaintext_chunks ciphertext_chunks

            val results_before_validation = [n_by_n_attempt] @ try_first_k_chunks
            val valid_results = List.map (fn candidate_key_option =>
              case candidate_key_option of
                NONE => NONE
              | SOME candidate_key => validate_key candidate_key plaintext_chunks ciphertext_chunks
            ) results_before_validation
            val first_result = List.find (fn x => x <> NONE) valid_results
            val return_value_temp =
              case first_result of
                NONE => NONE
              | SOME key_option => key_option
        in
          return_value_temp
        end
    in
      return_value
    end
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

  fun insert w dict = 
    let
      fun insert_helper (wrd, dct) =
        case (wrd, dct) of
          ([], _) => dct (* Empty word, do nothing *)
        | (x::xs, []) => [N (x, xs = [], insert_helper (xs, []))] (* No children, just add the word, we are in new teritory *)
        | (x::xs, N (y, isWord, children) :: rest) => (* If we can move forward we move forward, if not we check next in node *)
            if x = y then
              let
                val new_child = insert_helper (xs, children)
                val new_node = N (y, isWord orelse xs = [], new_child)
              in
                new_node :: rest
              end
            else
              N (y, isWord, children) :: insert_helper (wrd, rest)
      in
        insert_helper (w, dict)
    end

    (* This function does not need to be commented on, obvious what it does *)
   fun lookup w dict =
     let
       fun lookup_helper ([], _) = false
         | lookup_helper (x::xs, []) = false
         | lookup_helper (x::xs, N(c, isW, children)::rest) =
             if x = c then
               if xs = [] then isW
               else lookup_helper (xs, children)
             else
               lookup_helper (x::xs, rest)
     in
       lookup_helper (w, dict)
     end
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

fun encode txt = 
  let
    fun index_of (c, d) =
      case (c, d) of
        (_, []) => raise Fail "Element not found"
        | (chr, l::lst) => if chr = l then 0 else 1 + index_of (chr, lst)

    fun char_to_int c = index_of (c, alphabet)
  in
    List.map char_to_int (String.explode txt)
  end

fun decode code = String.implode (List.map (fn i => List.nth (alphabet, i)) (code))

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
  fun encrypt key plaintext = 
    let
      val plaintext_encoded = encode plaintext
      val ciphertext_encoded = Cipher.encrypt key plaintext_encoded
      val ciphertext = decode ciphertext_encoded
    in
      ciphertext
    end
  fun decrypt key ciphertext = 
    let
      val ciphertext_encoded = encode ciphertext
      val plaintext_encoded_option = Cipher.decrypt key ciphertext_encoded
    in
      case plaintext_encoded_option of
        NONE => NONE
      | SOME plaintext_encoded =>
          SOME (decode plaintext_encoded)
    end
  fun knownPlaintextAttack keyLength plaintext ciphertext = 
    let
      val plaintext_encoded = encode plaintext
      val ciphertext_encoded = encode ciphertext
      val key_option = Cipher.knownPlaintextAttack keyLength plaintext_encoded ciphertext_encoded
    in
      case key_option of
        NONE => NONE
      | SOME key =>
          SOME key
    end

  fun ciphertextOnlyAttack keyLength ciphertext = 
    let
      val ciphertext_encoded = encode ciphertext

      (* Generate all candidate n x n matrices over the ring *)
      fun generate_candidates n =
      (* Each matrix is represented as a number. This is then transformed into a matrix representation *)
      (* I am not sure if there exists a better way, this israther slow and generating 2x2 matricies takes about 2 seconds on my machine *)
        let
          fun exp x p = 
            case p of
              0 => 1
            | _ => x * exp x (p - 1)

          fun number_to_flattened_matrix num =
            let
              fun helper(idx, n, lst) =
                case idx of
                  0 => lst
                | _ =>
                    let
                      val remainder = n mod alphabetSize
                      val quotient = n div alphabetSize
                    in
                      helper (idx - 1, quotient, remainder :: lst)
                    end
            in
              helper (n * n, num, [])
            end

          fun flattened_matrix_to_matrix flat =
            let
              fun build_rows ([], _) = []
                | build_rows (lst, 0) = []
                | build_rows (lst, r) =
                    let
                      val row = List.take (lst, n)
                      val rest = List.drop (lst, n)
                    in
                      row :: build_rows (rest, r - 1)
                    end
            in
              build_rows (flat, n)
            end
          
          val number_of_all_matrices = exp alphabetSize (exp keyLength 2)
          val all_matrix_encodings = List.tabulate (number_of_all_matrices, fn i => i)
          val all_flattened_matrices = List.map number_to_flattened_matrix all_matrix_encodings
          val all_matrices = List.map flattened_matrix_to_matrix all_flattened_matrices
        in
          all_matrices
        end

      val candidates = generate_candidates keyLength
      val invertible_candidates = List.filter (fn m => case Matrix.inv m of NONE => false | SOME _ => true) candidates
      val decrypted_candidates = List.map (fn candidate_key => (decrypt candidate_key ciphertext, candidate_key)) invertible_candidates

      (* Note that this is not super general *)
      (* https://smlfamily.github.io/Basis/string.html#SIG:STRING.tokens:VAL I hope this is allowed *)
      fun count_number_of_valid_words plaintext =
        let
          val words = String.tokens (not o Char.isAlpha) plaintext
          val valid_words = List.filter (fn w => Trie.lookup (String.explode (String.map Char.toLower w)) dictionary) words
          (* print out plaintext and number of valid words *)
          (* val _ = print (plaintext^ " | " ^ Int.toString (length valid_words) ^ "\n") *)
        in
          length valid_words
        end


      
      val scored_candidates = List.map (fn (decrypted_option, candidate_key) =>
        case decrypted_option of
          NONE => (0, candidate_key)
        | SOME decrypted_plaintext => (count_number_of_valid_words decrypted_plaintext, candidate_key)
      ) decrypted_candidates



      val best_candidate = List.foldl (fn ((score, key), (best_score, best_key)) =>
        if score > best_score then (score, key) else (best_score, best_key)
      ) (0, hd invertible_candidates) scored_candidates 

      (* As long as the best is 0, we do not care about the key, so we can use an arbitrary key *)

      val (best_score, best_key) = best_candidate

    in
      if best_score = 0 then NONE
      else SOME best_key
    end

end
end;

(* 
TEST 1

val alphabet = "\n !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
val possible_key_sizes = [1,2,3,4,5,6,7,8,9,10]
val x99 = String.substring ("#include<stdlib.h>\n#include<stdio.h>\n#include<unistd.h>\n#include<sys/types.h>\n#include<sys/wait.h>\n", 0, 96)
val y99 = String.substring ("Y,}*a@F^d'=1$e:g!n&x:L}U9(y8on+x+a:N2ym0 mQn+o#a)aaO4l+<Y,}*a@F^P#q]/lJj.\"oE./].Ff!HS=af~|sakH#jRWV", 0, 96)

(*  try to find a solution for each size *)
structure H2 = HillCipher(val alphabet = alphabet);
val results = List.map(fn key_size => H2.knownPlaintextAttack key_size x99 y99) possible_key_sizes
val _ = List.map (fn res => case res of
    NONE => print "No key found\n"
  | SOME key_matrix => print (H2.Matrix.showMatrix (SOME key_matrix) ^ "\n")) results;

No key found
No key found
No key found
No key found
No key found
No key found
No key found
[[19, 14, 14, 76, 16, 7, 5, 18]
[23, 15, 69, 65, 89, 38, 43, 91]
[11, 0, 24, 39, 75, 14, 46, 19]
[88, 57, 61, 56, 25, 40, 41, 8]
[84, 2, 51, 93, 87, 32, 39, 52]
[92, 52, 45, 45, 92, 76, 66, 56]
[7, 37, 41, 79, 56, 75, 16, 14]
[30, 8, 9, 65, 46, 36, 36, 35]]
No key found
No key found
*)

(*
TEST 2

val alphabet = " abcdefghijklmnopqrstuvwxyz"
val keyLength = 2 (* or 1 *)
val ciphertext = "ulglf etn ytc n pkoozawrgspqnbn i ajn obgsimpktr pn qvbrhaqfhnebxijtmiy opnrbaysdiwibrqfaiebimgsrivniimtocjfbrhnjvghdijshajthrj trujpqii pirmiy opbrhaiidyebghqqgrbrqftrjyxocfrajrhxkatrucimeigsquyjjmy opermeebhckasiavimhnoxgwavebolibgsyaimsissclzbkvbdras fxxlssmigkeizewtdukqcawr pn aimmbsjktxl wiebimhnjtlxtrtgf zutrucimhnatljavjtlxtricc cisstrjyxoimpkaib nrqutr pwrlcjfsif n mto w ibujavjkkzhudfhryshtkzpvtr pgiqtihsihweizeyaimeitrdycao csbraxbroowiyttrtgc hufmraaibstrujpqiiwxoofxczjdytwrobfmi yjgjfnnttzespikihnimgsdauikzlceryfgsimj gi xumhubrgspverpktr pwrlcjfsidibfbrhnimgsqerbgsjkkzimlj"


structure H2 = HillCipher(val alphabet = alphabet);
val result = H2.ciphertextOnlyAttack keyLength ciphertext
val decrypted_option = 
  case result of
    NONE => NONE
  | SOME key_matrix => H2.decrypt key_matrix ciphertext
val _ = 
  case decrypted_option of
    NONE => print "No valid decryption found\n"
  | SOME decrypted_text => print ("Decrypted text: \n" ^ decrypted_text ^ "\n")

(* For n = 1 *)
Decrypted text: 
ulplf wke gkc e ybooqjnrpayhete i jse otpaivybkr ye hdtrzjhfzewtxiskvig oyertjgaminitrhfjiwtivparideiivkocsftrzesdpzmisazjskzrs krusyhii yirvig oytrzjiimgwtpzhhprtrhfkrsgxocfrjsrzxbjkrucivwipahugssvg oywrvwwtzcbjaijdivzeoxpnjdwtolitpagjivaiaaclqtbdtmrja fxxlaavipbwiqwnkmubhcjnr ye jivvtasbkxl niwtivzesklxkrkpf qukrucivzejklsjdsklxkricc ciaakrsgxoivybjit erhukr ynrlcsfaif e vko n itusjdsbbqzumfzrgazkbqydkr ypihkizaiznwiqwgjivwikrmgcjo catrjxtroonigkkrkpc zufvrjjitakrusyhiinxoofxcqsmgknrotfvi gspsfeekkqwayibizeivpamjuibqlcwrgfpaivs pi xuvzutrpaydwrybkr ynrlcsfaimitftrzeivpahwrtpasbbqivls

(* For n = 2 *)
Decrypted text: 
from fairest creatures we desire increase that thereby beauty s rose might never die but as the riper should by time decease his tender heir might bear his memory but thou contracted to thine own bright eyes feed st thy light s flame with self substantial fuel making a famine where abundance lies thy self thy foe to thy sweet self too cruel thou that art now the world s fresh ornament and only herald to the gaudy spring within thine own bud buriest thy content and tender churl mak st waste in niggarding pity the world or else this glutton be to eat the world s due by the grave and thee

This text takes about half a minute to decipher
*)