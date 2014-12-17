(*  Title:      Pure/library.ML
    Author:     Lawrence C Paulson, Cambridge University Computer Laboratory
    Author:     Markus Wenzel, TU Muenchen

Basic library: functions, pairs, booleans, lists, integers,
strings, lists as sets, orders, current directory, misc.

See also General/basics.ML for the most fundamental concepts.
*)

infix 2 ?
infix 3 o oo ooo oooo
infix 4 ~~ upto downto
infix orf andf

signature BASIC_LIBRARY =
sig
  (*functions*)
  val undefined: 'a -> 'b
  val I: 'a -> 'a
  val K: 'a -> 'b -> 'a
  val curry: ('a * 'b -> 'c) -> 'a -> 'b -> 'c
  val uncurry: ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  val ? : bool * ('a -> 'a) -> 'a -> 'a
  val oo: ('a -> 'b) * ('c -> 'd -> 'a) -> 'c -> 'd -> 'b
  val ooo: ('a -> 'b) * ('c -> 'd -> 'e -> 'a) -> 'c -> 'd -> 'e -> 'b
  val oooo: ('a -> 'b) * ('c -> 'd -> 'e -> 'f -> 'a) -> 'c -> 'd -> 'e -> 'f -> 'b
  val funpow: int -> ('a -> 'a) -> 'a -> 'a
  val funpow_yield: int -> ('a -> 'b * 'a) -> 'a -> 'b list * 'a

  (*user errors*)
  exception ERROR of string
  val error: string -> 'a
  val cat_error: string -> string -> 'a
  val assert_all: ('a -> bool) -> 'a list -> ('a -> string) -> unit

  (*pairs*)
  val pair: 'a -> 'b -> 'a * 'b
  val rpair: 'a -> 'b -> 'b * 'a
  val fst: 'a * 'b -> 'a
  val snd: 'a * 'b -> 'b
  val eq_fst: ('a * 'c -> bool) -> ('a * 'b) * ('c * 'd) -> bool
  val eq_snd: ('b * 'd -> bool) -> ('a * 'b) * ('c * 'd) -> bool
  val eq_pair: ('a * 'c -> bool) -> ('b * 'd -> bool) -> ('a * 'b) * ('c * 'd) -> bool
  val swap: 'a * 'b -> 'b * 'a
  val apfst: ('a -> 'b) -> 'a * 'c -> 'b * 'c
  val apsnd: ('a -> 'b) -> 'c * 'a -> 'c * 'b
  val pairself: ('a -> 'b) -> 'a * 'a -> 'b * 'b

  (*booleans*)
  val equal: ''a -> ''a -> bool
  val not_equal: ''a -> ''a -> bool
  val orf: ('a -> bool) * ('a -> bool) -> 'a -> bool
  val andf: ('a -> bool) * ('a -> bool) -> 'a -> bool
  val exists: ('a -> bool) -> 'a list -> bool
  val forall: ('a -> bool) -> 'a list -> bool
  val setmp_CRITICAL: 'a Unsynchronized.ref -> 'a -> ('b -> 'c) -> 'b -> 'c
  val setmp_thread_data: 'a Universal.tag -> 'a -> 'a -> ('b -> 'c) -> 'b -> 'c

  (*lists*)
  val single: 'a -> 'a list
  val the_single: 'a list -> 'a
  val singleton: ('a list -> 'b list) -> 'a -> 'b
  val yield_singleton: ('a list -> 'c -> 'b list * 'c) -> 'a -> 'c -> 'b * 'c
  val perhaps_apply: ('a -> 'a option) list -> 'a -> 'a option
  val perhaps_loop: ('a -> 'a option) -> 'a -> 'a option
  val foldl1: ('a * 'a -> 'a) -> 'a list -> 'a
  val foldr1: ('a * 'a -> 'a) -> 'a list -> 'a
  val eq_list: ('a * 'a -> bool) -> 'a list * 'a list -> bool
  val maps: ('a -> 'b list) -> 'a list -> 'b list
  val filter: ('a -> bool) -> 'a list -> 'a list
  val filter_out: ('a -> bool) -> 'a list -> 'a list
  val map_filter: ('a -> 'b option) -> 'a list -> 'b list
  val take: int -> 'a list -> 'a list
  val drop: int -> 'a list -> 'a list
  val chop: int -> 'a list -> 'a list * 'a list
  val chop_groups: int -> 'a list -> 'a list list
  val nth: 'a list -> int -> 'a
  val nth_list: 'a list list -> int -> 'a list
  val nth_map: int -> ('a -> 'a) -> 'a list -> 'a list
  val nth_drop: int -> 'a list -> 'a list
  val map_index: (int * 'a -> 'b) -> 'a list -> 'b list
  val fold_index: (int * 'a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val map_range: (int -> 'a) -> int -> 'a list
  val fold_range: (int -> 'a -> 'a) -> int -> 'a -> 'a
  val split_last: 'a list -> 'a list * 'a
  val find_first: ('a -> bool) -> 'a list -> 'a option
  val find_index: ('a -> bool) -> 'a list -> int
  val get_first: ('a -> 'b option) -> 'a list -> 'b option
  val get_index: ('a -> 'b option) -> 'a list -> (int * 'b) option
  val flat: 'a list list -> 'a list
  val unflat: 'a list list -> 'b list -> 'b list list
  val grouped: int -> (('a list -> 'b list) -> 'c list list -> 'd list list) ->
    ('a -> 'b) -> 'c list -> 'd list
  val burrow: ('a list -> 'b list) -> 'a list list -> 'b list list
  val burrow_options: ('a list -> 'b list) -> 'a option list -> 'b option list
  val fold_burrow: ('a list -> 'c -> 'b list * 'd) -> 'a list list -> 'c -> 'b list list * 'd
  val separate: 'a -> 'a list -> 'a list
  val surround: 'a -> 'a list -> 'a list
  val replicate: int -> 'a -> 'a list
  val map_product: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val fold_product: ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
  val map2: ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
  val fold2: ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
  val fold_rev2: ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
  val forall2: ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
  val map_split: ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list
  val zip_options: 'a list -> 'b option list -> ('a * 'b) list
  val ~~ : 'a list * 'b list -> ('a * 'b) list
  val split_list: ('a * 'b) list -> 'a list * 'b list
  val burrow_fst: ('a list -> 'b list) -> ('a * 'c) list -> ('b * 'c) list
  val is_prefix: ('a * 'a -> bool) -> 'a list -> 'a list -> bool
  val take_prefix: ('a -> bool) -> 'a list -> 'a list * 'a list
  val chop_prefix: ('a * 'b -> bool) -> 'a list * 'b list -> 'a list * ('a list * 'b list)
  val take_suffix: ('a -> bool) -> 'a list -> 'a list * 'a list
  val prefixes1: 'a list -> 'a list list
  val prefixes: 'a list -> 'a list list
  val suffixes1: 'a list -> 'a list list
  val suffixes: 'a list -> 'a list list

  (*integers*)
  val upto: int * int -> int list
  val downto: int * int -> int list
  val radixpand: int * int -> int list
  val radixstring: int * string * int -> string
  val string_of_int: int -> string
  val signed_string_of_int: int -> string
  val string_of_indexname: string * int -> string
  val read_radix_int: int -> string list -> int * string list
  val read_int: string list -> int * string list
  val oct_char: string -> string

  (*strings*)
  val nth_string: string -> int -> string
  val fold_string: (string -> 'a -> 'a) -> string -> 'a -> 'a
  val exists_string: (string -> bool) -> string -> bool
  val forall_string: (string -> bool) -> string -> bool
  val first_field: string -> string -> (string * string) option
  val enclose: string -> string -> string -> string
  val unenclose: string -> string
  val quote: string -> string
  val cartouche: string -> string
  val space_implode: string -> string list -> string
  val commas: string list -> string
  val commas_quote: string list -> string
  val cat_lines: string list -> string
  val space_explode: string -> string -> string list
  val split_lines: string -> string list
  val plain_words: string -> string
  val prefix_lines: string -> string -> string
  val prefix: string -> string -> string
  val suffix: string -> string -> string
  val unprefix: string -> string -> string
  val unsuffix: string -> string -> string
  val trim_line: string -> string
  val replicate_string: int -> string -> string
  val translate_string: (string -> string) -> string -> string
  val match_string: string -> string -> bool

  (*reals*)
  val string_of_real: real -> string
  val signed_string_of_real: real -> string

  (*lists as sets -- see also Pure/General/ord_list.ML*)
  val member: ('b * 'a -> bool) -> 'a list -> 'b -> bool
  val insert: ('a * 'a -> bool) -> 'a -> 'a list -> 'a list
  val remove: ('b * 'a -> bool) -> 'b -> 'a list -> 'a list
  val update: ('a * 'a -> bool) -> 'a -> 'a list -> 'a list
  val union: ('a * 'a -> bool) -> 'a list -> 'a list -> 'a list
  val subtract: ('b * 'a -> bool) -> 'b list -> 'a list -> 'a list
  val inter: ('a * 'b -> bool) -> 'b list -> 'a list -> 'a list
  val merge: ('a * 'a -> bool) -> 'a list * 'a list -> 'a list
  val subset: ('a * 'b -> bool) -> 'a list * 'b list -> bool
  val eq_set: ('a * 'a -> bool) -> 'a list * 'a list -> bool
  val distinct: ('a * 'a -> bool) -> 'a list -> 'a list
  val duplicates: ('a * 'a -> bool) -> 'a list -> 'a list
  val has_duplicates: ('a * 'a -> bool) -> 'a list -> bool
  val map_transpose: ('a list -> 'b) -> 'a list list -> 'b list

  (*lists as multisets*)
  val remove1: ('b * 'a -> bool) -> 'b -> 'a list -> 'a list
  val combine: ('a * 'a -> bool) -> 'a list -> 'a list -> 'a list
  val submultiset: ('a * 'b -> bool) -> 'a list * 'b list -> bool

  (*orders*)
  val is_equal: order -> bool
  val rev_order: order -> order
  val make_ord: ('a * 'a -> bool) -> 'a * 'a -> order
  val bool_ord: bool * bool -> order
  val int_ord: int * int -> order
  val string_ord: string * string -> order
  val fast_string_ord: string * string -> order
  val option_ord: ('a * 'b -> order) -> 'a option * 'b option -> order
  val prod_ord: ('a * 'b -> order) -> ('c * 'd -> order) -> ('a * 'c) * ('b * 'd) -> order
  val dict_ord: ('a * 'b -> order) -> 'a list * 'b list -> order
  val list_ord: ('a * 'b -> order) -> 'a list * 'b list -> order
  val sort: ('a * 'a -> order) -> 'a list -> 'a list
  val sort_distinct: ('a * 'a -> order) -> 'a list -> 'a list
  val sort_strings: string list -> string list
  val sort_wrt: ('a -> string) -> 'a list -> 'a list
  val tag_list: int -> 'a list -> (int * 'a) list
  val untag_list: (int * 'a) list -> 'a list
  val order_list: (int * 'a) list -> 'a list

  (*random numbers*)
  exception RANDOM
  val random: unit -> real
  val random_range: int -> int -> int

  (*misc*)
  val divide_and_conquer: ('a -> 'a list * ('b list -> 'b)) -> 'a -> 'b
  val divide_and_conquer': ('a -> 'b -> ('a list * ('c list * 'b -> 'c * 'b)) * 'b) ->
    'a -> 'b -> 'c * 'b
  val partition_eq: ('a * 'a -> bool) -> 'a list -> 'a list list
  val partition_list: (int -> 'a -> bool) -> int -> int -> 'a list -> 'a list list
  type serial = int
  val serial: unit -> serial
  val serial_string: unit -> string
  eqtype stamp
  val stamp: unit -> stamp
  structure Any: sig type T = exn end
  val cd: string -> unit
  val pwd: unit -> string
  val getenv: string -> string
  val getenv_strict: string -> string
end;

signature LIBRARY =
sig
  include BASIC_LIBRARY
  val foldl: ('a * 'b -> 'a) -> 'a * 'b list -> 'a
  val foldr: ('a * 'b -> 'b) -> 'a list * 'b -> 'b
end;

structure Library: LIBRARY =
struct

(* functions *)

fun undefined _ = raise Match;

fun I x = x;
fun K x = fn _ => x;
fun curry f x y = f (x, y);
fun uncurry f (x, y) = f x y;

(*conditional application*)
fun b ? f = fn x => if b then f x else x;

(*composition with multiple args*)
fun (f oo g) x y = f (g x y);
fun (f ooo g) x y z = f (g x y z);
fun (f oooo g) x y z w = f (g x y z w);

(*function exponentiation: f (... (f x) ...) with n applications of f*)
fun funpow (0 : int) _ = I
  | funpow n f = f #> funpow (n - 1) f;

fun funpow_yield (0 : int) _ x = ([], x)
  | funpow_yield n f x = x |> f ||>> funpow_yield (n - 1) f |>> op ::;


(* user errors *)

exception ERROR of string;
fun error msg = raise ERROR msg;

fun cat_error "" msg = error msg
  | cat_error msg "" = error msg
  | cat_error msg1 msg2 = error (msg1 ^ "\n" ^ msg2);

fun assert_all pred list msg =
  let
    fun ass [] = ()
      | ass (x :: xs) = if pred x then ass xs else error (msg x);
  in ass list end;


(* pairs *)

fun pair x y = (x, y);
fun rpair x y = (y, x);

fun fst (x, y) = x;
fun snd (x, y) = y;

fun eq_fst eq ((x1, _), (x2, _)) = eq (x1, x2);
fun eq_snd eq ((_, y1), (_, y2)) = eq (y1, y2);
fun eq_pair eqx eqy ((x1, y1), (x2, y2)) = eqx (x1, x2) andalso eqy (y1, y2);

fun swap (x, y) = (y, x);

fun apfst f (x, y) = (f x, y);
fun apsnd f (x, y) = (x, f y);
fun pairself f (x, y) = (f x, f y);


(* booleans *)

(*polymorphic equality*)
fun equal x y = x = y;
fun not_equal x y = x <> y;

(*combining predicates*)
fun p orf q = fn x => p x orelse q x;
fun p andf q = fn x => p x andalso q x;

val exists = List.exists;
val forall = List.all;


(* flags *)

fun setmp_CRITICAL flag value f x =
  NAMED_CRITICAL "setmp" (fn () => Unsynchronized.setmp flag value f x);

fun setmp_thread_data tag orig_data data f x =
  uninterruptible (fn restore_attributes => fn () =>
    let
      val _ = Thread.setLocal (tag, data);
      val result = Exn.capture (restore_attributes f) x;
      val _ = Thread.setLocal (tag, orig_data);
    in Exn.release result end) ();



(** lists **)

fun single x = [x];

fun the_single [x] = x
  | the_single _ = raise List.Empty;

fun singleton f x = the_single (f [x]);

fun yield_singleton f x = f [x] #>> the_single;

fun perhaps_apply funs arg =
  let
    fun app [] res = res
      | app (f :: fs) (changed, x) =
          (case f x of
            NONE => app fs (changed, x)
          | SOME x' => app fs (true, x'));
  in (case app funs (false, arg) of (false, _) => NONE | (true, arg') => SOME arg') end;

fun perhaps_loop f arg =
  let
    fun loop (changed, x) =
      (case f x of
        NONE => (changed, x)
      | SOME x' => loop (true, x'));
  in (case loop (false, arg) of (false, _) => NONE | (true, arg') => SOME arg') end;


(* fold -- old versions *)

(*the following versions of fold are designed to fit nicely with infixes*)

(*  (op @) (e, [x1, ..., xn])  ===>  ((e @ x1) @ x2) ... @ xn
    for operators that associate to the left (TAIL RECURSIVE)*)
fun foldl (f: 'a * 'b -> 'a) : 'a * 'b list -> 'a =
  let fun itl (e, [])  = e
        | itl (e, a::l) = itl (f(e, a), l)
  in  itl end;

(*  (op @) ([x1, ..., xn], e)  ===>   x1 @ (x2 ... @ (xn @ e))
    for operators that associate to the right (not tail recursive)*)
fun foldr f (l, e) =
  let fun itr [] = e
        | itr (a::l) = f(a, itr l)
  in  itr l  end;

(*  (op @) [x1, ..., xn]  ===>  ((x1 @ x2) @ x3) ... @ xn
    for operators that associate to the left (TAIL RECURSIVE)*)
fun foldl1 f [] = raise List.Empty
  | foldl1 f (x :: xs) = foldl f (x, xs);

(*  (op @) [x1, ..., xn]  ===>   x1 @ (x2 ... @ (x[n-1] @ xn))
    for n > 0, operators that associate to the right (not tail recursive)*)
fun foldr1 f [] = raise List.Empty
  | foldr1 f l =
      let fun itr [x] = x
            | itr (x::l) = f(x, itr l)
      in  itr l  end;


(* basic list functions *)

fun eq_list eq (list1, list2) =
  pointer_eq (list1, list2) orelse
    let
      fun eq_lst (x :: xs, y :: ys) = eq (x, y) andalso eq_lst (xs, ys)
        | eq_lst _ = true;
    in length list1 = length list2 andalso eq_lst (list1, list2) end;

fun maps f [] = []
  | maps f (x :: xs) = f x @ maps f xs;

val filter = List.filter;
fun filter_out f = filter (not o f);
val map_filter = List.mapPartial;

fun take (0: int) xs = []
  | take _ [] = []
  | take n (x :: xs) = x :: take (n - 1) xs;

fun drop (0: int) xs = xs
  | drop _ [] = []
  | drop n (x :: xs) = drop (n - 1) xs;

fun chop (0: int) xs = ([], xs)
  | chop _ [] = ([], [])
  | chop n (x :: xs) = chop (n - 1) xs |>> cons x;

fun chop_groups n list =
  (case chop (Int.max (n, 1)) list of
    ([], _) => []
  | (g, rest) => g :: chop_groups n rest);


(*return nth element of a list, where 0 designates the first element;
  raise Subscript if list too short*)
fun nth xs i = List.nth (xs, i);

fun nth_list xss i = nth xss i handle General.Subscript => [];

fun nth_map 0 f (x :: xs) = f x :: xs
  | nth_map n f (x :: xs) = x :: nth_map (n - 1) f xs
  | nth_map (_: int) _ [] = raise Subscript;

fun nth_drop n xs =
  List.take (xs, n) @ List.drop (xs, n + 1);

fun map_index f =
  let
    fun map_aux (_: int) [] = []
      | map_aux i (x :: xs) = f (i, x) :: map_aux (i + 1) xs
  in map_aux 0 end;

fun fold_index f =
  let
    fun fold_aux (_: int) [] y = y
      | fold_aux i (x :: xs) y = fold_aux (i + 1) xs (f (i, x) y)
  in fold_aux 0 end;

fun map_range f i =
  let
    fun map_aux (k: int) =
      if k < i then f k :: map_aux (k + 1) else []
  in map_aux 0 end;

fun fold_range f i =
  let
    fun fold_aux (k: int) y =
      if k < i then fold_aux (k + 1) (f k y) else y
  in fold_aux 0 end;


(*rear decomposition*)
fun split_last [] = raise List.Empty
  | split_last [x] = ([], x)
  | split_last (x :: xs) = apfst (cons x) (split_last xs);

(*find first element satisfying predicate*)
val find_first = List.find;

(*find position of first element satisfying a predicate*)
fun find_index pred =
  let fun find (_: int) [] = ~1
        | find n (x :: xs) = if pred x then n else find (n + 1) xs;
  in find 0 end;

(*get first element by lookup function*)
fun get_first _ [] = NONE
  | get_first f (x :: xs) =
      (case f x of
        NONE => get_first f xs
      | some => some);

fun get_index f =
  let
    fun get (_: int) [] = NONE
      | get i (x :: xs) =
          (case f x of
            NONE => get (i + 1) xs
          | SOME y => SOME (i, y))
  in get 0 end;

val flat = List.concat;

fun unflat (xs :: xss) ys =
      let val (ps, qs) = chop (length xs) ys
      in ps :: unflat xss qs end
  | unflat [] [] = []
  | unflat _ _ = raise ListPair.UnequalLengths;

fun grouped n comb f = chop_groups n #> comb (map f) #> flat;

fun burrow f xss = unflat xss (f (flat xss));

fun burrow_options f os = map (try hd) (burrow f (map the_list os));

fun fold_burrow f xss s =
  apfst (unflat xss) (f (flat xss) s);

(*separate s [x1, x2, ..., xn]  ===>  [x1, s, x2, s, ..., s, xn]*)
fun separate s (x :: (xs as _ :: _)) = x :: s :: separate s xs
  | separate _ xs = xs;

fun surround s (x :: xs) = s :: x :: surround s xs
  | surround s [] = [s];

(*make the list [x, x, ..., x] of length n*)
fun replicate (n: int) x =
  let fun rep (0, xs) = xs
        | rep (n, xs) = rep (n - 1, x :: xs)
  in
    if n < 0 then raise Subscript
    else rep (n, [])
  end;


(* direct product *)

fun map_product f _ [] = []
  | map_product f [] _ = []
  | map_product f (x :: xs) ys = map (f x) ys @ map_product f xs ys;

fun fold_product f _ [] z = z
  | fold_product f [] _ z = z
  | fold_product f (x :: xs) ys z = z |> fold (f x) ys |> fold_product f xs ys;


(* lists of pairs *)

fun map2 _ [] [] = []
  | map2 f (x :: xs) (y :: ys) = f x y :: map2 f xs ys
  | map2 _ _ _ = raise ListPair.UnequalLengths;

fun fold2 f [] [] z = z
  | fold2 f (x :: xs) (y :: ys) z = fold2 f xs ys (f x y z)
  | fold2 f _ _ _ = raise ListPair.UnequalLengths;

fun fold_rev2 f [] [] z = z
  | fold_rev2 f (x :: xs) (y :: ys) z = f x y (fold_rev2 f xs ys z)
  | fold_rev2 f _ _ _ = raise ListPair.UnequalLengths;

fun forall2 P [] [] = true
  | forall2 P (x :: xs) (y :: ys) = P x y andalso forall2 P xs ys
  | forall2 P _ _ = raise ListPair.UnequalLengths;

fun map_split f [] = ([], [])
  | map_split f (x :: xs) =
      let
        val (y, w) = f x;
        val (ys, ws) = map_split f xs;
      in (y :: ys, w :: ws) end;

fun zip_options (x :: xs) (SOME y :: ys) = (x, y) :: zip_options xs ys
  | zip_options (_ :: xs) (NONE :: ys) = zip_options xs ys
  | zip_options _ [] = []
  | zip_options [] _ = raise ListPair.UnequalLengths;

(*combine two lists forming a list of pairs:
  [x1, ..., xn] ~~ [y1, ..., yn]  ===>  [(x1, y1), ..., (xn, yn)]*)
fun [] ~~ [] = []
  | (x :: xs) ~~ (y :: ys) = (x, y) :: (xs ~~ ys)
  | _ ~~ _ = raise ListPair.UnequalLengths;

(*inverse of ~~; the old 'split':
  [(x1, y1), ..., (xn, yn)]  ===>  ([x1, ..., xn], [y1, ..., yn])*)
val split_list = ListPair.unzip;

fun burrow_fst f xs = split_list xs |>> f |> op ~~;


(* prefixes, suffixes *)

fun is_prefix _ [] _ = true
  | is_prefix eq (x :: xs) (y :: ys) = eq (x, y) andalso is_prefix eq xs ys
  | is_prefix eq _ _ = false;

(* [x1, ..., xi, ..., xn]  --->  ([x1, ..., x(i-1)], [xi, ..., xn])
   where xi is the first element that does not satisfy the predicate*)
fun take_prefix (pred : 'a -> bool)  (xs: 'a list) : 'a list * 'a list =
  let fun take (rxs, []) = (rev rxs, [])
        | take (rxs, x :: xs) =
            if  pred x  then  take(x :: rxs, xs)  else  (rev rxs, x :: xs)
  in  take([], xs)  end;

fun chop_prefix eq ([], ys) = ([], ([], ys))
  | chop_prefix eq (xs, []) = ([], (xs, []))
  | chop_prefix eq (xs as x :: xs', ys as y :: ys') =
      if eq (x, y) then
        let val (ps', xys'') = chop_prefix eq (xs', ys')
        in (x :: ps', xys'') end
      else ([], (xs, ys));

(* [x1, ..., xi, ..., xn]  --->  ([x1, ..., xi], [x(i+1), ..., xn])
   where xi is the last element that does not satisfy the predicate*)
fun take_suffix _ [] = ([], [])
  | take_suffix pred (x :: xs) =
      (case take_suffix pred xs of
        ([], sffx) => if pred x then ([], x :: sffx) else ([x], sffx)
      | (prfx, sffx) => (x :: prfx, sffx));

fun prefixes1 [] = []
  | prefixes1 (x :: xs) = map (cons x) ([] :: prefixes1 xs);

fun prefixes xs = [] :: prefixes1 xs;

fun suffixes1 xs = map rev (prefixes1 (rev xs));
fun suffixes xs = [] :: suffixes1 xs;



(** integers **)

(* lists of integers *)

(*make the list [from, from + 1, ..., to]*)
fun ((i: int) upto j) =
  if i > j then [] else i :: (i + 1 upto j);

(*make the list [from, from - 1, ..., to]*)
fun ((i: int) downto j) =
  if i < j then [] else i :: (i - 1 downto j);


(* convert integers to strings *)

(*expand the number in the given base;
  example: radixpand (2, 8) gives [1, 0, 0, 0]*)
fun radixpand (base, num) : int list =
  let
    fun radix (n, tail) =
      if n < base then n :: tail
      else radix (n div base, (n mod base) :: tail)
  in radix (num, []) end;

(*expands a number into a string of characters starting from "zerochar";
  example: radixstring (2, "0", 8) gives "1000"*)
fun radixstring (base, zerochar, num) =
  let val offset = ord zerochar;
      fun chrof n = chr (offset + n)
  in implode (map chrof (radixpand (base, num))) end;


local
  val zero = ord "0";
  val small = 10000: int;
  val small_table = Vector.tabulate (small, Int.toString);
in

fun string_of_int i =
  if i < 0 then Int.toString i
  else if i < 10 then chr (zero + i)
  else if i < small then Vector.sub (small_table, i)
  else Int.toString i;

end;

fun signed_string_of_int i =
  if i < 0 then "-" ^ string_of_int (~ i) else string_of_int i;

fun string_of_indexname (a, 0) = a
  | string_of_indexname (a, i) = a ^ "_" ^ string_of_int i;


(* read integers *)

fun read_radix_int radix cs =
  let
    val zero = ord "0";
    val limit = zero + radix;
    fun scan (num, []) = (num, [])
      | scan (num, c :: cs) =
          if zero <= ord c andalso ord c < limit then
            scan (radix * num + (ord c - zero), cs)
          else (num, c :: cs);
  in scan (0, cs) end;

val read_int = read_radix_int 10;

fun oct_char s = chr (#1 (read_radix_int 8 (raw_explode s)));



(** strings **)

(* functions tuned for strings, avoiding explode *)

fun nth_string str i =
  (case try String.substring (str, i, 1) of
    SOME s => s
  | NONE => raise Subscript);

fun fold_string f str x0 =
  let
    val n = size str;
    fun iter (x, i) =
      if i < n then iter (f (String.substring (str, i, 1)) x, i + 1) else x;
  in iter (x0, 0) end;

fun exists_string pred str =
  let
    val n = size str;
    fun ex i = i < n andalso (pred (String.substring (str, i, 1)) orelse ex (i + 1));
  in ex 0 end;

fun forall_string pred = not o exists_string (not o pred);

fun first_field sep str =
  let
    val n = size sep;
    val len = size str;
    fun find i =
      if i + n > len then NONE
      else if String.substring (str, i, n) = sep then SOME i
      else find (i + 1);
  in
    (case find 0 of
      NONE => NONE
    | SOME i => SOME (String.substring (str, 0, i), String.extract (str, i + n, NONE)))
  end;

(*enclose in brackets*)
fun enclose lpar rpar str = lpar ^ str ^ rpar;
fun unenclose str = String.substring (str, 1, size str - 2);

(*simple quoting (does not escape special chars)*)
val quote = enclose "\"" "\"";

val cartouche = enclose "\\<open>" "\\<close>";

fun space_implode a bs = implode (separate a bs);

val commas = space_implode ", ";
val commas_quote = commas o map quote;

val cat_lines = space_implode "\n";

(*space_explode "." "h.e..l.lo" = ["h", "e", "", "l", "lo"]*)
fun space_explode _ "" = []
  | space_explode sep s = String.fields (fn c => str c = sep) s;

val split_lines = space_explode "\n";

fun plain_words s = space_explode "_" s |> space_implode " ";

fun prefix_lines "" txt = txt
  | prefix_lines prfx txt = txt |> split_lines |> map (fn s => prfx ^ s) |> cat_lines;

fun prefix prfx s = prfx ^ s;
fun suffix sffx s = s ^ sffx;

fun unprefix prfx s =
  if String.isPrefix prfx s then String.substring (s, size prfx, size s - size prfx)
  else raise Fail "unprefix";

fun unsuffix sffx s =
  if String.isSuffix sffx s then String.substring (s, 0, size s - size sffx)
  else raise Fail "unsuffix";

val trim_line = perhaps (try (unsuffix "\n"));

fun replicate_string (0: int) _ = ""
  | replicate_string 1 a = a
  | replicate_string k a =
      if k mod 2 = 0 then replicate_string (k div 2) (a ^ a)
      else replicate_string (k div 2) (a ^ a) ^ a;

fun translate_string f = String.translate (f o String.str);

(*crude matching of str against simple glob pat*)
fun match_string pat str =
  let
    fun match [] _ = true
      | match (p :: ps) s =
          size p <= size s andalso
            (case try (unprefix p) s of
              SOME s' => match ps s'
            | NONE => match (p :: ps) (String.substring (s, 1, size s - 1)));
  in match (space_explode "*" pat) str end;


(** reals **)

val string_of_real = Real.fmt (StringCvt.GEN NONE);

fun signed_string_of_real x =
  if x < 0.0 then "-" ^ string_of_real (~ x) else string_of_real x;



(** lists as sets -- see also Pure/General/ord_list.ML **)

(* canonical operations *)

fun member eq list x =
  let
    fun memb [] = false
      | memb (y :: ys) = eq (x, y) orelse memb ys;
  in memb list end;

fun insert eq x xs = if member eq xs x then xs else x :: xs;
fun remove eq x xs = if member eq xs x then filter_out (fn y => eq (x, y)) xs else xs;
fun update eq x xs = cons x (remove eq x xs);

fun inter eq xs = filter (member eq xs);

fun union eq = fold (insert eq);
fun subtract eq = fold (remove eq);

fun merge eq (xs, ys) =
  if pointer_eq (xs, ys) then xs
  else if null xs then ys
  else fold_rev (insert eq) ys xs;


(* subset and set equality *)

fun subset eq (xs, ys) = forall (member eq ys) xs;

fun eq_set eq (xs, ys) =
  eq_list eq (xs, ys) orelse
    (subset eq (xs, ys) andalso subset (eq o swap) (ys, xs));


(*makes a list of the distinct members of the input; preserves order, takes
  first of equal elements*)
fun distinct eq lst =
  let
    fun dist (rev_seen, []) = rev rev_seen
      | dist (rev_seen, x :: xs) =
          if member eq rev_seen x then dist (rev_seen, xs)
          else dist (x :: rev_seen, xs);
  in dist ([], lst) end;

(*returns a list containing all repeated elements exactly once; preserves
  order, takes first of equal elements*)
fun duplicates eq lst =
  let
    fun dups (rev_dups, []) = rev rev_dups
      | dups (rev_dups, x :: xs) =
          if member eq rev_dups x orelse not (member eq xs x) then
            dups (rev_dups, xs)
          else dups (x :: rev_dups, xs);
  in dups ([], lst) end;

fun has_duplicates eq =
  let
    fun dups [] = false
      | dups (x :: xs) = member eq xs x orelse dups xs;
  in dups end;


(* matrices *)

fun map_transpose f xss =
  let
    val n =
      (case distinct (op =) (map length xss) of
        [] => 0
      | [n] => n
      | _ => raise ListPair.UnequalLengths);
  in map_range (fn m => f (map (fn xs => nth xs m) xss)) n end;



(** lists as multisets **)

fun remove1 eq x [] = []
  | remove1 eq x (y :: ys) = if eq (x, y) then ys else y :: remove1 eq x ys;

fun combine eq xs ys = fold (remove1 eq) ys xs @ ys;

fun submultiset _ ([], _)  = true
  | submultiset eq (x :: xs, ys) = member eq ys x andalso submultiset eq (xs, remove1 eq x ys);



(** orders **)

fun is_equal EQUAL = true
  | is_equal _ = false;

fun rev_order LESS = GREATER
  | rev_order EQUAL = EQUAL
  | rev_order GREATER = LESS;

(*assume rel is a linear strict order*)
fun make_ord rel (x, y) =
  if rel (x, y) then LESS
  else if rel (y, x) then GREATER
  else EQUAL;

fun bool_ord (false, true) = LESS
  | bool_ord (true, false) = GREATER
  | bool_ord _ = EQUAL;

val int_ord = Int.compare;
val string_ord = String.compare;

fun fast_string_ord (s1, s2) =
  if pointer_eq (s1, s2) then EQUAL
  else (case int_ord (size s1, size s2) of EQUAL => string_ord (s1, s2) | ord => ord);

fun option_ord ord (SOME x, SOME y) = ord (x, y)
  | option_ord _ (NONE, NONE) = EQUAL
  | option_ord _ (NONE, SOME _) = LESS
  | option_ord _ (SOME _, NONE) = GREATER;

(*lexicographic product*)
fun prod_ord a_ord b_ord ((x, y), (x', y')) =
  (case a_ord (x, x') of EQUAL => b_ord (y, y') | ord => ord);

(*dictionary order -- in general NOT well-founded!*)
fun dict_ord elem_ord (x :: xs, y :: ys) =
      (case elem_ord (x, y) of EQUAL => dict_ord elem_ord (xs, ys) | ord => ord)
  | dict_ord _ ([], []) = EQUAL
  | dict_ord _ ([], _ :: _) = LESS
  | dict_ord _ (_ :: _, []) = GREATER;

(*lexicographic product of lists*)
fun list_ord elem_ord (xs, ys) =
  (case int_ord (length xs, length ys) of EQUAL => dict_ord elem_ord (xs, ys) | ord => ord);


(* sorting *)

(*stable mergesort -- preserves order of equal elements*)
fun mergesort unique ord =
  let
    fun merge (xs as x :: xs') (ys as y :: ys') =
          (case ord (x, y) of
            LESS => x :: merge xs' ys
          | EQUAL =>
              if unique then merge xs ys'
              else x :: merge xs' ys
          | GREATER => y :: merge xs ys')
      | merge [] ys = ys
      | merge xs [] = xs;

    fun merge_all [xs] = xs
      | merge_all xss = merge_all (merge_pairs xss)
    and merge_pairs (xs :: ys :: xss) = merge xs ys :: merge_pairs xss
      | merge_pairs xss = xss;

    fun runs (x :: y :: xs) =
          (case ord (x, y) of
             LESS => ascending y [x] xs
           | EQUAL =>
               if unique then runs (x :: xs)
               else ascending y [x] xs
           | GREATER => descending y [x] xs)
      | runs xs = [xs]

    and ascending x xs (zs as y :: ys) =
          (case ord (x, y) of
             LESS => ascending y (x :: xs) ys
           | EQUAL =>
               if unique then ascending x xs ys
               else ascending y (x :: xs) ys
           | GREATER => rev (x :: xs) :: runs zs)
      | ascending x xs [] = [rev (x :: xs)]

    and descending x xs (zs as y :: ys) =
          (case ord (x, y) of
             GREATER => descending y (x :: xs) ys
           | EQUAL =>
               if unique then descending x xs ys
               else (x :: xs) :: runs zs
           | LESS => (x :: xs) :: runs zs)
      | descending x xs [] = [x :: xs];

  in merge_all o runs end;

fun sort ord = mergesort false ord;
fun sort_distinct ord = mergesort true ord;

val sort_strings = sort string_ord;
fun sort_wrt key xs = sort (string_ord o pairself key) xs;


(* items tagged by integer index *)

(*insert tags*)
fun tag_list k [] = []
  | tag_list k (x :: xs) = (k:int, x) :: tag_list (k + 1) xs;

(*remove tags and suppress duplicates -- list is assumed sorted!*)
fun untag_list [] = []
  | untag_list [(k: int, x)] = [x]
  | untag_list ((k, x) :: (rest as (k', x') :: _)) =
      if k = k' then untag_list rest
      else x :: untag_list rest;

(*return list elements in original order*)
fun order_list list = untag_list (sort (int_ord o pairself fst) list);



(** random numbers **)

exception RANDOM;

fun rmod x y = x - y * Real.realFloor (x / y);

local
  val a = 16807.0;
  val m = 2147483647.0;
  val random_seed = Unsynchronized.ref 1.0;
in

fun random () = CRITICAL (fn () =>
  let val r = rmod (a * ! random_seed) m
  in (random_seed := r; r) end);

end;

fun random_range l h =
  if h < l orelse l < 0 then raise RANDOM
  else l + Real.floor (rmod (random ()) (real (h - l + 1)));



(** misc **)

fun divide_and_conquer decomp x =
  let val (ys, recomb) = decomp x
  in recomb (map (divide_and_conquer decomp) ys) end;

fun divide_and_conquer' decomp x s =
  let val ((ys, recomb), s') = decomp x s
  in recomb (fold_map (divide_and_conquer' decomp) ys s') end;


(*Partition a list into buckets  [ bi, b(i+1), ..., bj ]
   putting x in bk if p(k)(x) holds.  Preserve order of elements if possible.*)
fun partition_list p i j =
  let
    fun part (k: int) xs =
      if k > j then
        (case xs of
          [] => []
        | _ => raise Fail "partition_list")
      else
        let val (ns, rest) = List.partition (p k) xs
        in ns :: part (k + 1) rest end;
  in part (i: int) end;

fun partition_eq (eq: 'a * 'a -> bool) =
  let
    fun part [] = []
      | part (x :: ys) =
          let val (xs, xs') = List.partition (fn y => eq (x, y)) ys
          in (x :: xs) :: part xs' end;
  in part end;


(* serial numbers and abstract stamps *)

type serial = int;
val serial = Multithreading.serial;
val serial_string = string_of_int o serial;

datatype stamp = Stamp of serial;
fun stamp () = Stamp (serial ());


(* values of any type *)

(*note that the builtin exception datatype may be extended by new
  constructors at any time*)
structure Any = struct type T = exn end;


(* current directory *)

val cd = OS.FileSys.chDir;
val pwd = OS.FileSys.getDir;


(* getenv *)

fun getenv x =
  (case OS.Process.getEnv x of
    NONE => ""
  | SOME y => y);

fun getenv_strict x =
  (case getenv x of
    "" => error ("Undefined Isabelle environment variable: " ^ quote x)
  | y => y);

end;

structure Basic_Library: BASIC_LIBRARY = Library;
open Basic_Library;

