(*  Title:      Pure/General/scan.ML
    Author:     Markus Wenzel and Tobias Nipkow, TU Muenchen

Generic scanners (for potentially infinite input).
*)

infix 5 -- :-- :|-- |-- --| ^^;
infixr 5 ::: @@@;
infix 3 >>;
infixr 0 ||;

signature BASIC_SCAN =
sig
  type message = unit -> string
  (*error msg handler*)
  val !! : ('a * message option -> message) -> ('a -> 'b) -> 'a -> 'b
  (*apply function*)
  val >> : ('a -> 'b * 'c) * ('b -> 'd) -> 'a -> 'd * 'c
  (*alternative*)
  val || : ('a -> 'b) * ('a -> 'b) -> 'a -> 'b
  (*sequential pairing*)
  val -- : ('a -> 'b * 'c) * ('c -> 'd * 'e) -> 'a -> ('b * 'd) * 'e
  (*dependent pairing*)
  val :-- : ('a -> 'b * 'c) * ('b -> 'c -> 'd * 'e) -> 'a -> ('b * 'd) * 'e
  (*projections*)
  val :|-- : ('a -> 'b * 'c) * ('b -> 'c -> 'd * 'e) -> 'a -> 'd * 'e
  val |-- : ('a -> 'b * 'c) * ('c -> 'd * 'e) -> 'a -> 'd * 'e
  val --| : ('a -> 'b * 'c) * ('c -> 'd * 'e) -> 'a -> 'b * 'e
  (*concatenation*)
  val ^^ : ('a -> string * 'b) * ('b -> string * 'c) -> 'a -> string * 'c
  val ::: : ('a -> 'b * 'c) * ('c -> 'b list * 'd) -> 'a -> 'b list * 'd
  val @@@ : ('a -> 'b list * 'c) * ('c -> 'b list * 'd) -> 'a -> 'b list * 'd
  (*one element literal*)
  val $$ : string -> string list -> string * string list
  val ~$$ : string -> string list -> string * string list
end;

signature SCAN =
sig
  include BASIC_SCAN
  val prompt: string -> ('a -> 'b) -> 'a -> 'b
  val permissive: ('a -> 'b) -> 'a -> 'b
  val error: ('a -> 'b) -> 'a -> 'b
  val catch: ('a -> 'b) -> 'a -> 'b    (*exception Fail*)
  val fail: 'a -> 'b
  val fail_with: ('a -> message) -> 'a -> 'b
  val succeed: 'a -> 'b -> 'a * 'b
  val some: ('a -> 'b option) -> 'a list -> 'b * 'a list
  val one: ('a -> bool) -> 'a list -> 'a * 'a list
  val this: string list -> string list -> string list * string list
  val this_string: string -> string list -> string * string list
  val many: ('a -> bool) -> 'a list -> 'a list * 'a list
  val many1: ('a -> bool) -> 'a list -> 'a list * 'a list
  val optional: ('a -> 'b * 'a) -> 'b -> 'a -> 'b * 'a
  val option: ('a -> 'b * 'a) -> 'a -> 'b option * 'a
  val repeat: ('a -> 'b * 'a) -> 'a -> 'b list * 'a
  val repeat1: ('a -> 'b * 'a) -> 'a -> 'b list * 'a
  val single: ('a -> 'b * 'a) -> 'a -> 'b list * 'a
  val bulk: ('a -> 'b * 'a) -> 'a -> 'b list * 'a
  val max: ('a * 'a -> bool) -> ('b -> 'a * 'b) -> ('b -> 'a * 'b) -> 'b -> 'a * 'b
  val ahead: ('a -> 'b * 'c) -> 'a -> 'b * 'a
  val unless: ('a -> 'b * 'a) -> ('a -> 'c * 'd) -> 'a -> 'c * 'd
  val first: ('a -> 'b) list -> 'a -> 'b
  val state: 'a * 'b -> 'a * ('a * 'b)
  val depend: ('a -> 'b -> ('c * 'd) * 'e) -> 'a * 'b -> 'd * ('c * 'e)
  val peek: ('a -> 'b -> 'c * 'd) -> 'a * 'b -> 'c * ('a * 'd)
  val pass: 'a -> ('a * 'b -> 'c * ('d * 'e)) -> 'b -> 'c * 'e
  val lift: ('a -> 'b * 'c) -> 'd * 'a -> 'b * ('d * 'c)
  val unlift: (unit * 'a -> 'b * ('c * 'd)) -> 'a -> 'b * 'd
  val trace: ('a list -> 'b * 'c list) -> 'a list -> ('b * 'a list) * 'c list
  type 'a stopper
  val stopper: ('a list -> 'a) -> ('a -> bool) -> 'a stopper
  val is_stopper: 'a stopper -> 'a -> bool
  val finite': 'a stopper -> ('b * 'a list -> 'c * ('d * 'a list))
    -> 'b * 'a list -> 'c * ('d * 'a list)
  val finite: 'a stopper -> ('a list -> 'b * 'a list) -> 'a list -> 'b * 'a list
  val read: 'a stopper -> ('a list -> 'b * 'a list) -> 'a list -> 'b option
  val drain: string -> (string -> 'a -> 'b list * 'a) -> 'b stopper ->
    ('c * 'b list -> 'd * ('e * 'b list)) -> ('c * 'b list) * 'a -> ('d * ('e * 'b list)) * 'a
  type lexicon
  val is_literal: lexicon -> string list -> bool
  val literal: lexicon -> (string * 'a) list -> (string * 'a) list * (string * 'a) list
  val empty_lexicon: lexicon
  val extend_lexicon: string list -> lexicon -> lexicon
  val make_lexicon: string list list -> lexicon
  val dest_lexicon: lexicon -> string list
  val merge_lexicons: lexicon * lexicon -> lexicon
end;

structure Scan: SCAN =
struct


(** scanners **)

(* exceptions *)

type message = unit -> string;

exception MORE of string option;        (*need more input (prompt)*)
exception FAIL of message option;       (*try alternatives (reason of failure)*)
exception ABORT of message;             (*dead end*)

fun !! err scan xs = scan xs handle FAIL msg => raise ABORT (err (xs, msg));
fun permissive scan xs = scan xs handle MORE _ => raise FAIL NONE | ABORT _ => raise FAIL NONE;
fun strict scan xs = scan xs handle MORE _ => raise FAIL NONE;
fun prompt str scan xs = scan xs handle MORE NONE => raise MORE (SOME str);
fun error scan xs = scan xs handle ABORT msg => Library.error (msg ());

fun catch scan xs = scan xs
  handle ABORT msg => raise Fail (msg ())
    | FAIL msg => raise Fail (case msg of NONE => "Syntax error" | SOME m => m ());


(* scanner combinators *)

fun (scan >> f) xs = scan xs |>> f;

fun (scan1 || scan2) xs = scan1 xs handle FAIL _ => scan2 xs;

fun (scan1 :-- scan2) xs =
  let
    val (x, ys) = scan1 xs;
    val (y, zs) = scan2 x ys;
  in ((x, y), zs) end;

fun (scan1 -- scan2) = scan1 :-- (fn _ => scan2);
fun (scan1 :|-- scan2) = scan1 :-- scan2 >> #2;
fun (scan1 |-- scan2) = scan1 -- scan2 >> #2;
fun (scan1 --| scan2) = scan1 -- scan2 >> #1;
fun (scan1 ^^ scan2) = scan1 -- scan2 >> op ^;
fun (scan1 ::: scan2) = scan1 -- scan2 >> op ::;
fun (scan1 @@@ scan2) = scan1 -- scan2 >> op @;


(* generic scanners *)

fun fail _ = raise FAIL NONE;
fun fail_with msg_of xs = raise FAIL (SOME (msg_of xs));
fun succeed y xs = (y, xs);

fun some _ [] = raise MORE NONE
  | some f (x :: xs) =
      (case f x of SOME y => (y, xs) | _ => raise FAIL NONE);

fun one _ [] = raise MORE NONE
  | one pred (x :: xs) =
      if pred x then (x, xs) else raise FAIL NONE;

fun $$ a = one (fn s: string => s = a);
fun ~$$ a = one (fn s: string => s <> a);

fun this ys xs =
  let
    fun drop_prefix [] xs = xs
      | drop_prefix (_ :: _) [] = raise MORE NONE
      | drop_prefix (y :: ys) (x :: xs) =
          if (y: string) = x then drop_prefix ys xs else raise FAIL NONE;
  in (ys, drop_prefix ys xs) end;

fun this_string s = this (raw_explode s) >> K s;  (*primitive string -- no symbols here!*)

fun many _ [] = raise MORE NONE
  | many pred (lst as x :: xs) =
      if pred x then apfst (cons x) (many pred xs)
      else ([], lst);

fun many1 pred = one pred ::: many pred;

fun optional scan def = scan || succeed def;
fun option scan = (scan >> SOME) || succeed NONE;

fun repeat scan =
  let
    fun rep ys xs =
      (case (SOME (scan xs) handle FAIL _ => NONE) of
        NONE => (rev ys, xs)
      | SOME (y, xs') => rep (y :: ys) xs');
  in rep [] end;

fun repeat1 scan = scan ::: repeat scan;

fun single scan = scan >> (fn x => [x]);
fun bulk scan = scan -- repeat (permissive scan) >> (op ::);

fun max leq scan1 scan2 xs =
  (case (option scan1 xs, option scan2 xs) of
    ((NONE, _), (NONE, _)) => raise FAIL NONE           (*looses FAIL msg!*)
  | ((SOME tok1, xs'), (NONE, _)) => (tok1, xs')
  | ((NONE, _), (SOME tok2, xs')) => (tok2, xs')
  | ((SOME tok1, xs1'), (SOME tok2, xs2')) =>
      if leq (tok2, tok1) then (tok1, xs1') else (tok2, xs2'));

fun ahead scan xs = (fst (scan xs), xs);

fun unless test scan =
  ahead (option test) :-- (fn NONE => scan | _ => fail) >> #2;

fun first [] = fail
  | first (scan :: scans) = scan || first scans;


(* state based scanners *)

fun state (st, xs) = (st, (st, xs));

fun depend scan (st, xs) =
  let val ((st', y), xs') = scan st xs
  in (y, (st', xs')) end;

fun peek scan = depend (fn st => scan st >> pair st);

fun pass st scan xs =
  let val (y, (_, xs')) = scan (st, xs)
  in (y, xs') end;

fun lift scan (st, xs) =
  let val (y, xs') = scan xs
  in (y, (st, xs')) end;

fun unlift scan = pass () scan;


(* trace input *)

fun trace scan xs =
  let val (y, xs') = scan xs
  in ((y, take (length xs - length xs') xs), xs') end;


(* stopper *)

datatype 'a stopper = Stopper of ('a list -> 'a) * ('a -> bool);

fun stopper mk_stopper is_stopper = Stopper (mk_stopper, is_stopper);
fun is_stopper (Stopper (_, is_stopper)) = is_stopper;


(* finite scans *)

fun finite' (Stopper (mk_stopper, is_stopper)) scan (state, input) =
  let
    fun lost () = raise ABORT (fn () => "Bad scanner: lost stopper of finite scan!");

    fun stop [] = lost ()
      | stop lst =
          let val (xs, x) = split_last lst
          in if is_stopper x then ((), xs) else lost () end;
  in
    if exists is_stopper input then
      raise ABORT (fn () => "Stopper may not occur in input of finite scan!")
    else (strict scan --| lift stop) (state, input @ [mk_stopper input])
  end;

fun finite stopper scan = unlift (finite' stopper (lift scan));

fun read stopper scan xs =
  (case error (finite stopper (option scan)) xs of
    (y as SOME _, []) => y
  | _ => NONE);


(* infinite scans -- draining state-based source *)

fun drain def_prompt get stopper scan ((state, xs), src) =
  (scan (state, xs), src) handle MORE prompt =>
    (case get (the_default def_prompt prompt) src of
      ([], _) => (finite' stopper scan (state, xs), src)
    | (xs', src') => drain def_prompt get stopper scan ((state, xs @ xs'), src'));



(** datatype lexicon -- position tree **)

datatype lexicon = Lexicon of (bool * lexicon) Symtab.table;

val empty_lexicon = Lexicon Symtab.empty;

fun is_literal _ [] = false
  | is_literal (Lexicon tab) (c :: cs) =
      (case Symtab.lookup tab c of
        SOME (tip, lex) => tip andalso null cs orelse is_literal lex cs
      | NONE => false);


(* scan longest match *)

fun literal lexicon =
  let
    fun finish (SOME (res, rest)) = (rev res, rest)
      | finish NONE = raise FAIL NONE;
    fun scan _ res (Lexicon tab) [] = if Symtab.is_empty tab then finish res else raise MORE NONE
      | scan path res (Lexicon tab) (c :: cs) =
          (case Symtab.lookup tab (fst c) of
            SOME (tip, lex) =>
              let val path' = c :: path
              in scan path' (if tip then SOME (path', cs) else res) lex cs end
          | NONE => finish res);
  in scan [] NONE lexicon end;


(* build lexicons *)

fun extend_lexicon chrs lexicon =
  let
    fun ext [] lex = lex
      | ext (c :: cs) (Lexicon tab) =
          (case Symtab.lookup tab c of
            SOME (tip, lex) => Lexicon (Symtab.update (c, (tip orelse null cs, ext cs lex)) tab)
          | NONE => Lexicon (Symtab.update (c, (null cs, ext cs empty_lexicon)) tab));
  in if is_literal lexicon chrs then lexicon else ext chrs lexicon end;

fun make_lexicon chrss = fold extend_lexicon chrss empty_lexicon;


(* merge lexicons *)

fun dest path (Lexicon tab) = Symtab.fold (fn (d, (tip, lex)) =>
  let
    val path' = d :: path;
    val content = dest path' lex;
  in append (if tip then rev path' :: content else content) end) tab [];

val dest_lexicon = map implode o dest [];
fun merge_lexicons (lex1, lex2) = fold extend_lexicon (dest [] lex2) lex1;

end;

structure Basic_Scan: BASIC_SCAN = Scan;
open Basic_Scan;
