(*  Title:      Pure/General/rat.ML
    Author:     Tobias Nipkow, Florian Haftmann, TU Muenchen
    Author:     Makarius

Canonical implementation of exact rational numbers.
*)

signature RAT =
sig
  eqtype rat
  val of_int: int -> rat
  val make: int * int -> rat
  val dest: rat -> int * int
  val string_of_rat: rat -> string
  val signed_string_of_rat: rat -> string
  val ord: rat * rat -> order
  val le: rat -> rat -> bool
  val lt: rat -> rat -> bool
  val sign: rat -> order
  val abs: rat -> rat
  val add: rat -> rat -> rat
  val mult: rat -> rat -> rat
  val neg: rat -> rat
  val inv: rat -> rat
  val floor: rat -> int
  val ceil: rat -> int
end;

structure Rat : RAT =
struct

datatype rat = Rat of int * int;  (*numerator, positive (!) denominator*)

fun of_int i = Rat (i, 1);

fun common (p1, q1) (p2, q2) =
  let val m = Integer.lcm q1 q2
  in ((p1 * (m div q1), p2 * (m div q2)), m) end;

fun make (_, 0) = raise Div
  | make (p, q) =
    let
      val m = Integer.gcd p q;
      val (p', q') = (p div m, q div m);
    in Rat (if q' < 0 then (~ p', ~ q') else (p', q')) end

fun dest (Rat r) = r;

fun string_of_rat (Rat (p, 1)) = string_of_int p
  | string_of_rat (Rat (p, q)) = string_of_int p ^ "/" ^ string_of_int q;

fun signed_string_of_rat (Rat (p, 1)) = signed_string_of_int p
  | signed_string_of_rat (Rat (p, q)) = signed_string_of_int p ^ "/" ^ string_of_int q;

fun ord (Rat (p1, q1), Rat (p2, q2)) =
  (case (Integer.sign p1, Integer.sign p2) of
    (LESS, EQUAL) => LESS
  | (LESS, GREATER) => LESS
  | (EQUAL, LESS) => GREATER
  | (EQUAL, EQUAL) => EQUAL
  | (EQUAL, GREATER) => LESS
  | (GREATER, LESS) => GREATER
  | (GREATER, EQUAL) => GREATER
  | _ => int_ord (fst (common (p1, q1) (p2, q2))));

fun le a b = ord (a, b) <> GREATER;
fun lt a b = ord (a, b) = LESS;

fun sign (Rat (p, _)) = Integer.sign p;

fun abs (r as Rat (p, q)) = if p < 0 then Rat (~ p, q) else r;

fun add (Rat r1) (Rat r2) =
  let val ((m1, m2), n) = common r1 r2
  in make (m1 + m2, n) end;

fun mult (Rat (p1, q1)) (Rat (p2, q2)) = make (p1 * p2, q1 * q2);

fun neg (Rat (p, q)) = Rat (~ p, q);

fun inv (Rat (p, q)) =
  (case Integer.sign p of
    LESS => Rat (~ q, ~ p)
  | EQUAL => raise Div
  | GREATER => Rat (q, p));

fun floor (Rat (p, q)) = p div q;

fun ceil (Rat (p, q)) =
  (case Integer.div_mod p q of
    (m, 0) => m
  | (m, _) => m + 1);

end;

ML_system_overload (uncurry Rat.add) "+";
ML_system_overload (fn (a, b) => Rat.add a (Rat.neg b)) "-";
ML_system_overload (uncurry Rat.mult) "*";
ML_system_overload (fn (a, b) => Rat.mult a (Rat.inv b)) "/";
ML_system_overload (uncurry Rat.lt) "<";
ML_system_overload (uncurry Rat.le) "<=";
ML_system_overload (fn (a, b) => Rat.lt b a) ">";
ML_system_overload (fn (a, b) => Rat.le b a) ">=";
ML_system_overload Rat.neg "~";
ML_system_overload Rat.abs "abs";

ML_system_pp (fn _ => fn _ => fn x => Pretty.to_polyml (Pretty.str ("@" ^ Rat.string_of_rat x)));
