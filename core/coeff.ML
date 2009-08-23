signature COEFF =
sig
  type T;
  val add : T -> T -> T;
  val subtr : T -> T -> T;
  val mult : T -> T -> T;
  (* changed from div to divide to not clobber built-in -ak *)
  val divide : T -> T -> T;
  val eq : T -> T -> bool;
  val zero : T;
  val one : T;
  val is_zero : T -> bool;
  val is_one : T -> bool;
  val ord : (T * T) -> General.order;
  val pretty : T -> Pretty.T;
  val print : T -> unit;
  exception BadExpression of string;
end

signature RATIONAL = 
sig
  include COEFF;
  val mk : int -> int -> T
end


structure DB_Rational 
(* : RATIONAL *)
= struct
type T = 
     int (* numerator *)
     * int; (* denominator *)

exception BadExpression of string;

fun ord ((d1,n1), (d2,n2)) = 
    let val (lhs,rhs) = (d1*n2,d2*n1)
    in 
      if lhs < rhs  then General.LESS
      else if lhs = rhs then General.EQUAL
      else General.GREATER
    end

fun eq x y = (ord (x, y) = EQUAL);

(* use: Integer.gcd
fun gcd (m,n) =
    if m=n then m
    else if m mod 2 = 0 then 
             if n mod 2 = 0 then 2 * gcd(m div 2, n div 2)
                            else gcd(m div 2, n) 
    else (*m odd*) 
         if n mod 2 = 0 then gcd(m, n div 2)
         else (*both odd*)
             if m<n then gcd((n-m) div 2, m) else gcd((m-n) div 2, n);
*)

fun rat_reduce (_,0) = raise BadExpression 
			       "Divide by zero in rational coeffiecient"
  | rat_reduce (0,_) = (0,1)
  | rat_reduce (n,d)  = 
    if d < 0 then rat_reduce (~n, ~d)
    else 
      if  n < 0 then 
	      let val (nr,dr) = rat_reduce (~n,d)
	      in (~nr,dr) end
      else 
	      let val c = Integer.gcd n d
	      in (n div c, d div c) end

fun rat_add (n1,d1) (n2,d2) =  rat_reduce (n1*d2 + n2*d1, d1*d2)
fun rat_subtr (n1,d1) (n2,d2) = rat_add (n1,d1) (~n2,d2)
fun rat_mult (n1,d1) (n2,d2) = rat_reduce (n1*n2, d1*d2)
fun rat_div (n1,d1) (n2,d2) = rat_mult (n1,d1) (d2,n2)

fun mk n d = rat_reduce (n,d)

val zero = mk 0 1
val one = mk 1 1
fun is_zero r = eq r zero
fun is_one r = eq r one

fun pretty (n,d) = 
    if n=0 then Pretty.str "0"
    else
      let val bits = 
	      if d = 1 then [Int.toString (abs n)]
	      else ["(",Int.toString (abs n),"/",Int.toString d,")"]
      in 
	      if n < 0  then 
	        Pretty.str (String.concat ("-"::bits))
	      else 
	        Pretty.str (String.concat bits)
      end;

val print = Pretty.writeln o pretty

val add = rat_add;
val subtr = rat_subtr;
val mult = rat_mult;
val divide = rat_div;

end;

structure Rational : RATIONAL = DB_Rational;


structure DB_RationalMod2 = 
struct

open Rational;

fun rat_reduce r = 
    let 
      val (n2,d2) = DB_Rational.rat_reduce r
      val twod2 = (2 * d2)
      val (m,n3) = Integer.div_mod n2 twod2
    in ((if m < 0 then n3 - twod2 else n3),d2) end;

fun add (n1,d1) (n2,d2) =  rat_reduce (n1*d2 + n2*d1, d1*d2)
fun subtr (n1,d1) (n2,d2) = add (n1,d1) (~n2,d2)
fun mult (n1,d1) (n2,d2) = rat_reduce (n1*n2, d1*d2)
fun div (n1,d1) (n2,d2) = mult (n1,d1) (d2,n2)

fun mk n d = rat_reduce (n,d);

end;

structure RationalMod2 = DB_RationalMod2 : RATIONAL;

(*
install_pp (make_pp ["Rational", "T"] (Pretty.pprint o Rational.pretty));
*)
