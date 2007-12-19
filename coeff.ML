signature COEFF =
sig
  type T;
  val add : T -> T -> T;
  val subtr : T -> T -> T;
  val mult : T -> T -> T;
  val div : T -> T -> T;
  val eq : T -> T -> bool;
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

structure Rational :> RATIONAL = 
struct
type T = int * int;
exception BadExpression of string;
fun eq x y = (x = y);

fun is_zero (0,_) = true
  | is_zero (_,_) = false

fun is_one (1,1) = true
  | is_one (_,_) = false

fun ord ((d1,n1), (d2,n2)) = 
    let val (lhs,rhs) = (d1*n2,d2*n1)
    in 
      if lhs < rhs  then General.LESS
      else if lhs = rhs then General.EQUAL
      else General.GREATER
    end

fun gcd (m,n) =
    if m=n then m
    else if m mod 2 = 0 then 
             if n mod 2 = 0 then 2 * gcd(m div 2, n div 2)
                            else gcd(m div 2, n) 
    else (*m odd*) 
         if n mod 2 = 0 then gcd(m, n div 2)
         else (*both odd*)
             if m<n then gcd((n-m) div 2, m) else gcd((m-n) div 2, n);


fun rat_reduce (_,0) = raise BadExpression 
			       "Divide by zero in rational coeffiecient"
  | rat_reduce (0,_) = (0,1)
  | rat_reduce (d,n)  = 
    if n < 0  then rat_reduce (~d, ~n)
    else 
      if  d < 0 then 
	let val (dr,nr) = rat_reduce (~d,n)
	in
	  (~dr,nr)
	end
      else 
	let val c = gcd(d,n)  
	in  
	  (d div c, n div c)
	end

fun rat_add (d1,n1) (d2,n2) =  rat_reduce (d1*n2 + d2*n1, n1*n2)
fun rat_subtr (d1,n1) (d2,n2) = rat_add (d1,n1) (~d2,n2)
fun rat_mult (d1,n1) (d2,n2) = rat_reduce (d1*d2, n1*n2)
fun rat_div (d1,n1) (d2,n2) = rat_mult (d1,n1) (n2,d2)

fun mk d n = rat_reduce (d,n)

fun pretty (d,n) = 
    if d=0 then Pretty.str "0"
    else
      let val bits = 
	      if n = 1 then [Int.toString (abs d)]
	      else ["(",Int.toString (abs d),"/",Int.toString n,")"]
      in 
	if d < 0  then 
	  Pretty.str (String.concat ("-"::bits))
	else 
	  Pretty.str (String.concat bits)
      end

val print = Pretty.writeln o pretty

val add = rat_add;
val subtr = rat_subtr;
val mult = rat_mult;
val div = rat_div;
end
