signature GCD_SEMIRING =
sig
  type T
  (*exception DivisionByComplex*)
  val plus : T * T -> T
  val times : T * T -> T
  val zero : T
  val one : T
  val divide : T * T -> T
  val is_zero : T -> bool
  val is_one : T -> bool
  val to_string : T -> string
  
  val eq : T * T -> bool
  val gcd : T * T -> T             (* greatest common (real) divisor *)
  val total_ord : T * T -> order   (* any total order. plus and times need not be monotone *)
  val conjugate : T -> T           (* any involution *)
  val is_real : T -> bool          (* is equal to conjugate *)
end

structure IntSemiring : GCD_SEMIRING =
struct
  type T = int
  val plus = op+
  val times = op*
  val divide = op div
  val zero = 0
  val one = 1
  fun is_zero v = (v=zero)
  fun is_one v = (v=one)
  val to_string = Int.toString
  
  val eq = (op=)
  
  fun gcd (a,0) = a
    | gcd (0,a) = a
    | gcd (a,b) = gcd (b,a mod b)

  val total_ord = int_ord
  val conjugate = I
  val is_real = K true
end

(* semiring of complexified integers *)
structure CIntSemiring : GCD_SEMIRING =
struct
  type T = int * int
  
  fun plus ((r1,i1),(r2,i2)) = (r1+r2,i1+i2)
  fun times ((r1,i1),(r2,i2)) = (r1*r2 - i1*i2, r1*i2 + r2*i1)
  fun conjugate (r,i) = (r,~i)
  fun divide ((r1,i1),(r2,0)) = (r1 div r2, i1 div r2)
    | divide (num,denom) = divide (times (num, conjugate denom), times (denom, conjugate denom))
  val zero = (0,0)
  val one = (1,0)
  fun is_zero v = (v=zero)
  fun is_one v = (v=one)
  
  fun to_string (r,i) = let
    val rstr = if (r < 0) then ("-" ^ Int.toString (~r)) else (Int.toString r)
    val (isgn,iop,iabs) = if (i < 0) then ("-","-", ~i) else ("", "+", i)
    val istr = if iabs = 1 then "i" else Int.toString iabs ^ "i"
  in case (r,i) of (_,0) => rstr
                 | (0,_) => isgn ^ istr
                 | (_,_) => rstr ^ iop ^ istr
  end
  
  val eq = (op=)
  
  fun gcd ((r1,i1),(r2,i2)) = (IntSemiring.gcd (IntSemiring.gcd (r1,i1), IntSemiring.gcd (r2,i2)), 0)
    
  val total_ord = prod_ord int_ord int_ord
  fun is_real (_,i) = (i=0)
end
