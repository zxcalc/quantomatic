signature GCD_SEMIRING =
sig
  type T
  exception DivisionByComplex
  val plus : T * T -> T
  val times : T * T -> T
  val zero : T
  val one : T
  val div_real : T * T -> T        (* raises DivisionByComplex is second arg is not real *)
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
  exception DivisionByComplex
  val plus = op+
  val times = op*
  val div_real = op div
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

structure CISemiring : GCD_SEMIRING =
struct
  type T = int * int
  exception DivisionByComplex
  
  fun plus ((r1,i1),(r2,i2)) = (r1+r2,i1+i2)
  fun times ((r1,i1),(r2,i2)) = (r1*r2 + i1*i2, r1*i2 + r2*i1)
  fun div_real ((r1,i1),(r2,0)) = (r1 div r2, i1 div r2)
    | div_real _ = raise DivisionByComplex
  val zero = (0,0)
  val one = (1,0)
  fun is_zero v = (v=zero)
  fun is_one v = (v=one)
  
  fun to_string (r,0) = (Int.toString r)
    | to_string (0,i) = (Int.toString i) ^ "i"
    | to_string (r,i) = (Int.toString r) ^ " + " ^ (Int.toString i) ^ "i"
  
  val eq = (op=)
  
  fun gcd ((r1,i1),(r2,i2)) = (IntSemiring.gcd (IntSemiring.gcd (r1,i1), IntSemiring.gcd (r2,i2)), 0)
    
  val total_ord = prod_ord int_ord int_ord
  fun conjugate (r,i) = (r,~i)
  fun is_real (_,i) = (i=0)
end
