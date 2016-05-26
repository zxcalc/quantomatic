signature GCD_SEMIRING =
sig
  type T
  (*exception DivisionByComplex*)
  val plus : T * T -> T
  val times : T * T -> T
  val neg : T -> T
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
  val complement : T -> T          (* the product of the Galois conjugates *)
                                   (* used for reduction to scalar normal form *)
  val is_real : T -> bool          (* is an integer! Needs to be in Z for tensor comparisons *)
end

structure IntSemiring : GCD_SEMIRING =
struct
  type T = int
  val plus = op+
  val times = op*
  val divide = op div
  fun neg v = ~v
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
  val complement = conjugate
  val is_real = K true
end

(* semiring of complexified integers *)
structure CIntSemiring : GCD_SEMIRING =
struct
  type T = int * int
  
  fun plus ((r1,i1),(r2,i2)) = (r1+r2,i1+i2)
  fun times ((r1,i1),(r2,i2)) = (r1*r2 - i1*i2, r1*i2 + r2*i1)
  fun neg (r,i) = (~r,~i)
  fun conjugate (r,i) = (r,~i)
  fun complement a = conjugate a
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

structure CubicIntegers : GCD_SEMIRING =
struct
type T = int * int * int

(*Considered as polynomials in Z[X]/(X^2+X+1)*)

  fun plus ((a,b,c),(d,e,f)) = (a+d,b+e,c+f)
  fun times ((a,b,c),(d,e,f)) = (a*d+e*c+b*f,a*e+b*d+c*f,c*d+e*b+a*f)

  fun neg ((a,b,c)) = (~a,~b,~c)
  fun minus (a,b) = plus(a,neg b)

(*Galois conjugates*)
  fun g1 (a,b,c) = (a,c,b)
  fun complement a = g1 a
  fun conjugate a = complement a


  fun norm a = let
      val (n,_,_) = times (a, complement a)
    in
      n
    end

  fun divide ((a,b,c),(d,0,0)) = (a div d,b div d,c div d)
    | divide (num, denom) = divide (times (num, complement denom), times (denom, complement denom))

  val zero = (0,0,0)
  val one = (1,0,0)

  fun is_zero v = (v=zero)
  fun is_one v = (v=one)

  fun to_string (a,b,c) = Pretty.string_of (Pretty.list "(" ")" (map (Pretty.str o Int.toString) [a,b,c]))


  val eq = (op=)     

  fun modulo (a,b) = minus(a,times(b,divide(a,b)))

(*Euclid, but return one if we reach a unit*)                       
  fun gcd (a,(0,0,0)) = if ((norm a) = 1) then one else a
    | gcd ((0,0,0),a) = if ((norm a) = 1) then one else a
    | gcd (a,b) = gcd (b,modulo(a,b))
      
fun total_ord ((a,b,c), (d,e,f)) =
 list_ord int_ord ([a,b,c], [d,e,f]);

  fun is_real (_,b,c) = (b=0 andalso c=0) (*actually is_int*)
end


structure Pi4Integers : GCD_SEMIRING =
struct
  type T = int * int * int * int

(*Considered as polynomials in Z[X]/(X^4+1) *)

  fun plus ((r,t1,t2,t3),(s,u1,u2,u3)) = (r+s,t1+u1,t2+u2,t3+u3)
  fun times ((r,t1,t2,t3),(s,u1,u2,u3)) =
    ((r*s)-(t1*u3)-(t2*u2)-(t3*u1),(r*u1)+(t1*s)-(t2*u3)-(t3*u2),(r*u2)+(t1*u1)+(t2*s)-(t3*u3),(r*u3)+(t1*u2)+(t2*u1)+(t3*s))
  fun neg a = times((~1,0,0,0),a)
  fun minus (a,b) = plus(a,neg b)



(*Galois conjugates*)
  fun g1 (a,b,c,d) = (a,d,~c,b)
  fun g2 (a,b,c,d) = (a,~b,c,~d)
  fun g3 (a,b,c,d) = (a,~d,~c,~b)


  fun complement a = times ( times ( g1 a, g2 a), g3 a)

  fun conjugate (r,t1,t2,t3) = (r,~t3,~t2,~t1)

  fun norm a = let
      val (n,_,_,_) = times (a, complement a)
    in
      n
    end

(*To be shown that this is indeed a Euclidean domain!*)
  fun divide ((r, t1, t2, t3),(s,0,0,0)) = (r div s,t1 div s,t2 div s,t3 div s)
    | divide (num, denom) = divide (times (num, complement denom), times (denom, complement denom))

  val zero = (0,0,0,0)
  val one = (1,0,0,0)

  fun is_zero v = (v=zero)
  fun is_one v = (v=one)

  fun to_string (r,t1,t2,t3) = Pretty.string_of (Pretty.list "(" ")" (map (Pretty.str o Int.toString) [r,t1,t2,t3]))


  val eq = (op=)     

  fun modulo (a,b) = minus(a,times(b,divide(a,b)))

(*Euclid, but return one if we reach a unit*)                       
  fun gcd (a,(0,0,0,0)) = if ((norm a) = 1) then one else a
    | gcd ((0,0,0,0),a) = if ((norm a) = 1) then one else a
    | gcd (a,b) = gcd (b,modulo(a,b))
      
fun total_ord ((r,t1,t2,t3), (s,u1,u2,u3)) =
 list_ord int_ord ([r,t1,t2,t3], [s,u1,u2,u3]);

  fun is_real (_,t1,t2,t3) = (t1=0 andalso t2=0 andalso t3=0) (*actually is_int*)
end
