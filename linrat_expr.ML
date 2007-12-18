(* linear combinations with rational coeffs (and pi *)
structure Linrat_expr :> EXPR =
struct 

type var = EXPR.var;

type rat = int * int;

fun gcd (m,n) =
    if m=n then m
    else if m mod 2 = 0 then 
             if n mod 2 = 0 then 2 * gcd(m div 2, n div 2)
                            else gcd(m div 2, n) 
    else (*m odd*) 
         if n mod 2 = 0 then gcd(m, n div 2)
         else (*both odd*)
             if m<n then gcd((n-m) div 2, m) else gcd((m-n) div 2, n);


fun reduce (d,n)  =
     


datatype T = ZERO | Pi | Var of var | Scalar of coeff * T | Sum of T * T ;

fun reduce 
    ZERO = ZERO
  | Pi = Pi
  | Var v = v
  | Sum(ZERO * ex) = reduce ex
  | Sum(ex,ZERO) = reduce ex
  | Scalar ((_,0),_) = raise BadExpression("Divide by zero in coefficient")
  | Scalar ((0,_),_) = ZERO
     


end
