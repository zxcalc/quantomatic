(* linear combinations with rational coeffs (and pi *)
structure Linrat_expr :> EXPR =
struct 
exception BadExpression of string;
type var = VName.T;

(*datatype basis = Pi | Var of var;*)
structure coeff = Rational;
(* the first coeff is for Pi *)
type T = coeff.T * (coeff.T VName.NTab.T);
type table = T VName.NTab.T;
type varexpr = T;

fun kv_eq ((k1,v1),(k2,v2)) = 
    VName.eq k1 k2 andalso coeff.eq v1 v2

fun kv_order ((k1,v1),(k2,v2)) = 
    if  VName.ord(k1,k2) = General.EQUAL
    then 
      coeff.ord(v1,v2)
    else VName.ord(k1,k2);

fun  kv_list_ord ([],[]) = General.EQUAL
   | kv_list_ord ([],_) = General.LESS
   | kv_list_ord (_,[]) = General.GREATER
   | kv_list_ord (x::xs,y::ys) = 
     if  kv_order(x,y) <> General.EQUAL then 
       kv_order(x,y)
     else 
       kv_list_ord(xs,ys)

fun eq (x,xT)  (y,yT) = 
    if  coeff.eq x y then true
    else
      let 
	val xs = VName.NTab.dest xT
	val ys = VName.NTab.dest yT
	fun test (xx,yy,b) = (kv_eq (xx,yy)) andalso b
      in 	 
	ListPair.foldlEq test true (xs, ys)
	handle ListPair.UnequalLengths => false	
      end
      
fun ord (x,xT)  (y,yT) =
    if coeff.ord(x,y) <> General.EQUAL
    then coeff.ord(x,y)
    else let 
	val xs = VName.NTab.dest xT
	val ys = VName.NTab.dest yT
      in 
	kv_list_ord(xs,ys)
      end

fun get_vars (_,tab)  = VName.NTab.get_names tab;

fun add_expr (pi1,tab1) (pi2,tab2) = 
    let fun f (k,v) t = 
	    if  VName.NTab.contains t k 
	    then 
	      VName.NTab.update (k, coeff.add v (VName.NTab.get t k)) t
	    else 
	      VName.NTab.update (k, v) t
    in 
      let val newtab = VName.NTab.fold f tab2 tab1
      in 
	(coeff.add pi1 pi2, newtab)
      end
    end

    

fun scale_expr s (e_pi, e_tab) = 
    let 
      val rescale = (coeff.mult s)
    in 
      let fun f (n,v) t = VName.NTab.update (n, (rescale v)) t 
      in 
	let val newtab = 
		VName.NTab.fold f e_tab VName.NTab.empty  
	in 
	  ((rescale e_pi), newtab) 
	end	  
      end
    end

(* BUG! 
Need to check somewhere that in the lup forall pairs (k,v)
no variable k occurs in any expression v
*)
fun subs lup target = 
    let 
      fun sub1 (k,v) texp =
	  let val (p,ttab) = texp
	  in 
	    if  VName.NTab.contains ttab k 
	    then 
	      add_expr (p, VName.NTab.delete k ttab)  
		       (scale_expr (VName.NTab.get ttab k) v)
	    else 
	      texp
	  end
    in 
      VName.NTab.fold sub1 lup target
    end


end
