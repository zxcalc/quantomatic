(* linear combinations with rational coeffs (and pi *)
signature LINRAT_EXPR
= sig 
include EXPR
structure Coeff : RATIONAL
val zero : T
val is_zero : T -> bool
val pi : T
val mk_var : VName.T -> T
val mk : Coeff.T -> (VName.T * Coeff.T) list -> T
val subtr_expr : T -> T -> T
val add_expr : T -> T -> T
val scale_expr : Coeff.T -> T -> T
end;

structure DB_LinratExpr   =
struct 
exception BadExpression of string;
structure Coeff = Rational;
type T = Coeff.T * (Coeff.T VName.NTab.T);  (* the first Coeff is for Pi *)

(* =====================================================================*)


fun kv_eq ((k1,v1),(k2,v2)) = 
    VName.eq k1 k2 andalso Coeff.eq v1 v2

fun eq (x,xT)  (y,yT) = 
    if  Coeff.eq x y then true
    else
      let 
	      val xs = VName.NTab.dest xT
	      val ys = VName.NTab.dest yT
	      fun test (xx,yy,b) = (kv_eq (xx,yy)) andalso b
      in 	 
	      ListPair.foldlEq test true (xs, ys)
	      handle ListPair.UnequalLengths => false	
      end

(* =====================================================================*)

fun kv_order ((k1,v1),(k2,v2)) = 
    if  VName.ord(k1,k2) = General.EQUAL
    then 
      Coeff.ord(v1,v2)
    else VName.ord(k1,k2);

fun  kv_list_ord ([],[]) = General.EQUAL
   | kv_list_ord ([],_) = General.LESS
   | kv_list_ord (_,[]) = General.GREATER
   | kv_list_ord (x::xs,y::ys) = 
     if  kv_order(x,y) <> General.EQUAL then 
       kv_order(x,y)
     else 
       kv_list_ord(xs,ys)
       
fun ord ((x,xT),  (y,yT)) =
    if Coeff.ord(x,y) <> General.EQUAL
    then Coeff.ord(x,y)
    else let 
	      val xs = VName.NTab.dest xT
	      val ys = VName.NTab.dest yT
      in 
	      kv_list_ord(xs,ys)
      end

(* =====================================================================*)

fun get_vars (_,tab)  = VName.NTab.get_names tab;

(* various mk functions ============================================ *)

val empty = VName.NTab.empty : Coeff.T VName.NTab.T;

val zero = (Coeff.mk 0 1, empty);
fun is_zero (cpi,vars) = (Coeff.is_zero cpi 
                          andalso VName.NTab.is_name_empty vars);

val pi = (Coeff.mk 1 1, empty);

fun mk_var nm = 
    (Coeff.mk 0 1, VName.NTab.update (nm, Coeff.mk 1 1) empty)

fun mk const kv_list =
    (const, fold VName.NTab.update kv_list empty )

(* reduce exprs to a normal by removing variables whose Coeff = 0 ======*)
fun reduce_expr (p, t) = 
    let fun red (k,v) tab = 
	          if  Coeff.is_zero v		
	          then VName.NTab.delete k t 
	          else t
    in 
      (p, VName.NTab.fold red t t)
    end

(* multiplication of an expression by a scalar ===================*)    
fun scale_expr s (e_pi, e_tab) = 
    let val rescale = (Coeff.mult s)
    in 
      let fun f (n,v) t = VName.NTab.update (n, (rescale v)) t 
      in 
	      let val newtab = 
		            VName.NTab.fold f e_tab VName.NTab.empty  
	      in 
	        reduce_expr ((rescale e_pi), newtab) 
	      end	  
      end
    end

(* arithmetic addition for expressions ========================*)
fun add_expr (pi1,tab1) (pi2,tab2) = 
    let fun f (k,v) t = 
	          if  VName.NTab.contains t k 
	          then 
	            VName.NTab.update (k, Coeff.add v (VName.NTab.get t k)) t
	          else 
	            VName.NTab.update (k, v) t
    in 
      let val newtab = VName.NTab.fold f tab2 tab1
      in 
	      reduce_expr (Coeff.add pi1 pi2, newtab)
      end
    end

(* arithmetic subtraction for expressions ========================*)
fun subtr_expr e1 e2 = 
    add_expr e1 (scale_expr (Coeff.mk ~1 1) e2)

(* =====================================================================*)
(* subs
   given LookUP of (k,v) name->expression pairs, substitute 
   v for k whereever it occurs in expression target
 *)					
(* BUG! 
  Need to check somewhere that in the lup forall pairs (k,v)
  no variable k occurs in any expression v
 *)
fun subs lup target = 
    let fun sub1 (k,v) texp =
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

(* =====================================================================*)

fun pretty (pi, tab) =
    let val ppi  = if (Coeff.is_zero pi) then []
		               else [Pretty.block [Coeff.pretty pi,Pretty.str "*Pi"]]
	      fun pretty1 (k,v) [] =
	          [(Pretty.block [(Coeff.pretty v), Pretty.str "*",VName.pretty k])]
	        | pretty1 (k,v) pl =
	          (Pretty.block [(Coeff.pretty v), Pretty.str "*",VName.pretty k])::
	          ((Pretty.str "  +  ")::pl)
    in 
      Pretty.block (VName.NTab.fold pretty1 tab ppi)
    end

val print = Pretty.writeln o pretty


(* =====================================================================*)
(* 
 val match : T -> T -> T VName.NTab.T -> T VName.NTab.T option;
 *)
fun match pat target already_matched =
    let val pat2 = subs already_matched pat
    in
      (* find an uninstantiated variable in pattern that we can push
         the difference between the expressions into *)
      case VName.NSet.get_local_bot 
             (VName.NSet.subtract (get_vars pat2) (get_vars target))
       (* if all pat vars are in target, check for direct equality *)
       of NONE => 
          if eq pat target then SOME already_matched else NONE
       (* there is a pat var not in target, show difference between 
          expressions into an instantiation of that var. *)
        | SOME match_var => 
          let val c = Coeff.div (Coeff.mk 1 1) 
                                (VName.NTab.get (snd pat) match_var)
		          (* 1/Coeff of match var *)
		          val rhs = add_expr 
			                    (scale_expr c (subtr_expr target pat2)) 
			                    (mk_var match_var) 
	        in
	          SOME (VName.NTab.update (match_var, rhs) already_matched)
	        end
    end;
end;


structure LinratExpr : LINRAT_EXPR = DB_LinratExpr;

(*
install_pp (make_pp ["LinratExpr", "T"] (Pretty.pprint o LinratExpr.pretty));
*)

structure AngleExpr = LinratExpr;

structure linrat_expr_test_vals = 
struct
structure exp = LinratExpr;
structure var = VName;
structure n = exp.Coeff;

val zero = exp.zero;
val pi = exp.pi;

val a_v = "a";
val b_v = "b";
val c_v = "c";

val a = exp.mk_var a_v;
val b = exp.mk_var b_v;
val c = exp.mk_var c_v;

val u = n.mk ~1 1
val w = n.mk 1 6;
val x = n.mk ~2 3;
val y = n.mk 7 8;
val z = n.mk 0 1;

fun op +(x,y) = exp.add_expr x y;
fun op -(x,y) = exp.subtr_expr x y;
fun op *(k,x) = exp.scale_expr k x;

val tab1a = var.NTab.update (a_v, pi) var.NTab.empty;
val tab1b = var.NTab.update (b_v, x*pi) var.NTab.empty;
val tab1c = var.NTab.update (c_v, zero) var.NTab.empty;
val tab1ab = var.NTab.update (b_v, pi) tab1a;
val tab1bc = var.NTab.update (c_v, x*pi) tab1b;
val tab1ca = var.NTab.update (a_v, zero) tab1c;

val tab2a = var.NTab.update (a_v, pi+x*c) var.NTab.empty;
val tab2b = var.NTab.update (b_v, x*pi+u*a) var.NTab.empty;
val tab2c = var.NTab.update (c_v, a) var.NTab.empty;
val tab2ab = var.NTab.update (b_v, pi+c) tab2a;
val tab2bc = var.NTab.update (c_v, x*pi) tab2b;
val tab2ca = var.NTab.update (a_v, zero) tab2c;

end;

          (* testing match:
           val SOME x = exp.match a zero var.NTab.empty;
               (VName.NTab.print exp.pretty) x;
           *)
