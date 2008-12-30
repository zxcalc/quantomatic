(* linear combinations with rational coeffs (and pi) *)
signature LINRAT_EXPR
= sig 
include EXPR
structure Coeff : RATIONAL
val zero : T
val is_zero : T -> bool
val pi : T
val mk_var : VName.name -> T
val mk : Coeff.T -> (VName.name * Coeff.T) list -> T
val subtr_expr : T -> T -> T
val add_expr : T -> T -> T
val scale_expr : Coeff.T -> T -> T
val parse : string -> T
end;

structure DB_LinratExpr   =
struct 
exception BadExpression of string;
structure Coeff = Rational;
type T = Coeff.T * (Coeff.T VName.NTab.T);  (* the first Coeff is for Pi *)

(* =====================================================================*)


fun kv_eq ((k1,v1),(k2,v2)) = 
    VName.name_eq (k1, k2) andalso Coeff.eq v1 v2

fun eq (x,xT)  (y,yT) = 
    if  Coeff.eq x y then true
    else
      let 
	      val xs = VName.NTab.list_of xT
	      val ys = VName.NTab.list_of yT
	      fun test (xx,yy,b) = (kv_eq (xx,yy)) andalso b
      in 	 
	      ListPair.foldlEq test true (xs, ys)
	      handle ListPair.UnequalLengths => false	
      end

(* =====================================================================*)

fun kv_order ((k1,v1),(k2,v2)) = 
    if  VName.name_ord(k1,k2) = General.EQUAL
    then 
      Coeff.ord(v1,v2)
    else VName.name_ord(k1,k2);

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
	      val xs = VName.NTab.list_of xT
	      val ys = VName.NTab.list_of yT
      in 
	      kv_list_ord(xs,ys)
      end

(* =====================================================================*)

fun get_vars (_,tab)  = VName.NTab.get_nameset tab;

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
	          [(Pretty.block [(Coeff.pretty v), Pretty.str "*",VName.pretty_name k])]
	        | pretty1 (k,v) pl =
	          (Pretty.block [(Coeff.pretty v), Pretty.str "*",VName.pretty_name k])::
	          ((Pretty.str " + ")::pl)
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




(* functionality to parse expressions *)
structure LinratParser =
struct

exception Parse
structure L = SimpleLexer
datatype expr = EXP of DB_LinratExpr.T
	      | RAT of Rational.T
	      | TOK of L.token

(* wrap all the terminals in a TOK *)
val num = L.num >> TOK
val ident = L.ident >> TOK
val sym = fn s => (L.sym s) >> TOK
val noop = L.noop >> TOK

(* semantic ops *)
fun rat (TOK(L.INT n), TOK(L.INT d)) = RAT (Rational.mk n d)
fun whole_num n = rat (n,TOK(L.INT 1))
fun rat_term (RAT r, TOK(L.ID id)) = EXP (DB_LinratExpr.mk (Rational.mk 0 1) [(id,r)])
fun const_term (RAT r) = EXP (DB_LinratExpr.mk r [])
val one_pi = const_term (RAT (Rational.mk 1 1))
fun var_term (TOK(L.ID id)) = EXP (DB_LinratExpr.mk_var id)
fun plus (EXP a, EXP c) = EXP (DB_LinratExpr.add_expr a c)


(* the grammar, decorated with semantic operators *)
fun coeff x = x|>(
	      ((num --| sym "/" -- num) >> rat)
		  || (sym "(" |-- coeff --| sym ")")
		  || num >> whole_num)
fun term x = x|>(
	     ((coeff --| (sym "*" || noop) --| sym "Pi") >> const_term)
		 || (sym "Pi" >> (fn _ => one_pi))
		 || ((coeff --| (sym "*" || noop) -- ident) >> rat_term)
		 || (ident >> var_term))
fun terms x = x|>(
	      ((term --| sym "+" -- terms) >> plus)
		  || (sym "(" |-- terms --| sym ")")
		  || term)

fun parse str = let val (EXP e,ts) = terms ((Symbol.explode str)@[Symbol.eof])
		in if ts = [Symbol.eof] then e else raise Parse
		end


end



structure AngleExpr : LINRAT_EXPR = 
struct
open DB_LinratExpr
val parse = LinratParser.parse
end
