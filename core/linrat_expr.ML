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
exception parse_exp
end;

structure DB_LinratExpr =
struct 
exception BadExpression of string;
exception parse_exp;
structure Coeff = Rational;

type T = Coeff.T * (Coeff.T VName.NTab.T);  (* the first Coeff is for Pi *)

(* =====================================================================*)


fun kv_eq ((k1,v1),(k2,v2)) = 
    VName.name_eq (k1, k2) andalso Coeff.eq v1 v2


fun eq (x,xT) (y,yT) = 
    if Coeff.eq x y then 
      ListPair.all kv_eq (VName.NTab.list_of xT,VName.NTab.list_of yT)
	    handle ListPair.UnequalLengths => false
    else false;

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

val zero = (Coeff.zero, empty);
fun is_zero (cpi,vars) = (Coeff.is_zero cpi 
                          andalso VName.NTab.is_name_empty vars);

val pi = (Coeff.one, empty);

fun mk const kv_list =
    (const, fold VName.NTab.update kv_list empty )

fun mk_var nm = mk Coeff.zero [(nm, Coeff.mk 1 1)];

(* reduce exprs to a normal by removing variables whose Coeff = 0 ======*)
fun reduce_expr (p, t) = 
    let fun red (k,v) tab = 
	          if  Coeff.is_zero v
	          then VName.NTab.delete k tab
	          else tab
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

(*
fun pretty (pi, tab) =
    let fun pretty1 (k,v) =
	    if (Coeff.is_one v) then Pretty.str k
	    else Pretty.block [Coeff.pretty v, Pretty.str (" "^k)]
	val ppi  = if (Coeff.is_zero pi) then [] else [pretty1 ("\\pi", pi)]
	val plist = VName.NTab.fold (cons o pretty1) tab ppi
    in 
	case plist of [] => Pretty.str "0"
		    | _ => (Pretty.block o Pretty.separate " +" o rev) plist
    end
*)

fun pretty1 (k,v) =
    if Coeff.is_one (Coeff.subtr Coeff.zero v)
    then Pretty.block [Pretty.str "-", Pretty.str k]
    else (
	if Coeff.is_one v then Pretty.str k
	else Pretty.block [Coeff.pretty v, Pretty.str (" "^k)])

fun pretty_list [] = [Pretty.str "0"]
  | pretty_list [x] = [pretty1 x]
  | pretty_list (x::(xs as ((k,v)::ys))) =
    (pretty1 x)::
    (if Coeff.ord (v, Coeff.zero) = LESS
     then Pretty.str " - " :: pretty_list ((k,Coeff.subtr Coeff.zero v)::ys)
     else Pretty.str " + " :: pretty_list xs)


fun pretty (pi, tab) =
    let val tlist = rev (VName.NTab.list_of tab)
    in (Pretty.block o pretty_list)
	   (if Coeff.is_zero pi then tlist
	    else ("\\pi", pi)::tlist)
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
       (* there is a pat var not in target, put difference between 
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


(* functionality to parse expressions *)
structure LinratParser =
struct
structure L = SimpleLexer
datatype expr = EXP of T
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
fun rat_term (RAT r, TOK(L.ID id)) = EXP (mk (Rational.mk 0 1) [(id,r)])
fun const_term (RAT r) = EXP (mk r [])
val one_pi = const_term (RAT (Rational.mk 1 1))
fun var_term (TOK(L.ID id)) = EXP (mk_var id)
fun plus (EXP a, EXP b) = EXP (add_expr a b)
fun minus (EXP a, EXP b) = EXP (subtr_expr a b)
fun neg (EXP a) = EXP (subtr_expr zero a)

(* the grammar, decorated with semantic operators *)
fun pi x = x|>((sym "\\" || noop)
		   --| (sym "p" || sym "P") --| (sym "i" || sym "I"))

fun coeff x = x|>(
	      ((num --| sym "/" -- num) >> rat)
		  || (sym "(" |-- coeff --| sym ")")
		  || num >> whole_num)
(* first term *)
fun term x = x|>(
	     ((sym "-" |-- term) >> neg)
		 || ((coeff --| (sym "*" || noop) --| pi) >> const_term)
		 || (pi >> (K one_pi))
		 || ((coeff --| (sym "*" || noop) -- ident) >> rat_term)
		 || (coeff >> const_term)
		 || (ident >> var_term)
		 || (sym "(" |-- expr --| sym ")"))

(* successive terms *)
and term' x = x|>(
	      (sym "+" |-- term)
		  || (sym "-" |-- term >> neg))
and terms x = x|>(
	      ((term' -- terms) >> plus)
		  || term')
and expr x = x|>(
	     (term -- terms >> plus)
		 || term)

fun parse str = let val (EXP e) = L.parse_with expr str
		in e
		end
    handle L.MoreTokens _ => raise parse_exp
	 | FAIL => raise parse_exp



end

val parse = LinratParser.parse

end

structure LinratExpr = DB_LinratExpr : LINRAT_EXPR


