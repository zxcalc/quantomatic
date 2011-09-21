(* linear combinations with rational coeffs (and pi) *)
signature LINRAT_EXPR
= sig 
  include EXPR
  structure Coeff : RATIONAL
  exception parse_exp
  val parse : string -> T
  val zero : T
  val is_zero : T -> bool
  val pi : T
  val mk_var : VName.name -> T
  val mk : Coeff.T -> (VName.name * Coeff.T) list -> T
  val subtr_expr : T -> T -> T
  val add_expr : T -> T -> T
  val scale_expr : Coeff.T -> T -> T
end;

structure DB_LinratExpr =
struct 

(* Variable names in expressions *)
structure VName :> NAMES where type name = string = StrName;

exception BadExpression of string;
exception parse_exp;
structure PiCoeff = RationalMod2;
structure Coeff = Rational;

type T = PiCoeff.T * (Coeff.T VName.NTab.T);  (* the PiCoeff is for Pi *)

(* =====================================================================*)


fun kv_eq ((k1,v1),(k2,v2)) = 
    VName.name_eq (k1, k2) andalso Coeff.eq v1 v2

(* don't use basis library version because it doesn't raise exception 
   on unequal length lists *)
fun forall_pair eq ([],[]) = true
  | forall_pair eq (h1::t1,h2::t2) = eq (h1,h2) andalso forall_pair eq (t1,t2)
  | forall_pair eq _ = raise ListPair.UnequalLengths;

fun eq (x,xT) (y,yT) = 
    if PiCoeff.eq x y then 
      forall_pair kv_eq (VName.NTab.list_of xT, VName.NTab.list_of yT)
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

fun try_rename1 n1 n2 (k,tab) = 
    (case VName.NTab.try_rename1 n1 n2 tab of 
       NONE => NONE
     | SOME tab2 => SOME (k,tab2));


structure CRnm = CompoundRenamingFun(
    structure Nm = VName
    type obj = T
    val get_nameset = get_vars
    val try_rename1 = try_rename1);


(* various mk functions ============================================ *)
val empty = VName.NTab.empty : Coeff.T VName.NTab.T;


val zero = (PiCoeff.zero, empty);
fun is_zero (cpi,vars) = 
    (PiCoeff.is_zero cpi andalso VName.NTab.is_name_empty vars);

val pi = (PiCoeff.one, empty);

fun mk const kv_list =
    (const, fold VName.NTab.update kv_list empty )

fun mk_var nm = mk PiCoeff.zero [(nm, Coeff.mk 1 1)];

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
    (PiCoeff.mult s e_pi, VName.NTab.map_all (K (Coeff.mult s)) e_tab);

(* arithmetic addition for expressions ========================*)
fun add_expr (pi1,tab1) (pi2,tab2) = 
    let 
      fun f (k,v) t = 
	        if VName.NTab.contains t k
	        then VName.NTab.map_entry (Coeff.add v) k t
	        else VName.NTab.update (k, v) t
      val newtab = VName.NTab.fold f tab2 tab1
    in reduce_expr (PiCoeff.add pi1 pi2, newtab) end

(* arithmetic subtraction for expressions ========================*)
fun subtr_expr e1 e2 = 
    add_expr e1 (scale_expr (Coeff.mk ~1 1) e2)

(* =====================================================================*)
(* subs
   given etab of name->expression table containing (k,expr) 
   (where expr doesn't contain k of course)
   substitutes k for expr in target, done for each element in the etab
   IMPROVE: travse vars of target: probably fewer of them. 
*)
fun subs etab target = 
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
      VName.NTab.fold sub1 etab target
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

fun pretty' pistr (pi, tab) =
    let val tlist = rev (VName.NTab.list_of tab)
    in (Pretty.block o pretty_list)
	   (if Coeff.is_zero pi then tlist
	    else (pistr, pi)::tlist)
    end


fun pretty_math fmt = case fmt
		       of AlgFormat.MATHEMATICA => pretty' "Pi"
			| AlgFormat.LATEX => pretty' "\\pi"
			| AlgFormat.PLAIN => pretty' "pi"
			| _ => raise AlgFormat.not_supported_exp

val pretty = pretty_math AlgFormat.LATEX;

val print = Pretty.writeln o pretty

(* =====================================================================*)
(* 
  val match : T -> T -> T VName.NTab.T -> T VName.NTab.T option;
  ASSUMES : pat and tgt may have common variables, but they will be 
  kept apart in the matching representation: we store everything in pairs: the tgt part and the expr part. Expression substitutions are thus a pair of expressions.  
*)
datatype match = Match of 
  (T     (* holds the tgt part of the expr *) 
  * T)   (* holds the pattern part of the expr *)
  VName.NTab.T; (* pattern var goes to tgt - pat expresion *) 

val empty_match = Match VName.NTab.empty; 

fun pretty_match (Match mtab) = 
    VName.NTab.pretty (fn (tgt_e, pat_e) => Pretty.block 
      [Pretty.str "(", pretty tgt_e, Pretty.str ")[of tgt] - (",
       pretty pat_e, Pretty.str ")[of pat]"])
      mtab; 
val print_match = Pretty.writeln o pretty_match;

fun subst_of_match (Match mtab) = 
    VName.NTab.map_all (fn n => fn (tgt_e,pat_e) => 
      subtr_expr tgt_e pat_e 
    ) mtab;

(* aux function for matching disjointly: eliminates one pattern variable; 
   assumes that pat2 has no variables which can be substituted by anything 
   in match. *)
fun match_elim1 pat2 tgt2 (match as Match mtab) =
    let val (pcoeff0,ptab) = pat2 in 
      case VName.NSet.get_local_bot (VName.NTab.get_nameset ptab)
       (* if all pat vars are in target, check for direct equality *)
       of NONE => (* pat2, not pat. *)
          if eq pat2 tgt2 then SOME match else NONE
       (* there is a pat var not in target, put difference between 
          expressions into an instantiation of that var. *)
        | SOME pv => 
          let (* 1/Coeff of match var *)
              val coeff_of_pv = VName.NTab.get ptab pv
              val c = Coeff.divide (Coeff.mk 1 1) coeff_of_pv
              val pat_subst = scale_expr c (pcoeff0, VName.NTab.delete pv ptab)
          in
            SOME (Match (VName.NTab.update (pv, (tgt2,pat_subst)) mtab))
          end
     end;

(* =====================================================================*)
(*  Apply substitution of pattern variables in mtab to variables 
    in pat and tgt. 
    e.g. given pat: c0 + c1*x1 + c2*x2 + ...
               tgt: k0 + k1*y1 + ...
               mtab: x1 |-> (t1,p1) 
                     note: t1 and p2 are the tgt and pat 
                           parts of the substitution
         then have: 
               pat := c0 + c2*x2 + ... - (c1*p1)
               tgt: k0 + k1*y1 + ... - (c1*t1)
*)
fun subst_in_match_pair (Match mtab) (pat,tgt) = 
    let 
      val (pcoeff0,ptab0) = pat
      fun sub1 (pv,pvcoeff) (pt as (pat,tgt)) =
          (case VName.NTab.lookup mtab pv of NONE => pt
           | SOME (tgt_sub_expr,pat_sub_expr) => 
             let val (pcoeff0,ptab) = pat 
	          in
              (subtr_expr (pcoeff0, VName.NTab.delete pv ptab)
			                    (scale_expr pvcoeff pat_sub_expr),
			         subtr_expr tgt (scale_expr pvcoeff tgt_sub_expr))		            
	          end)
    in 
      VName.NTab.fold sub1 ptab0 (pat,tgt)
    end

fun match pat tgt match =
    let val (pat2,tgt2) = subst_in_match_pair match (pat,tgt) 
    in match_elim1 (reduce_expr pat2) (reduce_expr tgt2) match end;

(* use a substitution to replace all variables that have been matched to 
   expressions. 
   ASSUMES: variables names in pat which created the match were disjoint from those in expr (pat variables will be introduced!);
*)
fun subst (Match mtab) expr = 
    let 
      val (xcoeff0,xtab0) = expr;
      fun sub1 (v,coeff) expr =
          (case VName.NTab.lookup mtab v of NONE => expr
           | SOME (tgt_sub_expr,pat_sub_expr) => 
             let val (xcoeff0,xtab) = expr
                 val expr' = (xcoeff0,VName.NTab.delete v xtab)
	          in
	            subtr_expr 
	              (add_expr expr' (scale_expr coeff tgt_sub_expr))
                (scale_expr coeff pat_sub_expr)
	          end)
    in 
      VName.NTab.fold sub1 xtab0 expr
    end

(* CHECK: do we need to subst in the target part? or just in the pat part. 
   This needs to be clarified. *)
fun compose_matches (Match mtab1) m2  = 
    Match (VName.NTab.map_all (fn n => fn (tgt_expr,pat_expr) => 
      (subst m2 tgt_expr, subst m2 pat_expr)) mtab1);

(* IMPROVE: look at subst_in_match_pair above and improve this code! 
   e.g. don't do set subtraction here - this code is called very oftem *)
fun match_sub pat2 target already_matched =
    case VName.NSet.get_local_bot (* only vars in pat2*)
           (VName.NSet.subtract (get_vars pat2) (get_vars target))
     (* if all pat vars are in target, check for direct equality *)
     of NONE => (* pat2, not pat. *)
        if eq (reduce_expr pat2) target then SOME already_matched else NONE
     (* there is a pat var not in target, put difference between 
        expressions into an instantiation of that var. *)
      | SOME match_var => 
        let val c = Coeff.divide (Coeff.mk 1 1) 
                 (* subs can have introduced previously unseen vars *)
                                 (VName.NTab.get (snd pat2) match_var) 
            (* 1/Coeff of match var *)
            val rhs = add_expr 
                        (scale_expr c (subtr_expr target pat2)) 
                        (mk_var match_var) (* IMPROVE: remove directly *)
        in
          SOME (VName.NTab.update (match_var, rhs) already_matched)
        end;

(* Elimination step for pat=target. Tries to express a variable in the
   equation in terms of the others and pushes the change into already_matched.
   Returns NONE in case of contradiction. *)
fun eliminate pat target already_matched =
    let val elim_previous = reduce_expr o (subs already_matched) 
        val pat2 = elim_previous pat
        val target2 = elim_previous target
        fun elim_sub ((x,l)::xs) ((y,r)::ys) ox oy =
            if x=y then (if Coeff.eq l r 
                         then elim_sub xs ys ox oy
                         else elim_sub xs ys ((x,Coeff.subtr l r)::ox) oy)
                   else elim_sub ((x,l)::xs) ys ((y,Coeff.subtr Coeff.zero r)::ox) oy
          | elim_sub [] ((y,r)::ys) ox oy = elim_sub [] ys ((y,Coeff.subtr Coeff.zero r)::ox) oy
          | elim_sub xs [] ox oy = (xs@ox, oy)
        val (pat3, target3) = elim_sub (VName.NTab.list_of (snd pat2))
                                       (VName.NTab.list_of (snd target2))
                                       [] []
    in match_sub (mk (fst pat) pat3) (mk (fst target) target3) already_matched end;

(* functionality to parse expressions *)
structure LinratParser =
struct
structure L = SimpleLexer
datatype expr = EXP of T
	            | RAT of Coeff.T
	            | TOK of L.token

(* wrap all the terminals in a TOK *)
val num = L.num >> TOK
val ident = L.ident >> TOK
val sym = fn s => (L.sym s) >> TOK
val noop = L.noop >> TOK

exception semantic_exp

(* semantic ops *)
fun rat (TOK(L.INT n), TOK(L.INT d)) = RAT (Coeff.mk n d)
  | rat _ = raise semantic_exp
fun pirat (TOK(L.INT n), TOK(L.INT d)) = RAT (PiCoeff.mk n d)
  | pirat _ = raise semantic_exp
fun whole_num mker n = mker (n,TOK(L.INT 1))
fun rat_term (RAT r, TOK(L.ID id)) = EXP (mk (PiCoeff.mk 0 1) [(id,r)])
  | rat_term _ = raise semantic_exp
fun const_term (RAT r) = EXP (mk r [])
  | const_term _ = raise semantic_exp
val one_pi = const_term (RAT (Coeff.mk 1 1))
fun var_term (TOK(L.ID id)) = EXP (mk_var id)
  | var_term _ = raise semantic_exp
fun plus (EXP a, EXP b) = EXP (add_expr a b)
  | plus _ = raise semantic_exp
fun minus (EXP a, EXP b) = EXP (subtr_expr a b)
  | minus _ = raise semantic_exp
fun neg (EXP a) = EXP (subtr_expr zero a)
  | neg _ = raise semantic_exp

(* the grammar, decorated with semantic operators *)
fun pi x = x|>((sym "\\" || noop)
		   --| (sym "p" || sym "P") --| (sym "i" || sym "I"))

fun coeff mker x = x|>(
	      ((num --| sym "/" -- num) >> mker)
		  || (sym "(" |-- (coeff mker) --| sym ")")
		  || num >> (whole_num mker))


(* first term *)
fun term x = x|>(
	     ((sym "-" |-- term) >> neg)
		 || (((coeff pirat) --| (sym "*" || noop) --| pi) >> const_term)
		 || (pi >> (K one_pi))
		 || (((coeff rat) --| (sym "*" || noop) -- ident) >> rat_term)
		 || ((coeff pirat) >> const_term)
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

fun parse str = let val e = case L.parse_with expr str
                             of EXP e=>e
                              | _ => raise semantic_exp
		in e
		end
    handle L.MoreTokens _ => raise parse_exp
	       | FAIL => raise parse_exp



end

val parse = LinratParser.parse

end

structure LinratExpr = DB_LinratExpr : LINRAT_EXPR
