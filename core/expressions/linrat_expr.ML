(* linear combinations with rational coeffs (and pi) *)
signature LINRAT_EXPR
= sig
  type T (* expressions *)
  val ord : T * T -> General.order
  val eq : T -> T -> bool
  structure Coeff : RATIONAL
  exception parse_exp
  (* Parse a string into a linear rational expression *)
  (* any alphabetic text (and \) is considered a variable name *)
  (* except \pi *)
  (* constants and coeffs of \pi are taken as coeffs of \pi, *)
  (* and calculated modulo 2 *)
  val parse : string -> T
  val zero : T
  val is_zero : T -> bool
  val one : T
  val mk_var : X.name -> T
  val mk : Coeff.T -> (X.name * Coeff.T) list -> T
  val mk_const : Coeff.T -> T
  val subtr_expr : T -> T -> T
  val add_expr : T -> T -> T
  val scale_expr : Coeff.T -> T -> T

  (* For SOME x, return the coefficient for x. For NONE, return the constant
   * term. *)
  val coeff_for_var : T -> X.name option -> Coeff.T

  val free_vars : T -> X.NSet.T

  val pretty : T -> Pretty.T
  val pretty_math : AlgFormat.format -> T -> Pretty.T
  val print : T -> unit
end

structure LinratExpr : LINRAT_EXPR =
struct

  (* Variable names in expressions *)
  exception parse_exp
  structure Coeff = Rational

  type T = Coeff.T * (Coeff.T XTab.T) 
  (* a constant coefficient, plus a table of coeffs for variables  *)

  (* =====================================================================*)


  fun kv_eq ((k1,v1),(k2,v2)) =
      X.name_eq (k1, k2) andalso Coeff.eq v1 v2

  (* don't use basis library version because it doesn't raise exception
     on unequal length lists *)
  fun forall_pair _ ([],[]) = true
    | forall_pair eq (h1::t1,h2::t2) = eq (h1,h2) andalso forall_pair eq (t1,t2)
    | forall_pair _ _ = raise ListPair.UnequalLengths

  fun eq (x,xT) (y,yT) =
      if Coeff.eq x y then
        forall_pair kv_eq (XTab.list_of xT, XTab.list_of yT)
          handle ListPair.UnequalLengths => false
      else false

  (* =====================================================================*)

  fun kv_order ((k1,v1),(k2,v2)) =
      if  X.name_ord(k1,k2) = General.EQUAL
      then
        Coeff.ord(v1,v2)
      else X.name_ord(k1,k2)

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
            val xs = XTab.list_of xT
            val ys = XTab.list_of yT
        in
            kv_list_ord(xs,ys)
        end

  (* various mk functions ============================================ *)
  val empty = XTab.empty : Coeff.T XTab.T


  val zero = (Coeff.zero, empty)
  fun is_zero (cpi,vars) =
      (Coeff.is_zero cpi andalso XTab.is_empty vars)

  val one = (Coeff.one, empty)

  fun mk const kv_list = (const, fold XTab.add kv_list empty)

  fun mk_var nm = mk Coeff.zero [(nm, Coeff.mk (1, 1))]

  fun coeff_for_var (_,tab) (SOME x) =
        (case XTab.get_opt tab x of SOME r => r | NONE => Rational.zero)
    | coeff_for_var (k,_) NONE = k

  fun free_vars (_,tab) = XTab.get_dom_set tab

  (* reduce exprs to a normal by removing variables whose Coeff = 0 ======*)
  fun reduce_expr (p, t) =
      let fun red (k,v) tab =
                if  Coeff.is_zero v
                then XTab.delete k tab
                else tab
      in
        (p, XTab.fold red t t)
      end

  (* multiplication of an expression by a scalar ===================*)
  fun scale_expr s (e_pi, e_tab) =
      (Coeff.mult s e_pi, XTab.map_all (Coeff.mult s) e_tab)

  fun mk_const k = scale_expr k one

  (* arithmetic addition for expressions ========================*)
  fun add_expr (const1,tab1) (const2,tab2) =
      let
        fun f (k,v) t =
              if XTab.dom_contains t k
              then XTab.map_entry (Coeff.add v) k t
              else XTab.set (k, v) t
        val newtab = XTab.fold f tab2 tab1
      in reduce_expr (Coeff.add const1 const2, newtab) end

  (* arithmetic subtraction for expressions ========================*)
  fun subtr_expr e1 e2 =
      add_expr e1 (scale_expr (Coeff.mk (~1, 1)) e2)


  (* =====================================================================*)

  (*
  fun pretty (pi, tab) =
      let fun pretty1 (k,v) =
          if (Coeff.is_one v) then Pretty.str k
          else Pretty.block [Coeff.pretty v, Pretty.str (" "^k)]
      val ppi  = if (Coeff.is_zero pi) then [] else [pretty1 ("\\pi", pi)]
      val plist = XTab.fold (cons o pretty1) tab ppi
      in
      case plist of [] => Pretty.str "0"
              | _ => (Pretty.block o Pretty.separate " +" o rev) plist
      end
  *)

  fun pretty1 (k,v) =
      if Coeff.is_one (Coeff.subtr Coeff.zero v) then
        Pretty.block [Pretty.str "-", 	X.pretty_name k]
      else if Coeff.is_one v then X.pretty_name k
      else Pretty.block [Coeff.pretty v, Pretty.str " ", X.pretty_name k]

  fun pretty_list [] = [Pretty.str "0"]
    | pretty_list [x] = [pretty1 x]
    | pretty_list (x::(xs as ((k,v)::ys))) =
      (pretty1 x)::
      (if Coeff.ord (v, Coeff.zero) = LESS
       then Pretty.str " - " :: pretty_list ((k,Coeff.subtr Coeff.zero v)::ys)
       else Pretty.str " + " :: pretty_list xs)

  fun pretty' pistr (const, tab) =
      let val tlist = rev (XTab.list_of tab)
      in (Pretty.block o pretty_list)
         (if Coeff.is_zero const then tlist
          else (pistr, const)::tlist)
      end


  fun pretty_math fmt = case fmt
                 of AlgFormat.MATHEMATICA => pretty' (X.mk "Pi")
              | AlgFormat.LATEX => pretty' (X.mk "\\pi")
              | AlgFormat.PLAIN => pretty' (X.mk "pi")
              | _ => raise AlgFormat.not_supported_exp

  val pretty = pretty_math AlgFormat.LATEX

  val print = Pretty.writeln o pretty

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
    fun rat (TOK(L.INT n), TOK(L.INT d)) = RAT (Coeff.mk (n, d))
      | rat _ = raise semantic_exp
    (*fun pirat (TOK(L.INT n), TOK(L.INT d)) = RAT (PiCoeff.mk (n, d))
      | pirat _ = raise semantic_exp*)
    fun whole_num mker n = mker (n,TOK(L.INT 1))
    fun rat_term (RAT r, TOK(L.ID id)) = EXP (mk (Coeff.mk (0, 1)) [(X.mk id,r)])
      | rat_term _ = raise semantic_exp
    fun const_term (RAT r) = EXP (mk r [])
      | const_term _ = raise semantic_exp
    (*val one_pi = const_term (RAT (Coeff.mk (1, 1)))*)
    fun var_term (TOK(L.ID id)) = EXP (mk_var (X.mk id))
      | var_term _ = raise semantic_exp
    fun plus (EXP a, EXP b) = EXP (add_expr a b)
      | plus _ = raise semantic_exp
    fun neg (EXP a) = EXP (subtr_expr zero a)
      | neg _ = raise semantic_exp

    (* the grammar, decorated with semantic operators *)
    (*fun pi x = x|>((sym "\\" || noop)
               --| (sym "p" || sym "P") --| (sym "i" || sym "I"))*)

    fun coeff mker x = x|>(
              ((num --| sym "/" -- num) >> mker)
              || (sym "(" |-- (coeff mker) --| sym ")")
              || num >> (whole_num mker))

    fun frac x = let
      val maybe_num = (num --| (sym "*" || noop))
                      || (fn x => (TOK(L.INT 1),x))
      fun mkrat_term ((n,i),d) = rat_term (rat (n,d),i)
    in
      x |> (   ((maybe_num -- ident --| sym "/" -- num) >> mkrat_term)
            || (sym "(" |-- frac --| sym ")"))
    end


    (* first term *)
    fun term x = x|>(
             ((sym "-" |-- term) >> neg)
             || frac
             || (((coeff rat) --| (sym "*" || noop) -- ident) >> rat_term)
             || ((coeff rat) >> const_term)
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

    fun parse str =
      (case Scan.catch (L.parse_with expr) str
         of EXP e => e
          | _     => raise semantic_exp)
      handle L.MoreTokens _ => raise parse_exp
           | Fail _         => raise parse_exp
           | semantic_exp   => raise parse_exp

  end

  fun parse "" = zero
     |  parse s = LinratParser.parse s

end

