(*datatype 'a tree = Nd of 'a * 'a tree list;

structure G = RGGraph;
structure NS = RGVertex.NSet;
val bv = Vertex.BVert ();

fun split set =
    let
	val fst = the (set|>NS.get_first)
	val rest = set|>NS.delete fst
    in (fst, rest)
    end

val g = G.empty

local
    val (v1, g) = g|>G.add_vertex bv
    val (v2, g) = g|>G.add_vertex bv
    val (v3, g) = g|>G.add_vertex bv
    val (v4, g) = g|>G.add_vertex bv
    val (v5, g) = g|>G.add_vertex bv
    val (v6, g) = g|>G.add_vertex bv
    val (v7, g) = g|>G.add_vertex bv
    val (v8, g) = g|>G.add_vertex bv
in

val (_,g) = g|>G.add_edge () v1 v2
val (_,g) = g|>G.add_edge () v1 v3
val (_,g) = g|>G.add_edge () v2 v4
val (_,g) = g|>G.add_edge () v3 v5
val (_,g) = g|>G.add_edge () v5 v1

(* a disconnected component *)
val (_,g) = g|>G.add_edge () v6 v7
val (_,g) = g|>G.add_edge () v6 v8
val (_,g) = g|>G.add_edge () v8 v7

end



fun wipe_edges g = UnitEdge.NSet.fold (G.delete_edge) (g|>G.get_enames) g

fun no_op a b = b
local
    fun dft' cmb1 cmb2 g branches prev tr =
	if (NS.is_empty branches) then (g, tr)
	else
	    let
		val (head,tail) = split branches
	    in
		if (G.has_vname g head) then
		    let
			val (g', tr') =
			    dft' cmb1 cmb2
				 (g|>G.delete_vertex head)
				 (G.adj_vnames g head)
				 head tr
		    in
			dft' cmb1 cmb2 g' tail prev (cmb1 (prev,head) tr')
		    end
		else (g, (cmb2 (prev,head) tr))
	    end
in

(* perform a depth-first traversal, folding on cmb1 for paths IN the
   spanning tree and cmb2 for cycle connections *)
fun dft2 cmb1 cmb2 g base =
    case NS.get_first (g|>G.get_vnames)
     of (SOME v) =>
	let
	    val (g',tr) = dft' cmb1 cmb2
			       (g|>G.delete_vertex v)
			       (G.adj_vnames g v)
			       v base
	in dft2 cmb1 cmb2 g' tr
	end
      | NONE => base

(* same, but only care about the spanning tree *)
fun dft cmb g base = dft2 cmb no_op g base

end


fun paths g = dft (curry op::) g []
fun traces g = dft2 no_op (curry op::) g []

fun spanning_tree g =
    dft
	(fn (s,t) => fn tr => #2(tr |> (G.add_edge () s t)))
	g (wipe_edges g)

fun spanning_tree_with_traces g =
    dft2
	(fn (s,t) => fn (tr,lst) => ((#2(tr |> (G.add_edge () s t))),lst))
	(fn (s,t) => fn (tr,lst) => (tr,(s,t)::lst))
	 g (wipe_edges g, [])

local
    fun bld_tree (p,h) [] = [Nd(p,[Nd(h,[])])]
      | bld_tree (p,h) (Nd(r,b)::ts) =
	let
	    val merge =
	     fn ts' =>
		case ts' of (Nd(r1,b1)::Nd(r2,b2)::ts'') =>
			    (if r1=r2 then Nd(r1,b1@b2)::ts''
			     else ts')
			  | _ => ts'
	in
	    merge (
	    if r=h then Nd(p,[Nd(r,b)])::ts
	    else Nd(p,[Nd(h,[])])::Nd(r,b)::ts
	    )
	end
in
fun span2 g = dft bld_tree g []
end

fun term [] = ""
  | term (s::t::ts) = "(" ^ (term [s]) ^ " X " ^ (term [t]) ^ ")"
  | term [Nd(r,[])] = r
  | term [Nd(r,ts)] = "(" ^ r ^ " o " ^ (term ts) ^ ")"
*)



(*
fun tokenise' _ [] toks = toks
  | tokenise' is_wc (c::cs) toks =
    let
	fun id [] buf = (rev buf, [])
	  | id (c::cs) buf = if is_wc c then id cs (c::buf) else (rev buf, c::cs)
    tokenise' is_wc cs
	      (if Char.isSpace c then toks else
	       (if is_wc c then
		    (fn (buf,cs) => tokenise' is_wc cs ((ID str)::toks))
			(id (c::cs) [])
		else ((SYM c)::toks)))
*)


(*structure SimpleRDP =
struct

exception Parse
datatype token = ID of string | SYM of char | INT of int | NOOP
val id_char = fn c => (Char.isAlphaNum c) orelse c = #"_"

fun tokenise is_wc str = let
    fun fl [] toks = toks
      | fl buf toks = let val s = (String.implode o rev) buf in
			  (case Int.fromString s
			    of SOME i => (INT i)
			     | NONE => (ID s))::toks
		      end
    fun tz [] buf toks = fl buf toks
      | tz (c::cs) buf toks =
	if Char.isSpace c then tz cs [] (fl buf toks)
	else (if is_wc c then tz cs (c::buf) toks
	      else tz cs [] ((SYM c)::(fl buf toks)))
in
    rev (tz (String.explode str) [] [])
end

fun parse parser str = let val (tr,rest) = Scan.catch parser (tokenise id_char str)
		       in case rest of [] => tr | _ => raise Parse
		       end

fun num ((tok as INT _)::toks) = (tok, toks)
  | num _ = Scan.fail_with (fn () => "expected: integer") ()
fun ident ((tok as ID _)::toks) = (tok, toks)
  | ident _ = Scan.fail_with (fn () => "expected: identifier") ()
fun keyword s ((tok as ID t)::toks) = if (s=t) then (tok,toks)
				      else Scan.fail_with
					       (fn()=>"expected:"^s) ()
  | keyword s _ = Scan.fail_with (fn () => "expected:identifier") ()
fun sym s ((tok as SYM t)::toks) = if (s=t) then (tok, toks)
			    else Scan.fail_with
				     (fn()=>"expected:"^(Char.toString s)) ()
  | sym _ _ = Scan.fail_with (fn () => "expected: symbol") ()

fun noop toks = (NOOP,toks)

end

structure ASTParser =
struct

structure P = SimpleRDP
type token = P.token
datatype 'a ast = OP of token * token ast list | TERM of token


fun unit x = TERM x
val num = P.num >> unit
val ident = P.ident >> unit
val sym = fn s => (P.sym s) >> unit


(* a few simple semantic ops *)
fun strip ((_,b),_) = b
fun infx ((a,TERM b),c) = OP(b,[a,c])
fun mult (a,b) = infx ((a,TERM (P.SYM #"*")),b)

(* the grammar, decorated with semantic operators *)
fun coeff x = x|>(
	      ((num -- sym #"/" -- num) >> infx)
		  || ((sym #"(" -- coeff -- sym #")") >> strip)
		  || num)
fun term x = x|>(
	     ((coeff -- sym #"*" -- ident) >> infx)
		 || ((coeff -- ident) >> mult)
		 || coeff || ident)
fun terms x = x|>(
	      ((term -- sym #"+" -- terms) >> infx)
		  || ((sym #"(" -- terms -- sym #")") >> strip)
		  || term)

val parse = P.parse terms

end*)

structure SimpleLexer =
struct

datatype token = INT of int | ID of string | SYM of string | NOOP

(* terminal definitions *)
val space = Scan.many Symbol.is_blank >> (fn _ => NOOP)
fun wrap_term t = space |-- t --| space
val num = wrap_term (Scan.many1
			 Symbol.is_digit
			 >> implode
			 >> (the o Int.fromString)
			 >> (fn x => INT x))

val ident = wrap_term (Scan.many1
			   Symbol.is_quasi_letter
			   >> implode
			   >> (fn x => ID x))
val sym = fn str => (wrap_term (Scan.this_string str >> (fn _ => SYM str)))
val noop = fn toks => (NOOP, toks)
end



structure LinratParser =
struct

structure L = SimpleLexer
datatype expr = EXP of DB_LinratExpr.T
	      | RAT of Rational.T
	      | TOK of L.token

fun unit x = TOK x
val num = L.num >> unit
val ident = L.ident >> unit
val sym = fn s => (L.sym s) >> unit
val noop = L.noop >> unit


(* semantic ops *)
val one = Rational.mk 1 1
val zero = Rational.mk 0 1

fun rat (TOK(L.INT n), TOK(L.INT d)) = RAT (Rational.mk n d)
fun whole_num n = rat (n,TOK(L.INT 1))
fun rat_term (RAT r, TOK(L.ID id)) = EXP (DB_LinratExpr.mk zero [(id,r)])
fun const_term (RAT r) = EXP (DB_LinratExpr.mk r [])
fun var_term (TOK(L.ID id)) = EXP (DB_LinratExpr.mk_var id)
fun plus (EXP a, EXP c) = EXP (DB_LinratExpr.add_expr a c)


(* the grammar, decorated with semantic operators *)
fun coeff x = x|>(
	      ((num --| sym "/" -- num) >> rat)
		  || (sym "(" |-- coeff --| sym ")")
		  || num >> whole_num)
fun term x = x|>(
	     ((coeff --| (sym "*" || noop) --| sym "Pi") >> const_term)
		 || ((coeff --| (sym "*" || noop) -- ident) >> rat_term)
		 || (ident >> var_term))
fun terms x = x|>(
	      ((term --| sym "+" -- terms) >> plus)
		  || (sym "(" |-- terms --| sym ")")
		  || term)

fun parse str = let val (EXP e,_) = terms ((Symbol.explode str)@[Symbol.eof]) in e
		end



end

