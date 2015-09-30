(*  Title:      Pure/logic.ML
    Author:     Lawrence C Paulson, Cambridge University Computer Laboratory
    Author:     Makarius

Abstract syntax operations of the Pure meta-logic.
*)

signature LOGIC =
sig
  val all_const: typ -> term
  val all: term -> term -> term
  val is_all: term -> bool
  val dest_all: term -> (string * typ) * term
  val list_all: (string * typ) list * term -> term
  val mk_equals: term * term -> term
  val dest_equals: term -> term * term
  val implies: term
  val mk_implies: term * term -> term
  val dest_implies: term -> term * term
  val list_implies: term list * term -> term
  val strip_imp_prems: term -> term list
  val strip_imp_concl: term -> term
  val strip_prems: int * term list * term -> term list * term
  val count_prems: term -> int
  val nth_prem: int * term -> term
  val true_prop: term
  val conjunction: term
  val mk_conjunction: term * term -> term
  val mk_conjunction_list: term list -> term
  val mk_conjunction_balanced: term list -> term
  val dest_conjunction: term -> term * term
  val dest_conjunction_list: term -> term list
  val dest_conjunction_balanced: int -> term -> term list
  val dest_conjunctions: term -> term list
  val strip_horn: term -> term list * term
  val mk_type: typ -> term
  val dest_type: term -> typ
  val type_map: (term -> term) -> typ -> typ
  val const_of_class: class -> string
  val class_of_const: string -> class
  val mk_of_class: typ * class -> term
  val dest_of_class: term -> typ * class
  val mk_of_sort: typ * sort -> term list
  val name_classrel: string * string -> string
  val mk_classrel: class * class -> term
  val dest_classrel: term -> class * class
  val name_arities: arity -> string list
  val name_arity: string * sort list * class -> string
  val mk_arities: arity -> term list
  val dest_arity: term -> string * sort list * class
  val unconstrainT: sort list -> term ->
    ((typ -> typ) * ((typ * class) * term) list * (typ * class) list) * term
  val protectC: term
  val protect: term -> term
  val unprotect: term -> term
  val mk_term: term -> term
  val dest_term: term -> term
  val occs: term * term -> bool
  val close_form: term -> term
  val combound: term * int * int -> term
  val rlist_abs: (string * typ) list * term -> term
  val incr_tvar_same: int -> typ Same.operation
  val incr_tvar: int -> typ -> typ
  val incr_indexes_same: string list * typ list * int -> term Same.operation
  val incr_indexes: string list * typ list * int -> term -> term
  val lift_abs: int -> term -> term -> term
  val lift_all: int -> term -> term -> term
  val strip_assums_hyp: term -> term list
  val strip_assums_concl: term -> term
  val strip_params: term -> (string * typ) list
  val has_meta_prems: term -> bool
  val flatten_params: int -> term -> term
  val list_rename_params: string list -> term -> term
  val assum_pairs: int * term -> (term * term) list
  val assum_problems: int * term -> (term -> term) * term list * term
  val varifyT_global: typ -> typ
  val unvarifyT_global: typ -> typ
  val varify_types_global: term -> term
  val unvarify_types_global: term -> term
  val varify_global: term -> term
  val unvarify_global: term -> term
  val get_goal: term -> int -> term
  val goal_params: term -> int -> term * term list
  val prems_of_goal: term -> int -> term list
  val concl_of_goal: term -> int -> term
end;

structure Logic : LOGIC =
struct


(*** Abstract syntax operations on the meta-connectives ***)

(** all **)

fun all_const T = Const ("Pure.all", (T --> propT) --> propT);

fun all v t = all_const (Term.fastype_of v) $ lambda v t;

fun is_all (Const ("Pure.all", _) $ Abs _) = true
  | is_all _ = false;

fun dest_all (Const ("Pure.all", _) $ Abs (abs as (_, T, _))) =
      let val (x, b) = Term.dest_abs abs  (*potentially slow*)
      in ((x, T), b) end
  | dest_all t = raise TERM ("dest_all", [t]);

fun list_all ([], t) = t
  | list_all ((a, T) :: vars, t) = all_const T $ Abs (a, T, list_all (vars, t));


(** equality **)

fun mk_equals (t, u) =
  let val T = Term.fastype_of t
  in Const ("Pure.eq", T --> T --> propT) $ t $ u end;

fun dest_equals (Const ("Pure.eq", _) $ t $ u) = (t, u)
  | dest_equals t = raise TERM ("dest_equals", [t]);


(** implies **)

val implies = Const ("Pure.imp", propT --> propT --> propT);

fun mk_implies (A, B) = implies $ A $ B;

fun dest_implies (Const ("Pure.imp", _) $ A $ B) = (A, B)
  | dest_implies A = raise TERM ("dest_implies", [A]);


(** nested implications **)

(* [A1,...,An], B  goes to  A1==>...An==>B  *)
fun list_implies ([], B) = B
  | list_implies (A::As, B) = implies $ A $ list_implies(As,B);

(* A1==>...An==>B  goes to  [A1,...,An], where B is not an implication *)
fun strip_imp_prems (Const("Pure.imp", _) $ A $ B) = A :: strip_imp_prems B
  | strip_imp_prems _ = [];

(* A1==>...An==>B  goes to B, where B is not an implication *)
fun strip_imp_concl (Const("Pure.imp", _) $ A $ B) = strip_imp_concl B
  | strip_imp_concl A = A : term;

(*Strip and return premises: (i, [], A1==>...Ai==>B)
    goes to   ([Ai, A(i-1),...,A1] , B)         (REVERSED)
  if  i<0 or else i too big then raises  TERM*)
fun strip_prems (0, As, B) = (As, B)
  | strip_prems (i, As, Const("Pure.imp", _) $ A $ B) =
        strip_prems (i-1, A::As, B)
  | strip_prems (_, As, A) = raise TERM("strip_prems", A::As);

(*Count premises -- quicker than (length o strip_prems) *)
fun count_prems (Const ("Pure.imp", _) $ _ $ B) = 1 + count_prems B
  | count_prems _ = 0;

(*Select Ai from A1 ==>...Ai==>B*)
fun nth_prem (1, Const ("Pure.imp", _) $ A $ _) = A
  | nth_prem (i, Const ("Pure.imp", _) $ _ $ B) = nth_prem (i - 1, B)
  | nth_prem (_, A) = raise TERM ("nth_prem", [A]);

(*strip a proof state (Horn clause):
  B1 ==> ... Bn ==> C   goes to   ([B1, ..., Bn], C)    *)
fun strip_horn A = (strip_imp_prems A, strip_imp_concl A);



(** conjunction **)

val true_prop = all_const propT $ Abs ("dummy", propT, mk_implies (Bound 0, Bound 0));
val conjunction = Const ("Pure.conjunction", propT --> propT --> propT);


(*A &&& B*)
fun mk_conjunction (A, B) = conjunction $ A $ B;

(*A &&& B &&& C -- improper*)
fun mk_conjunction_list [] = true_prop
  | mk_conjunction_list ts = foldr1 mk_conjunction ts;

(*(A &&& B) &&& (C &&& D) -- balanced*)
fun mk_conjunction_balanced [] = true_prop
  | mk_conjunction_balanced ts = Balanced_Tree.make mk_conjunction ts;


(*A &&& B*)
fun dest_conjunction (Const ("Pure.conjunction", _) $ A $ B) = (A, B)
  | dest_conjunction t = raise TERM ("dest_conjunction", [t]);

(*A &&& B &&& C -- improper*)
fun dest_conjunction_list t =
  (case try dest_conjunction t of
    NONE => [t]
  | SOME (A, B) => A :: dest_conjunction_list B);

(*(A &&& B) &&& (C &&& D) -- balanced*)
fun dest_conjunction_balanced 0 _ = []
  | dest_conjunction_balanced n t = Balanced_Tree.dest dest_conjunction n t;

(*((A &&& B) &&& C) &&& D &&& E -- flat*)
fun dest_conjunctions t =
  (case try dest_conjunction t of
    NONE => [t]
  | SOME (A, B) => dest_conjunctions A @ dest_conjunctions B);



(** types as terms **)

fun mk_type ty = Const ("Pure.type", Term.itselfT ty);

fun dest_type (Const ("Pure.type", Type ("itself", [ty]))) = ty
  | dest_type t = raise TERM ("dest_type", [t]);

fun type_map f = dest_type o f o mk_type;



(** type classes **)

(* const names *)

val classN = "_class";

val const_of_class = suffix classN;

fun class_of_const c = unsuffix classN c
  handle Fail _ => raise TERM ("class_of_const: bad name " ^ quote c, []);


(* class/sort membership *)

fun mk_of_class (ty, c) =
  Const (const_of_class c, Term.itselfT ty --> propT) $ mk_type ty;

fun dest_of_class (Const (c_class, _) $ ty) = (dest_type ty, class_of_const c_class)
  | dest_of_class t = raise TERM ("dest_of_class", [t]);

fun mk_of_sort (ty, S) = map (fn c => mk_of_class (ty, c)) S;


(* class relations *)

fun name_classrel (c1, c2) =
  Long_Name.base_name c1 ^ "_" ^ Long_Name.base_name c2;

fun mk_classrel (c1, c2) = mk_of_class (Term.aT [c1], c2);

fun dest_classrel tm =
  (case dest_of_class tm of
    (TVar (_, [c1]), c2) => (c1, c2)
  | _ => raise TERM ("dest_classrel", [tm]));


(* type arities *)

fun name_arities (t, _, S) =
  let val b = Long_Name.base_name t
  in S |> map (fn c => Long_Name.base_name c ^ "_" ^ b) end;

fun name_arity (t, dom, c) = hd (name_arities (t, dom, [c]));

fun mk_arities (t, Ss, S) =
  let val T = Type (t, ListPair.map TFree (Name.invent Name.context Name.aT (length Ss), Ss))
  in map (fn c => mk_of_class (T, c)) S end;

fun dest_arity tm =
  let
    fun err () = raise TERM ("dest_arity", [tm]);

    val (ty, c) = dest_of_class tm;
    val (t, tvars) =
      (case ty of
        Type (t, tys) => (t, map dest_TVar tys handle TYPE _ => err ())
      | _ => err ());
    val Ss =
      if has_duplicates (eq_fst (op =)) tvars then err ()
      else map snd tvars;
  in (t, Ss, c) end;


(* internalized sort constraints *)

fun unconstrainT shyps prop =
  let
    val present = rev ((fold_types o fold_atyps_sorts) (insert (eq_fst op =)) prop []);
    val extra = fold (Sorts.remove_sort o #2) present shyps;

    val n = length present;
    val (names1, names2) = Name.invent Name.context Name.aT (n + length extra) |> chop n;

    val present_map =
      map2 (fn (T, S) => fn a => (T, TVar ((a, 0), S))) present names1;
    val constraints_map =
      map2 (fn (_, S) => fn a => (S, TVar ((a, 0), S))) present names1 @
      map2 (fn S => fn a => (S, TVar ((a, 0), S))) extra names2;

    fun atyp_map T =
      (case AList.lookup (op =) present_map T of
        SOME U => U
      | NONE =>
          (case AList.lookup (op =) constraints_map (Type.sort_of_atyp T) of
            SOME U => U
          | NONE => raise TYPE ("Dangling type variable", [T], [])));

    val constraints =
      maps (fn (_, T as TVar (ai, S)) =>
        map (fn c => ((T, c), mk_of_class (TVar (ai, []), c))) S)
        constraints_map;

    val outer_constraints =
      maps (fn (T, S) => map (pair T) S)
        (present @ map (fn S => (TFree ("'dummy", S), S)) extra);

    val prop' =
      prop
      |> (Term.map_types o Term.map_atyps) (Type.strip_sorts o atyp_map)
      |> curry list_implies (map snd constraints);
  in ((atyp_map, constraints, outer_constraints), prop') end;



(** protected propositions and embedded terms **)

val protectC = Const ("Pure.prop", propT --> propT);
fun protect t = protectC $ t;

fun unprotect (Const ("Pure.prop", _) $ t) = t
  | unprotect t = raise TERM ("unprotect", [t]);


fun mk_term t = Const ("Pure.term", Term.fastype_of t --> propT) $ t;

fun dest_term (Const ("Pure.term", _) $ t) = t
  | dest_term t = raise TERM ("dest_term", [t]);



(*** Low-level term operations ***)

(*Does t occur in u?  Or is alpha-convertible to u?
  The term t must contain no loose bound variables*)
fun occs (t, u) = exists_subterm (fn s => t aconv s) u;

(*Close up a formula over all free variables by quantification*)
fun close_form A = fold (all o Free) (Term.add_frees A []) A;



(*** Specialized operations for resolution... ***)

(*computes t(Bound(n+k-1),...,Bound(n))  *)
fun combound (t, n, k) =
    if  k>0  then  combound (t,n+1,k-1) $ (Bound n)  else  t;

(* ([xn,...,x1], t)   ======>   (x1,...,xn)t *)
fun rlist_abs ([], body) = body
  | rlist_abs ((a,T)::pairs, body) = rlist_abs(pairs, Abs(a, T, body));

fun incr_tvar_same 0 = Same.same
  | incr_tvar_same k = Term_Subst.map_atypsT_same
      (fn TVar ((a, i), S) => TVar ((a, i + k), S)
        | _ => raise Same.SAME);

fun incr_tvar k T = incr_tvar_same k T handle Same.SAME => T;

(*For all variables in the term, increment indexnames and lift over the Us
    result is ?Gidx(B.(lev+n-1),...,B.lev) where lev is abstraction level *)
fun incr_indexes_same ([], [], 0) = Same.same
  | incr_indexes_same (fixed, Ts, k) =
      let
        val n = length Ts;
        val incrT = incr_tvar_same k;

        fun incr lev (Var ((x, i), T)) =
              combound (Var ((x, i + k), Ts ---> Same.commit incrT T), lev, n)
          | incr lev (Free (x, T)) =
              if member (op =) fixed x then
                combound (Free (x, Ts ---> Same.commit incrT T), lev, n)
              else Free (x, incrT T)
          | incr lev (Abs (x, T, body)) =
              (Abs (x, incrT T, incr (lev + 1) body handle Same.SAME => body)
                handle Same.SAME => Abs (x, T, incr (lev + 1) body))
          | incr lev (t $ u) =
              (incr lev t $ (incr lev u handle Same.SAME => u)
                handle Same.SAME => t $ incr lev u)
          | incr _ (Const (c, T)) = Const (c, incrT T)
          | incr _ (Bound _) = raise Same.SAME;
      in incr 0 end;

fun incr_indexes arg t = incr_indexes_same arg t handle Same.SAME => t;


(* Lifting functions from subgoal and increment:
    lift_abs operates on terms
    lift_all operates on propositions *)

fun lift_abs inc =
  let
    fun lift Ts (Const ("Pure.imp", _) $ _ $ B) t = lift Ts B t
      | lift Ts (Const ("Pure.all", _) $ Abs (a, T, B)) t = Abs (a, T, lift (T :: Ts) B t)
      | lift Ts _ t = incr_indexes ([], rev Ts, inc) t;
  in lift [] end;

fun lift_all inc =
  let
    fun lift Ts ((c as Const ("Pure.imp", _)) $ A $ B) t = c $ A $ lift Ts B t
      | lift Ts ((c as Const ("Pure.all", _)) $ Abs (a, T, B)) t = c $ Abs (a, T, lift (T :: Ts) B t)
      | lift Ts _ t = incr_indexes ([], rev Ts, inc) t;
  in lift [] end;

(*Strips assumptions in goal, yielding list of hypotheses.   *)
fun strip_assums_hyp B =
  let
    fun strip Hs (Const ("Pure.imp", _) $ H $ B) = strip (H :: Hs) B
      | strip Hs (Const ("Pure.all", _) $ Abs (a, T, t)) =
          strip (map (incr_boundvars 1) Hs) t
      | strip Hs B = rev Hs
  in strip [] B end;

(*Strips assumptions in goal, yielding conclusion.   *)
fun strip_assums_concl (Const("Pure.imp", _) $ H $ B) = strip_assums_concl B
  | strip_assums_concl (Const("Pure.all",_)$Abs(a,T,t)) = strip_assums_concl t
  | strip_assums_concl B = B;

(*Make a list of all the parameters in a subgoal, even if nested*)
fun strip_params (Const("Pure.imp", _) $ H $ B) = strip_params B
  | strip_params (Const("Pure.all",_)$Abs(a,T,t)) = (a,T) :: strip_params t
  | strip_params B = [];

(*test for nested meta connectives in prems*)
val has_meta_prems =
  let
    fun is_meta (Const ("Pure.eq", _) $ _ $ _) = true
      | is_meta (Const ("Pure.imp", _) $ _ $ _) = true
      | is_meta (Const ("Pure.all", _) $ _) = true
      | is_meta _ = false;
    fun ex_meta (Const ("Pure.imp", _) $ A $ B) = is_meta A orelse ex_meta B
      | ex_meta (Const ("Pure.all", _) $ Abs (_, _, B)) = ex_meta B
      | ex_meta _ = false;
  in ex_meta end;

(*Removes the parameters from a subgoal and renumber bvars in hypotheses,
    where j is the total number of parameters (precomputed)
  If n>0 then deletes assumption n. *)
fun remove_params j n A =
    if j=0 andalso n<=0 then A  (*nothing left to do...*)
    else case A of
        Const("Pure.imp", _) $ H $ B =>
          if n=1 then                           (remove_params j (n-1) B)
          else implies $ (incr_boundvars j H) $ (remove_params j (n-1) B)
      | Const("Pure.all",_)$Abs(a,T,t) => remove_params (j-1) n t
      | _ => if n>0 then raise TERM("remove_params", [A])
             else A;

(*Move all parameters to the front of the subgoal, renaming them apart;
  if n>0 then deletes assumption n. *)
fun flatten_params n A =
    let val params = strip_params A;
        val vars = ListPair.zip (Name.variant_list [] (map #1 params),
                                 map #2 params)
    in list_all (vars, remove_params (length vars) n A) end;

(*Makes parameters in a goal have the names supplied by the list cs.*)
fun list_rename_params cs (Const ("Pure.imp", _) $ A $ B) =
      implies $ A $ list_rename_params cs B
  | list_rename_params (c :: cs) ((a as Const ("Pure.all", _)) $ Abs (_, T, t)) =
      a $ Abs (c, T, list_rename_params cs t)
  | list_rename_params cs B = B;



(*** Treatment of "assume", "erule", etc. ***)

(*Strips assumptions in goal yielding
   HS = [Hn,...,H1],   params = [xm,...,x1], and B,
  where x1...xm are the parameters. This version (21.1.2005) REQUIRES
  the the parameters to be flattened, but it allows erule to work on
  assumptions of the form !!x. phi. Any !! after the outermost string
  will be regarded as belonging to the conclusion, and left untouched.
  Used ONLY by assum_pairs.
      Unless nasms<0, it can terminate the recursion early; that allows
  erule to work on assumptions of the form P==>Q.*)
fun strip_assums_imp (0, Hs, B) = (Hs, B)  (*recursion terminated by nasms*)
  | strip_assums_imp (nasms, Hs, Const("Pure.imp", _) $ H $ B) =
      strip_assums_imp (nasms-1, H::Hs, B)
  | strip_assums_imp (_, Hs, B) = (Hs, B); (*recursion terminated by B*)

(*Strips OUTER parameters only.*)
fun strip_assums_all (params, Const("Pure.all",_)$Abs(a,T,t)) =
      strip_assums_all ((a,T)::params, t)
  | strip_assums_all (params, B) = (params, B);

(*Produces disagreement pairs, one for each assumption proof, in order.
  A is the first premise of the lifted rule, and thus has the form
    H1 ==> ... Hk ==> B   and the pairs are (H1,B),...,(Hk,B).
  nasms is the number of assumptions in the original subgoal, needed when B
    has the form B1 ==> B2: it stops B1 from being taken as an assumption. *)
fun assum_pairs(nasms,A) =
  let val (params, A') = strip_assums_all ([],A)
      val (Hs,B) = strip_assums_imp (nasms,[],A')
      fun abspar t = rlist_abs(params, t)
      val D = abspar B
      fun pairrev ([], pairs) = pairs
        | pairrev (H::Hs, pairs) = pairrev(Hs,  (abspar H, D) :: pairs)
  in  pairrev (Hs,[])
  end;

fun assum_problems (nasms, A) =
  let
    val (params, A') = strip_assums_all ([], A)
    val (Hs, B) = strip_assums_imp (nasms, [], A')
    fun abspar t = rlist_abs (params, t)
  in (abspar, rev Hs, B) end;


(* global schematic variables *)

fun bad_schematic xi = "Illegal schematic variable: " ^ quote (Term.string_of_vname xi);
fun bad_fixed x = "Illegal fixed variable: " ^ quote x;

fun varifyT_global_same ty = ty
  |> Term_Subst.map_atypsT_same
    (fn TFree (a, S) => TVar ((a, 0), S)
      | TVar (ai, _) => raise TYPE (bad_schematic ai, [ty], []));

fun unvarifyT_global_same ty = ty
  |> Term_Subst.map_atypsT_same
    (fn TVar ((a, 0), S) => TFree (a, S)
      | TVar (ai, _) => raise TYPE (bad_schematic ai, [ty], [])
      | TFree (a, _) => raise TYPE (bad_fixed a, [ty], []));

val varifyT_global = Same.commit varifyT_global_same;
val unvarifyT_global = Same.commit unvarifyT_global_same;

fun varify_types_global tm = tm
  |> Same.commit (Term_Subst.map_types_same varifyT_global_same)
  handle TYPE (msg, _, _) => raise TERM (msg, [tm]);

fun unvarify_types_global tm = tm
  |> Same.commit (Term_Subst.map_types_same unvarifyT_global_same)
  handle TYPE (msg, _, _) => raise TERM (msg, [tm]);

fun varify_global tm = tm
  |> Same.commit (Term_Subst.map_aterms_same
    (fn Free (x, T) => Var ((x, 0), T)
      | Var (xi, _) => raise TERM (bad_schematic xi, [tm])
      | _ => raise Same.SAME))
  |> varify_types_global;

fun unvarify_global tm = tm
  |> Same.commit (Term_Subst.map_aterms_same
    (fn Var ((x, 0), T) => Free (x, T)
      | Var (xi, _) => raise TERM (bad_schematic xi, [tm])
      | Free (x, _) => raise TERM (bad_fixed x, [tm])
      | _ => raise Same.SAME))
  |> unvarify_types_global;


(* goal states *)

fun get_goal st i =
  nth_prem (i, st) handle TERM _ =>
    error ("Subgoal number " ^ string_of_int i ^ " out of range (a total of " ^
      string_of_int (count_prems st)  ^ " subgoals)");

(*reverses parameters for substitution*)
fun goal_params st i =
  let val gi = get_goal st i
      val rfrees = map Free (Term.rename_wrt_term gi (strip_params gi))
  in (gi, rfrees) end;

fun concl_of_goal st i =
  let val (gi, rfrees) = goal_params st i
      val B = strip_assums_concl gi
  in subst_bounds (rfrees, B) end;

fun prems_of_goal st i =
  let val (gi, rfrees) = goal_params st i
      val As = strip_assums_hyp gi
  in map (fn A => subst_bounds (rfrees, A)) As end;

end;
