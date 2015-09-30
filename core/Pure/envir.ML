(*  Title:      Pure/envir.ML
    Author:     Lawrence C Paulson, Cambridge University Computer Laboratory

Free-form environments.  The type of a term variable / sort of a type variable is
part of its name.  The lookup function must apply type substitutions,
since they may change the identity of a variable.
*)

signature ENVIR =
sig
  type tenv = (typ * term) Vartab.table
  datatype env = Envir of {maxidx: int, tenv: tenv, tyenv: Type.tyenv}
  val maxidx_of: env -> int
  val term_env: env -> tenv
  val type_env: env -> Type.tyenv
  val is_empty: env -> bool
  val empty: int -> env
  val merge: env * env -> env
  val insert_sorts: env -> sort list -> sort list
  val genvars: string -> env * typ list -> env * term list
  val genvar: string -> env * typ -> env * term
  val lookup1: tenv -> indexname * typ -> term option
  val lookup: env -> indexname * typ -> term option
  val update: (indexname * typ) * term -> env -> env
  val above: env -> int -> bool
  val vupdate: (indexname * typ) * term -> env -> env
  val norm_type_same: Type.tyenv -> typ Same.operation
  val norm_types_same: Type.tyenv -> typ list Same.operation
  val norm_type: Type.tyenv -> typ -> typ
  val norm_term_same: env -> term Same.operation
  val norm_term: env -> term -> term
  val beta_norm: term -> term
  val head_norm: env -> term -> term
  val eta_long: typ list -> term -> term
  val eta_contract: term -> term
  val beta_eta_contract: term -> term
  val aeconv: term * term -> bool
  val body_type: env -> typ -> typ
  val binder_types: env -> typ -> typ list
  val strip_type: env -> typ -> typ list * typ
  val fastype: env -> typ list -> term -> typ
  val subst_type_same: Type.tyenv -> typ Same.operation
  val subst_term_types_same: Type.tyenv -> term Same.operation
  val subst_term_same: Type.tyenv * tenv -> term Same.operation
  val subst_type: Type.tyenv -> typ -> typ
  val subst_term_types: Type.tyenv -> term -> term
  val subst_term: Type.tyenv * tenv -> term -> term
  val expand_atom: typ -> typ * term -> term
  val expand_term: (term -> (typ * term) option) -> term -> term
  val expand_term_frees: ((string * typ) * term) list -> term -> term
end;

structure Envir: ENVIR =
struct

(** datatype env **)

(*Updating can destroy environment in 2 ways!
   (1) variables out of range
   (2) circular assignments
*)

type tenv = (typ * term) Vartab.table;

datatype env = Envir of
 {maxidx: int,          (*upper bound of maximum index of vars*)
  tenv: tenv,           (*assignments to Vars*)
  tyenv: Type.tyenv};   (*assignments to TVars*)

fun make_env (maxidx, tenv, tyenv) =
  Envir {maxidx = maxidx, tenv = tenv, tyenv = tyenv};

fun maxidx_of (Envir {maxidx, ...}) = maxidx;
fun term_env (Envir {tenv, ...}) = tenv;
fun type_env (Envir {tyenv, ...}) = tyenv;

fun is_empty env =
  Vartab.is_empty (term_env env) andalso
  Vartab.is_empty (type_env env);


(* build env *)

fun empty maxidx = make_env (maxidx, Vartab.empty, Vartab.empty);

fun merge
   (Envir {maxidx = maxidx1, tenv = tenv1, tyenv = tyenv1},
    Envir {maxidx = maxidx2, tenv = tenv2, tyenv = tyenv2}) =
  make_env (Int.max (maxidx1, maxidx2),
    Vartab.merge (op =) (tenv1, tenv2),
    Vartab.merge (op =) (tyenv1, tyenv2));


(*NB: type unification may invent new sorts*)  (* FIXME tenv!? *)
val insert_sorts = Vartab.fold (fn (_, (_, T)) => Sorts.insert_typ T) o type_env;

(*Generate a list of distinct variables.
  Increments index to make them distinct from ALL present variables. *)
fun genvars name (Envir {maxidx, tenv, tyenv}, Ts) : env * term list =
  let
    fun genvs (_, [] : typ list) : term list = []
      | genvs (n, [T]) = [Var ((name, maxidx + 1), T)]
      | genvs (n, T :: Ts) =
          Var ((name ^ radixstring (26, "a" , n), maxidx + 1), T)
            :: genvs (n + 1, Ts);
  in (Envir {maxidx = maxidx + 1, tenv = tenv, tyenv = tyenv}, genvs (0, Ts)) end;

(*Generate a variable.*)
fun genvar name (env, T) : env * term =
  let val (env', [v]) = genvars name (env, [T])
  in (env', v) end;

fun var_clash xi T T' =
  raise TYPE ("Variable has two distinct types", [], [Var (xi, T'), Var (xi, T)]);

fun lookup_check check tenv (xi, T) =
  (case Vartab.lookup tenv xi of
    NONE => NONE
  | SOME (U, t) => if check (T, U) then SOME t else var_clash xi T U);

(*When dealing with environments produced by matching instead
  of unification, there is no need to chase assigned TVars.
  In this case, we can simply ignore the type substitution
  and use = instead of eq_type.*)
fun lookup1 tenv = lookup_check (op =) tenv;

fun lookup (Envir {tenv, tyenv, ...}) = lookup_check (Type.unified tyenv) tenv;

fun update ((xi, T), t) (Envir {maxidx, tenv, tyenv}) =
  Envir {maxidx = maxidx, tenv = Vartab.update_new (xi, (T, t)) tenv, tyenv = tyenv};

(*Determine if the least index updated exceeds lim*)
fun above (Envir {tenv, tyenv, ...}) lim =
  (case Vartab.min tenv of SOME ((_, i), _) => i > lim | NONE => true) andalso
  (case Vartab.min tyenv of SOME ((_, i), _) => i > lim | NONE => true);

(*Update, checking Var-Var assignments: try to suppress higher indexes*)
fun vupdate (aU as (a, U), t) (env as Envir {tyenv, ...}) =
  (case t of
    Var (nT as (name', T)) =>
      if a = name' then env     (*cycle!*)
      else if Term_Ord.indexname_ord (a, name') = LESS then
        (case lookup env nT of  (*if already assigned, chase*)
          NONE => update (nT, Var (a, T)) env
        | SOME u => vupdate (aU, u) env)
      else update (aU, t) env
  | _ => update (aU, t) env);



(** beta normalization wrt. environment **)

(*Chases variables in env.  Does not exploit sharing of variable bindings
  Does not check types, so could loop.*)

local

fun norm_type0 tyenv : typ Same.operation =
  let
    fun norm (Type (a, Ts)) = Type (a, Same.map norm Ts)
      | norm (TFree _) = raise Same.SAME
      | norm (TVar v) =
          (case Type.lookup tyenv v of
            SOME U => Same.commit norm U
          | NONE => raise Same.SAME);
  in norm end;

fun norm_term1 tenv : term Same.operation =
  let
    fun norm (Var v) =
          (case lookup1 tenv v of
            SOME u => Same.commit norm u
          | NONE => raise Same.SAME)
      | norm (Abs (a, T, body)) = Abs (a, T, norm body)
      | norm (Abs (_, _, body) $ t) = Same.commit norm (subst_bound (t, body))
      | norm (f $ t) =
          ((case norm f of
             Abs (_, _, body) => Same.commit norm (subst_bound (t, body))
           | nf => nf $ Same.commit norm t)
          handle Same.SAME => f $ norm t)
      | norm _ = raise Same.SAME;
  in norm end;

fun norm_term2 (envir as Envir {tenv, tyenv, ...}) : term Same.operation =
  let
    val normT = norm_type0 tyenv;
    fun norm (Const (a, T)) = Const (a, normT T)
      | norm (Free (a, T)) = Free (a, normT T)
      | norm (Var (xi, T)) =
          (case lookup envir (xi, T) of
            SOME u => Same.commit norm u
          | NONE => Var (xi, normT T))
      | norm (Abs (a, T, body)) =
          (Abs (a, normT T, Same.commit norm body)
            handle Same.SAME => Abs (a, T, norm body))
      | norm (Abs (_, _, body) $ t) = Same.commit norm (subst_bound (t, body))
      | norm (f $ t) =
          ((case norm f of
             Abs (_, _, body) => Same.commit norm (subst_bound (t, body))
           | nf => nf $ Same.commit norm t)
          handle Same.SAME => f $ norm t)
      | norm _ = raise Same.SAME;
  in norm end;

in

fun norm_type_same tyenv T =
  if Vartab.is_empty tyenv then raise Same.SAME
  else norm_type0 tyenv T;

fun norm_types_same tyenv Ts =
  if Vartab.is_empty tyenv then raise Same.SAME
  else Same.map (norm_type0 tyenv) Ts;

fun norm_type tyenv T = norm_type_same tyenv T handle Same.SAME => T;

fun norm_term_same (envir as Envir {tenv, tyenv, ...}) =
  if Vartab.is_empty tyenv then norm_term1 tenv
  else norm_term2 envir;

fun norm_term envir t = norm_term_same envir t handle Same.SAME => t;
fun beta_norm t = if Term.has_abs t then norm_term (empty 0) t else t;

end;


(* head normal form for unification *)

fun head_norm env =
  let
    fun norm (Var v) =
        (case lookup env v of
          SOME u => head_norm env u
        | NONE => raise Same.SAME)
      | norm (Abs (a, T, body)) = Abs (a, T, norm body)
      | norm (Abs (_, _, body) $ t) = Same.commit norm (subst_bound (t, body))
      | norm (f $ t) =
          (case norm f of
            Abs (_, _, body) => Same.commit norm (subst_bound (t, body))
          | nf => nf $ t)
      | norm _ = raise Same.SAME;
  in Same.commit norm end;


(* eta-long beta-normal form *)

fun eta_long Ts (Abs (s, T, t)) = Abs (s, T, eta_long (T :: Ts) t)
  | eta_long Ts t =
      (case strip_comb t of
        (Abs _, _) => eta_long Ts (beta_norm t)
      | (u, ts) =>
          let
            val Us = binder_types (fastype_of1 (Ts, t));
            val i = length Us;
            val long = eta_long (rev Us @ Ts);
          in
            fold_rev (Term.abs o pair "x") Us
              (list_comb (incr_boundvars i u,
                map (long o incr_boundvars i) ts @ map (long o Bound) (i - 1 downto 0)))
          end);


(* full eta contraction *)

local

fun decr lev (Bound i) = if i >= lev then Bound (i - 1) else raise Same.SAME
  | decr lev (Abs (a, T, body)) = Abs (a, T, decr (lev + 1) body)
  | decr lev (t $ u) = (decr lev t $ decrh lev u handle Same.SAME => t $ decr lev u)
  | decr _ _ = raise Same.SAME
and decrh lev t = (decr lev t handle Same.SAME => t);

fun eta (Abs (a, T, body)) =
    ((case eta body of
        body' as (f $ Bound 0) =>
          if Term.is_dependent f then Abs (a, T, body')
          else decrh 0 f
     | body' => Abs (a, T, body')) handle Same.SAME =>
        (case body of
          f $ Bound 0 =>
            if Term.is_dependent f then raise Same.SAME
            else decrh 0 f
        | _ => raise Same.SAME))
  | eta (t $ u) = (eta t $ Same.commit eta u handle Same.SAME => t $ eta u)
  | eta _ = raise Same.SAME;

in

fun eta_contract t =
  if Term.has_abs t then Same.commit eta t else t;

end;

val beta_eta_contract = eta_contract o beta_norm;

fun aeconv (t, u) = t aconv u orelse eta_contract t aconv eta_contract u;


fun body_type env (Type ("fun", [_, T])) = body_type env T
  | body_type env (T as TVar v) =
      (case Type.lookup (type_env env) v of
        NONE => T
      | SOME T' => body_type env T')
  | body_type _ T = T;

fun binder_types env (Type ("fun", [T, U])) = T :: binder_types env U
  | binder_types env (TVar v) =
      (case Type.lookup (type_env env) v of
        NONE => []
      | SOME T' => binder_types env T')
  | binder_types _ _ = [];

fun strip_type env T = (binder_types env T, body_type env T);

(*finds type of term without checking that combinations are consistent
  Ts holds types of bound variables*)
fun fastype (Envir {tyenv, ...}) =
  let
    val funerr = "fastype: expected function type";
    fun fast Ts (f $ u) =
          (case Type.devar tyenv (fast Ts f) of
            Type ("fun", [_, T]) => T
          | TVar v => raise TERM (funerr, [f $ u])
          | _ => raise TERM (funerr, [f $ u]))
      | fast Ts (Const (_, T)) = T
      | fast Ts (Free (_, T)) = T
      | fast Ts (Bound i) =
          (nth Ts i handle General.Subscript => raise TERM ("fastype: Bound", [Bound i]))
      | fast Ts (Var (_, T)) = T
      | fast Ts (Abs (_, T, u)) = T --> fast (T :: Ts) u;
  in fast end;


(** plain substitution -- without variable chasing **)

local

fun subst_type0 tyenv = Term_Subst.map_atypsT_same
  (fn TVar v =>
        (case Type.lookup tyenv v of
          SOME U => U
        | NONE => raise Same.SAME)
    | _ => raise Same.SAME);

fun subst_term1 tenv = Term_Subst.map_aterms_same
  (fn Var v =>
        (case lookup1 tenv v of
          SOME u => u
        | NONE => raise Same.SAME)
    | _ => raise Same.SAME);

fun subst_term2 tenv tyenv : term Same.operation =
  let
    val substT = subst_type0 tyenv;
    fun subst (Const (a, T)) = Const (a, substT T)
      | subst (Free (a, T)) = Free (a, substT T)
      | subst (Var (xi, T)) =
          (case lookup1 tenv (xi, T) of
            SOME u => u
          | NONE => Var (xi, substT T))
      | subst (Bound _) = raise Same.SAME
      | subst (Abs (a, T, t)) =
          (Abs (a, substT T, Same.commit subst t)
            handle Same.SAME => Abs (a, T, subst t))
      | subst (t $ u) = (subst t $ Same.commit subst u handle Same.SAME => t $ subst u);
  in subst end;

in

fun subst_type_same tyenv T =
  if Vartab.is_empty tyenv then raise Same.SAME
  else subst_type0 tyenv T;

fun subst_term_types_same tyenv t =
  if Vartab.is_empty tyenv then raise Same.SAME
  else Term_Subst.map_types_same (subst_type0 tyenv) t;

fun subst_term_same (tyenv, tenv) =
  if Vartab.is_empty tenv then subst_term_types_same tyenv
  else if Vartab.is_empty tyenv then subst_term1 tenv
  else subst_term2 tenv tyenv;

fun subst_type tyenv T = subst_type_same tyenv T handle Same.SAME => T;
fun subst_term_types tyenv t = subst_term_types_same tyenv t handle Same.SAME => t;
fun subst_term envs t = subst_term_same envs t handle Same.SAME => t;

end;



(** expand defined atoms -- with local beta reduction **)

fun expand_atom T (U, u) =
  subst_term_types (Type.raw_match (U, T) Vartab.empty) u
    handle Type.TYPE_MATCH => raise TYPE ("expand_atom: ill-typed replacement", [T, U], [u]);

fun expand_term get =
  let
    fun expand tm =
      let
        val (head, args) = Term.strip_comb tm;
        val args' = map expand args;
        fun comb head' = Term.list_comb (head', args');
      in
        (case head of
          Abs (x, T, t) => comb (Abs (x, T, expand t))
        | _ =>
          (case get head of
            SOME def => Term.betapplys (expand_atom (Term.fastype_of head) def, args')
          | NONE => comb head))
      end;
  in expand end;

fun expand_term_frees defs =
  let
    val eqs = map (fn ((x, U), u) => (x, (U, u))) defs;
    val get = fn Free (x, _) => AList.lookup (op =) eqs x | _ => NONE;
  in expand_term get end;

end;
