(*  Title:      Pure/type_infer_context.ML
    Author:     Stefan Berghofer and Markus Wenzel, TU Muenchen

Type-inference preparation and standard type inference.
*)

signature TYPE_INFER_CONTEXT =
sig
  val const_sorts: bool Config.T
  val const_type: Proof.context -> string -> typ option
  val prepare_positions: Proof.context -> term list -> term list * (Position.T * typ) list
  val prepare: Proof.context -> term list -> int * term list
  val infer_types: Proof.context -> term list -> term list
end;

structure Type_Infer_Context: TYPE_INFER_CONTEXT =
struct

(** prepare types/terms: create inference parameters **)

(* constraints *)

val const_sorts =
  Config.bool (Config.declare ("const_sorts", @{here}) (K (Config.Bool true)));

fun const_type ctxt =
  try ((not (Config.get ctxt const_sorts) ? Type.strip_sorts) o
    Consts.the_constraint (Proof_Context.consts_of ctxt));

fun var_type ctxt = the_default dummyT o Proof_Context.def_type ctxt;


(* prepare_typ *)

fun prepare_typ typ params_idx =
  let
    val (params', idx) = fold_atyps
      (fn TVar (xi, S) =>
          (fn ps_idx as (ps, idx) =>
            if Type_Infer.is_param xi andalso not (Vartab.defined ps xi)
            then (Vartab.update (xi, Type_Infer.mk_param idx S) ps, idx + 1) else ps_idx)
        | _ => I) typ params_idx;

    fun prepare (T as Type (a, Ts)) idx =
          if T = dummyT then (Type_Infer.mk_param idx [], idx + 1)
          else
            let val (Ts', idx') = fold_map prepare Ts idx
            in (Type (a, Ts'), idx') end
      | prepare (T as TVar (xi, _)) idx =
          (case Vartab.lookup params' xi of
            NONE => T
          | SOME p => p, idx)
      | prepare (TFree ("'_dummy_", S)) idx = (Type_Infer.mk_param idx S, idx + 1)
      | prepare (T as TFree _) idx = (T, idx);

    val (typ', idx') = prepare typ idx;
  in (typ', (params', idx')) end;


(* prepare_term *)

fun prepare_term ctxt tm (vparams, params, idx) =
  let
    fun add_vparm xi (ps_idx as (ps, idx)) =
      if not (Vartab.defined ps xi) then
        (Vartab.update (xi, Type_Infer.mk_param idx []) ps, idx + 1)
      else ps_idx;

    val (vparams', idx') = fold_aterms
      (fn Var (_, Type ("_polymorphic_", _)) => I
        | Var (xi, _) => add_vparm xi
        | Free (x, _) => add_vparm (x, ~1)
        | _ => I)
      tm (vparams, idx);
    fun var_param xi = the (Vartab.lookup vparams' xi);

    fun polyT_of T idx =
      apsnd snd (prepare_typ (Type_Infer.paramify_vars T) (Vartab.empty, idx));

    fun constraint T t ps =
      if T = dummyT then (t, ps)
      else
        let val (T', ps') = prepare_typ T ps
        in (Type.constraint T' t, ps') end;

    fun prepare (Const ("_type_constraint_", T) $ t) ps_idx =
          let
            val A = Type.constraint_type ctxt T;
            val (A', ps_idx') = prepare_typ A ps_idx;
            val (t', ps_idx'') = prepare t ps_idx';
          in (Const ("_type_constraint_", A' --> A') $ t', ps_idx'') end
      | prepare (Const (c, T)) (ps, idx) =
          (case const_type ctxt c of
            SOME U =>
              let val (U', idx') = polyT_of U idx
              in constraint T (Const (c, U')) (ps, idx') end
          | NONE => error ("Undeclared constant: " ^ quote c))
      | prepare (Var (xi, Type ("_polymorphic_", [T]))) (ps, idx) =
          let val (T', idx') = polyT_of T idx
          in (Var (xi, T'), (ps, idx')) end
      | prepare (Var (xi, T)) ps_idx = constraint T (Var (xi, var_param xi)) ps_idx
      | prepare (Free (x, T)) ps_idx = constraint T (Free (x, var_param (x, ~1))) ps_idx
      | prepare (Bound i) ps_idx = (Bound i, ps_idx)
      | prepare (Abs (x, T, t)) ps_idx =
          let
            val (T', ps_idx') = prepare_typ T ps_idx;
            val (t', ps_idx'') = prepare t ps_idx';
          in (Abs (x, T', t'), ps_idx'') end
      | prepare (t $ u) ps_idx =
          let
            val (t', ps_idx') = prepare t ps_idx;
            val (u', ps_idx'') = prepare u ps_idx';
          in (t' $ u', ps_idx'') end;

    val (tm', (params', idx'')) = prepare tm (params, idx');
  in (tm', (vparams', params', idx'')) end;


(* prepare_positions *)

fun prepare_positions ctxt tms =
  let
    fun prepareT (Type (a, Ts)) ps_idx =
          let val (Ts', ps_idx') = fold_map prepareT Ts ps_idx
          in (Type (a, Ts'), ps_idx') end
      | prepareT T (ps, idx) =
          (case Term_Position.decode_positionT T of
            SOME pos =>
              let val U = Type_Infer.mk_param idx []
              in (U, ((pos, U) :: ps, idx + 1)) end
          | NONE => (T, (ps, idx)));

    fun prepare (Const ("_type_constraint_", T)) ps_idx =
          let
            val A = Type.constraint_type ctxt T;
            val (A', ps_idx') = prepareT A ps_idx;
          in (Const ("_type_constraint_", A' --> A'), ps_idx') end
      | prepare (Const (c, T)) ps_idx =
          let val (T', ps_idx') = prepareT T ps_idx
          in (Const (c, T'), ps_idx') end
      | prepare (Free (x, T)) ps_idx =
          let val (T', ps_idx') = prepareT T ps_idx
          in (Free (x, T'), ps_idx') end
      | prepare (Var (xi, T)) ps_idx =
          let val (T', ps_idx') = prepareT T ps_idx
          in (Var (xi, T'), ps_idx') end
      | prepare (t as Bound _) ps_idx = (t, ps_idx)
      | prepare (Abs (x, T, t)) ps_idx =
          let
            val (T', ps_idx') = prepareT T ps_idx;
            val (t', ps_idx'') = prepare t ps_idx';
          in (Abs (x, T', t'), ps_idx'') end
      | prepare (t $ u) ps_idx =
          let
            val (t', ps_idx') = prepare t ps_idx;
            val (u', ps_idx'') = prepare u ps_idx';
          in (t' $ u', ps_idx'') end;

    val idx = Type_Infer.param_maxidx_of tms + 1;
    val (tms', (ps, _)) = fold_map prepare tms ([], idx);
  in (tms', ps) end;



(** order-sorted unification of types **)

exception NO_UNIFIER of string * typ Vartab.table;

fun unify ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;
    val arity_sorts = Proof_Context.arity_sorts ctxt;


    (* adjust sorts of parameters *)

    fun not_of_sort x S' S =
      "Variable " ^ x ^ "::" ^ Syntax.string_of_sort ctxt S' ^ " not of sort " ^
        Syntax.string_of_sort ctxt S;

    fun meet (_, []) tye_idx = tye_idx
      | meet (Type (a, Ts), S) (tye_idx as (tye, _)) =
          meets (Ts, arity_sorts a S handle ERROR msg => raise NO_UNIFIER (msg, tye)) tye_idx
      | meet (TFree (x, S'), S) (tye_idx as (tye, _)) =
          if Sign.subsort thy (S', S) then tye_idx
          else raise NO_UNIFIER (not_of_sort x S' S, tye)
      | meet (TVar (xi, S'), S) (tye_idx as (tye, idx)) =
          if Sign.subsort thy (S', S) then tye_idx
          else if Type_Infer.is_param xi then
            (Vartab.update_new
              (xi, Type_Infer.mk_param idx (Sign.inter_sort thy (S', S))) tye, idx + 1)
          else raise NO_UNIFIER (not_of_sort (Term.string_of_vname xi) S' S, tye)
    and meets (T :: Ts, S :: Ss) (tye_idx as (tye, _)) =
          meets (Ts, Ss) (meet (Type_Infer.deref tye T, S) tye_idx)
      | meets _ tye_idx = tye_idx;


    (* occurs check and assignment *)

    fun occurs_check tye xi (TVar (xi', _)) =
          if xi = xi' then raise NO_UNIFIER ("Occurs check!", tye)
          else
            (case Vartab.lookup tye xi' of
              NONE => ()
            | SOME T => occurs_check tye xi T)
      | occurs_check tye xi (Type (_, Ts)) = List.app (occurs_check tye xi) Ts
      | occurs_check _ _ _ = ();

    fun assign xi (T as TVar (xi', _)) S env =
          if xi = xi' then env
          else env |> meet (T, S) |>> Vartab.update_new (xi, T)
      | assign xi T S (env as (tye, _)) =
          (occurs_check tye xi T; env |> meet (T, S) |>> Vartab.update_new (xi, T));


    (* unification *)

    fun show_tycon (a, Ts) =
      quote (Syntax.string_of_typ ctxt (Type (a, replicate (length Ts) dummyT)));

    fun unif (T1, T2) (env as (tye, _)) =
      (case apply2 (`Type_Infer.is_paramT o Type_Infer.deref tye) (T1, T2) of
        ((true, TVar (xi, S)), (_, T)) => assign xi T S env
      | ((_, T), (true, TVar (xi, S))) => assign xi T S env
      | ((_, Type (a, Ts)), (_, Type (b, Us))) =>
          if a <> b then
            raise NO_UNIFIER
              ("Clash of types " ^ show_tycon (a, Ts) ^ " and " ^ show_tycon (b, Us), tye)
          else fold unif (Ts ~~ Us) env
      | ((_, T), (_, U)) => if T = U then env else raise NO_UNIFIER ("", tye));

  in unif end;



(** simple type inference **)

(* infer *)

fun infer ctxt =
  let
    (* errors *)

    fun prep_output tye bs ts Ts =
      let
        val (Ts_bTs', ts') = Type_Infer.finish ctxt tye (Ts @ map snd bs, ts);
        val (Ts', Ts'') = chop (length Ts) Ts_bTs';
        fun prep t =
          let val xs = rev (Term.variant_frees t (rev (map fst bs ~~ Ts'')))
          in Term.subst_bounds (map Syntax_Trans.mark_bound_abs xs, t) end;
      in (map prep ts', Ts') end;

    fun err_loose i = error ("Loose bound variable: B." ^ string_of_int i);

    fun unif_failed msg =
      "Type unification failed" ^ (if msg = "" then "" else ": " ^ msg) ^ "\n\n";

    fun err_appl msg tye bs t T u U =
      let val ([t', u'], [T', U']) = prep_output tye bs [t, u] [T, U]
      in error (unif_failed msg ^ Type.appl_error ctxt t' T' u' U' ^ "\n") end;


    (* main *)

    fun inf _ (Const (_, T)) tye_idx = (T, tye_idx)
      | inf _ (Free (_, T)) tye_idx = (T, tye_idx)
      | inf _ (Var (_, T)) tye_idx = (T, tye_idx)
      | inf bs (Bound i) tye_idx =
          (snd (nth bs i handle General.Subscript => err_loose i), tye_idx)
      | inf bs (Abs (x, T, t)) tye_idx =
          let val (U, tye_idx') = inf ((x, T) :: bs) t tye_idx
          in (T --> U, tye_idx') end
      | inf bs (t $ u) tye_idx =
          let
            val (T, tye_idx') = inf bs t tye_idx;
            val (U, (tye, idx)) = inf bs u tye_idx';
            val V = Type_Infer.mk_param idx [];
            val tye_idx'' = unify ctxt (U --> V, T) (tye, idx + 1)
              handle NO_UNIFIER (msg, tye') => err_appl msg tye' bs t T u U;
          in (V, tye_idx'') end;

  in inf [] end;


(* main interfaces *)

fun prepare ctxt raw_ts =
  let
    val constrain_vars = Term.map_aterms
      (fn Free (x, T) => Type.constraint T (Free (x, var_type ctxt (x, ~1)))
        | Var (xi, T) => Type.constraint T (Var (xi, var_type ctxt xi))
        | t => t);

    val ts = burrow_types (Syntax.check_typs ctxt) raw_ts;
    val idx = Type_Infer.param_maxidx_of ts + 1;
    val (ts', (_, _, idx')) =
      fold_map (prepare_term ctxt o constrain_vars) ts
        (Vartab.empty, Vartab.empty, idx);
  in (idx', ts') end;

fun infer_types ctxt raw_ts =
  let
    val (idx, ts) = prepare ctxt raw_ts;
    val (tye, _) = fold (snd oo infer ctxt) ts (Vartab.empty, idx);
    val (_, ts') = Type_Infer.finish ctxt tye ([], ts);
  in ts' end;

end;
