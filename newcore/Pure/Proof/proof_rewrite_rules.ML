(*  Title:      Pure/Proof/proof_rewrite_rules.ML
    Author:     Stefan Berghofer, TU Muenchen

Simplification functions for proof terms involving meta level rules.
*)

signature PROOF_REWRITE_RULES =
sig
  val rew : bool -> typ list -> term option list -> Proofterm.proof -> (Proofterm.proof * Proofterm.proof) option
  val rprocs : bool ->
    (typ list -> term option list -> Proofterm.proof -> (Proofterm.proof * Proofterm.proof) option) list
  val rewrite_terms : (term -> term) -> Proofterm.proof -> Proofterm.proof
  val elim_defs : theory -> bool -> thm list -> Proofterm.proof -> Proofterm.proof
  val elim_vars : (typ -> term) -> Proofterm.proof -> Proofterm.proof
  val hhf_proof : term -> term -> Proofterm.proof -> Proofterm.proof
  val un_hhf_proof : term -> term -> Proofterm.proof -> Proofterm.proof
  val mk_of_sort_proof : theory -> term option list -> typ * sort -> Proofterm.proof list
  val expand_of_class : theory -> typ list -> term option list -> Proofterm.proof ->
    (Proofterm.proof * Proofterm.proof) option
end;

structure ProofRewriteRules : PROOF_REWRITE_RULES =
struct

fun rew b _ _ =
  let
    fun ?? x = if b then SOME x else NONE;
    fun ax (prf as PAxm (s, prop, _)) Ts =
      if b then PAxm (s, prop, SOME Ts) else prf;
    fun ty T = if b then
        let val Type (_, [Type (_, [U, _]), _]) = T
        in SOME U end
      else NONE;
    val equal_intr_axm = ax Proofterm.equal_intr_axm [];
    val equal_elim_axm = ax Proofterm.equal_elim_axm [];
    val symmetric_axm = ax Proofterm.symmetric_axm [propT];

    fun rew' (PThm (_, (("Pure.protectD", _, _), _)) % _ %%
        (PThm (_, (("Pure.protectI", _, _), _)) % _ %% prf)) = SOME prf
      | rew' (PThm (_, (("Pure.conjunctionD1", _, _), _)) % _ % _ %%
        (PThm (_, (("Pure.conjunctionI", _, _), _)) % _ % _ %% prf %% _)) = SOME prf
      | rew' (PThm (_, (("Pure.conjunctionD2", _, _), _)) % _ % _ %%
        (PThm (_, (("Pure.conjunctionI", _, _), _)) % _ % _ %% _ %% prf)) = SOME prf
      | rew' (PAxm ("Pure.equal_elim", _, _) % _ % _ %%
        (PAxm ("Pure.equal_intr", _, _) % _ % _ %% prf %% _)) = SOME prf
      | rew' (PAxm ("Pure.symmetric", _, _) % _ % _ %%
        (PAxm ("Pure.equal_intr", _, _) % A % B %% prf1 %% prf2)) =
            SOME (equal_intr_axm % B % A %% prf2 %% prf1)

      | rew' (PAxm ("Pure.equal_elim", _, _) % SOME (_ $ A) % SOME (_ $ B) %%
        (PAxm ("Pure.combination", _, _) % SOME (Const ("Pure.prop", _)) %
          _ % _ % _ %% (PAxm ("Pure.reflexive", _, _) % _) %% prf1) %%
        ((tg as PThm (_, (("Pure.protectI", _, _), _))) % _ %% prf2)) =
        SOME (tg %> B %% (equal_elim_axm %> A %> B %% prf1 %% prf2))

      | rew' (PAxm ("Pure.equal_elim", _, _) % SOME (_ $ A) % SOME (_ $ B) %%
        (PAxm ("Pure.symmetric", _, _) % _ % _ %%
          (PAxm ("Pure.combination", _, _) % SOME (Const ("Pure.prop", _)) %
             _ % _ % _ %% (PAxm ("Pure.reflexive", _, _) % _) %% prf1)) %%
        ((tg as PThm (_, (("Pure.protectI", _, _), _))) % _ %% prf2)) =
        SOME (tg %> B %% (equal_elim_axm %> A %> B %%
          (symmetric_axm % ?? B % ?? A %% prf1) %% prf2))

      | rew' (PAxm ("Pure.equal_elim", _, _) % SOME X % SOME Y %%
        (PAxm ("Pure.combination", _, _) % _ % _ % _ % _ %%
          (PAxm ("Pure.combination", _, _) % SOME (Const ("Pure.imp", _)) % _ % _ % _ %%
             (PAxm ("Pure.reflexive", _, _) % _) %% prf1) %% prf2)) =
        let
          val _ $ A $ C = Envir.beta_norm X;
          val _ $ B $ D = Envir.beta_norm Y
        in SOME (AbsP ("H1", ?? X, AbsP ("H2", ?? B,
          Proofterm.equal_elim_axm %> C %> D %% Proofterm.incr_pboundvars 2 0 prf2 %%
            (PBound 1 %% (equal_elim_axm %> B %> A %%
              (Proofterm.symmetric_axm % ?? A % ?? B %% Proofterm.incr_pboundvars 2 0 prf1) %%
                PBound 0)))))
        end

      | rew' (PAxm ("Pure.equal_elim", _, _) % SOME X % SOME Y %%
        (PAxm ("Pure.symmetric", _, _) % _ % _ %%
          (PAxm ("Pure.combination", _, _) % _ % _ % _ % _ %%
            (PAxm ("Pure.combination", _, _) % SOME (Const ("Pure.imp", _)) % _ % _ % _ %%
               (PAxm ("Pure.reflexive", _, _) % _) %% prf1) %% prf2))) =
        let
          val _ $ A $ C = Envir.beta_norm Y;
          val _ $ B $ D = Envir.beta_norm X
        in SOME (AbsP ("H1", ?? X, AbsP ("H2", ?? A,
          equal_elim_axm %> D %> C %%
            (symmetric_axm % ?? C % ?? D %% Proofterm.incr_pboundvars 2 0 prf2) %%
              (PBound 1 %%
                (equal_elim_axm %> A %> B %% Proofterm.incr_pboundvars 2 0 prf1 %% PBound 0)))))
        end

      | rew' (PAxm ("Pure.equal_elim", _, _) % SOME X % SOME Y %%
        (PAxm ("Pure.combination", _, _) % SOME (Const ("Pure.all", _)) % _ % _ % _ %%
          (PAxm ("Pure.reflexive", _, _) % _) %%
            (PAxm ("Pure.abstract_rule", _, _) % _ % _ %% prf))) =
        let
          val Const (_, T) $ P = Envir.beta_norm X;
          val _ $ Q = Envir.beta_norm Y;
        in SOME (AbsP ("H", ?? X, Abst ("x", ty T,
            equal_elim_axm %> incr_boundvars 1 P $ Bound 0 %> incr_boundvars 1 Q $ Bound 0 %%
              (Proofterm.incr_pboundvars 1 1 prf %> Bound 0) %% (PBound 0 %> Bound 0))))
        end

      | rew' (PAxm ("Pure.equal_elim", _, _) % SOME X % SOME Y %%
        (PAxm ("Pure.symmetric", _, _) % _ % _ %%        
          (PAxm ("Pure.combination", _, _) % SOME (Const ("Pure.all", _)) % _ % _ % _ %%
            (PAxm ("Pure.reflexive", _, _) % _) %%
              (PAxm ("Pure.abstract_rule", _, _) % _ % _ %% prf)))) =
        let
          val Const (_, T) $ P = Envir.beta_norm X;
          val _ $ Q = Envir.beta_norm Y;
          val t = incr_boundvars 1 P $ Bound 0;
          val u = incr_boundvars 1 Q $ Bound 0
        in SOME (AbsP ("H", ?? X, Abst ("x", ty T,
          equal_elim_axm %> t %> u %%
            (symmetric_axm % ?? u % ?? t %% (Proofterm.incr_pboundvars 1 1 prf %> Bound 0))
              %% (PBound 0 %> Bound 0))))
        end

      | rew' (PAxm ("Pure.equal_elim", _, _) % SOME A % SOME C %%
        (PAxm ("Pure.transitive", _, _) % _ % SOME B % _ %% prf1 %% prf2) %% prf3) =
           SOME (equal_elim_axm %> B %> C %% prf2 %%
             (equal_elim_axm %> A %> B %% prf1 %% prf3))
      | rew' (PAxm ("Pure.equal_elim", _, _) % SOME A % SOME C %%
        (PAxm ("Pure.symmetric", _, _) % _ % _ %%
          (PAxm ("Pure.transitive", _, _) % _ % SOME B % _ %% prf1 %% prf2)) %% prf3) =
           SOME (equal_elim_axm %> B %> C %% (symmetric_axm % ?? C % ?? B %% prf1) %%
             (equal_elim_axm %> A %> B %% (symmetric_axm % ?? B % ?? A %% prf2) %% prf3))

      | rew' (PAxm ("Pure.equal_elim", _, _) % _ % _ %%
        (PAxm ("Pure.reflexive", _, _) % _) %% prf) = SOME prf
      | rew' (PAxm ("Pure.equal_elim", _, _) % _ % _ %%
        (PAxm ("Pure.symmetric", _, _) % _ % _ %%
          (PAxm ("Pure.reflexive", _, _) % _)) %% prf) = SOME prf

      | rew' (PAxm ("Pure.symmetric", _, _) % _ % _ %%
        (PAxm ("Pure.symmetric", _, _) % _ % _ %% prf)) = SOME prf

      | rew' (PAxm ("Pure.equal_elim", _, _) % _ % _ %%
        (PAxm ("Pure.equal_elim", _, _) % SOME (_ $ A $ C) % SOME (_ $ B $ D) %%
          (PAxm ("Pure.combination", _, _) % _ % _ % _ % _ %%
            (PAxm ("Pure.combination", _, _) % SOME (Const ("Pure.eq", _)) % _ % _ % _ %%
              (PAxm ("Pure.reflexive", _, _) % _) %% prf1) %% prf2) %% prf3) %% prf4) =
          SOME (equal_elim_axm %> C %> D %% prf2 %%
            (equal_elim_axm %> A %> C %% prf3 %%
              (equal_elim_axm %> B %> A %% (symmetric_axm % ?? A % ?? B %% prf1) %% prf4)))

      | rew' (PAxm ("Pure.equal_elim", _, _) % _ % _ %%
        (PAxm ("Pure.symmetric", _, _) % _ % _ %%
          (PAxm ("Pure.equal_elim", _, _) % SOME (_ $ A $ C) % SOME (_ $ B $ D) %%
            (PAxm ("Pure.combination", _, _) % _ % _ % _ % _ %%
              (PAxm ("Pure.combination", _, _) % SOME (Const ("Pure.eq", _)) % _ % _ % _ %%
                (PAxm ("Pure.reflexive", _, _) % _) %% prf1) %% prf2) %% prf3)) %% prf4) =
          SOME (equal_elim_axm %> A %> B %% prf1 %%
            (equal_elim_axm %> C %> A %% (symmetric_axm % ?? A % ?? C %% prf3) %%
              (equal_elim_axm %> D %> C %% (symmetric_axm % ?? C % ?? D %% prf2) %% prf4)))

      | rew' (PAxm ("Pure.equal_elim", _, _) % _ % _ %%
        (PAxm ("Pure.equal_elim", _, _) % SOME (_ $ B $ D) % SOME (_ $ A $ C) %%
          (PAxm ("Pure.symmetric", _, _) % _ % _ %%
            (PAxm ("Pure.combination", _, _) % _ % _ % _ % _ %%
              (PAxm ("Pure.combination", _, _) % SOME (Const ("Pure.eq", _)) % _ % _ % _ %%
                (PAxm ("Pure.reflexive", _, _) % _) %% prf1) %% prf2)) %% prf3) %% prf4) =
          SOME (equal_elim_axm %> D %> C %% (symmetric_axm % ?? C % ?? D %% prf2) %%
            (equal_elim_axm %> B %> D %% prf3 %%
              (equal_elim_axm %> A %> B %% prf1 %% prf4)))

      | rew' (PAxm ("Pure.equal_elim", _, _) % _ % _ %%
        (PAxm ("Pure.symmetric", _, _) % _ % _ %%
          (PAxm ("Pure.equal_elim", _, _) % SOME (_ $ B $ D) % SOME (_ $ A $ C) %%
            (PAxm ("Pure.symmetric", _, _) % _ % _ %%
              (PAxm ("Pure.combination", _, _) % _ % _ % _ % _ %%
                (PAxm ("Pure.combination", _, _) % SOME (Const ("Pure.eq", _)) % _ % _ % _ %%
                  (PAxm ("Pure.reflexive", _, _) % _) %% prf1) %% prf2)) %% prf3)) %% prf4) =
          SOME (equal_elim_axm %> B %> A %% (symmetric_axm % ?? A % ?? B %% prf1) %%
            (equal_elim_axm %> D %> B %% (symmetric_axm % ?? B % ?? D %% prf3) %%
              (equal_elim_axm %> C %> D %% prf2 %% prf4)))

      | rew' ((prf as PAxm ("Pure.combination", _, _) %
        SOME ((eq as Const ("Pure.eq", T)) $ t) % _ % _ % _) %%
          (PAxm ("Pure.reflexive", _, _) % _)) =
        let val (U, V) = (case T of
          Type (_, [U, V]) => (U, V) | _ => (dummyT, dummyT))
        in SOME (prf %% (ax Proofterm.combination_axm [U, V] %> eq % ?? eq % ?? t % ?? t %%
          (ax Proofterm.reflexive_axm [T] % ?? eq) %% (ax Proofterm.reflexive_axm [U] % ?? t)))
        end

      | rew' _ = NONE;
  in rew' #> Option.map (rpair Proofterm.no_skel) end;

fun rprocs b = [rew b];
val _ = Theory.setup (fold Proofterm.add_prf_rproc (rprocs false));


(**** apply rewriting function to all terms in proof ****)

fun rewrite_terms r =
  let
    fun rew_term Ts t =
      let
        val frees =
          map Free (Name.invent (Term.declare_term_frees t Name.context) "xa" (length Ts) ~~ Ts);
        val t' = r (subst_bounds (frees, t));
        fun strip [] t = t
          | strip (_ :: xs) (Abs (_, _, t)) = strip xs t;
      in
        strip Ts (fold lambda frees t')
      end;

    fun rew Ts (prf1 %% prf2) = rew Ts prf1 %% rew Ts prf2
      | rew Ts (prf % SOME t) = rew Ts prf % SOME (rew_term Ts t)
      | rew Ts (Abst (s, SOME T, prf)) = Abst (s, SOME T, rew (T :: Ts) prf)
      | rew Ts (AbsP (s, SOME t, prf)) = AbsP (s, SOME (rew_term Ts t), rew Ts prf)
      | rew _ prf = prf

  in rew [] end;


(**** eliminate definitions in proof ****)

fun vars_of t = rev (fold_aterms (fn v as Var _ => insert (op =) v | _ => I) t []);

fun insert_refl defs Ts (prf1 %% prf2) =
      let val (prf1', b) = insert_refl defs Ts prf1
      in
        if b then (prf1', true)
        else (prf1' %% fst (insert_refl defs Ts prf2), false)
      end
  | insert_refl defs Ts (Abst (s, SOME T, prf)) =
      (Abst (s, SOME T, fst (insert_refl defs (T :: Ts) prf)), false)
  | insert_refl defs Ts (AbsP (s, t, prf)) =
      (AbsP (s, t, fst (insert_refl defs Ts prf)), false)
  | insert_refl defs Ts prf =
      (case Proofterm.strip_combt prf of
        (PThm (_, ((s, prop, SOME Ts), _)), ts) =>
          if member (op =) defs s then
            let
              val vs = vars_of prop;
              val tvars = Term.add_tvars prop [] |> rev;
              val (_, rhs) = Logic.dest_equals (Logic.strip_imp_concl prop);
              val rhs' = Term.betapplys (subst_TVars (map fst tvars ~~ Ts)
                (fold_rev (fn x => fn b => Abs ("", dummyT, abstract_over (x, b))) vs rhs),
                map the ts);
            in
              (Proofterm.change_type (SOME [fastype_of1 (Ts, rhs')])
                Proofterm.reflexive_axm %> rhs', true)
            end
          else (prf, false)
      | (_, []) => (prf, false)
      | (prf', ts) => (Proofterm.proof_combt' (fst (insert_refl defs Ts prf'), ts), false));

fun elim_defs thy r defs prf =
  let
    val defs' = map (Logic.dest_equals o
      map_types Type.strip_sorts o prop_of o Drule.abs_def) defs;
    val defnames = map Thm.derivation_name defs;
    val f = if not r then I else
      let
        val cnames = map (fst o dest_Const o fst) defs';
        val thms = Proofterm.fold_proof_atoms true
          (fn PThm (_, ((name, prop, _), _)) =>
              if member (op =) defnames name orelse
                not (exists_Const (member (op =) cnames o #1) prop)
              then I
              else cons (name, SOME prop)
            | _ => I) [prf] [];
      in Reconstruct.expand_proof thy thms end;
  in
    rewrite_terms (Pattern.rewrite_term thy defs' [])
      (fst (insert_refl defnames [] (f prf)))
  end;


(**** eliminate all variables that don't occur in the proposition ****)

fun elim_vars mk_default prf =
  let
    val prop = Reconstruct.prop_of prf;
    val tv = Term.add_vars prop [];
    val tf = Term.add_frees prop [];

    fun hidden_variable (Var v) = not (member (op =) tv v)
      | hidden_variable (Free f) = not (member (op =) tf f)
      | hidden_variable _ = false;

    fun mk_default' T =
      fold_rev (Term.abs o pair "x") (binder_types T) (mk_default (body_type T));

    fun elim_varst (t $ u) = elim_varst t $ elim_varst u
      | elim_varst (Abs (s, T, t)) = Abs (s, T, elim_varst t)
      | elim_varst (t as Free (x, T)) = if member (op =) tf (x, T) then t else mk_default' T
      | elim_varst (t as Var (xi, T)) = if member (op =) tv (xi, T) then t else mk_default' T
      | elim_varst t = t;
  in
    Proofterm.map_proof_terms (fn t =>
      if Term.exists_subterm hidden_variable t then Envir.beta_norm (elim_varst t) else t) I prf
  end;


(**** convert between hhf and non-hhf form ****)

fun hhf_proof P Q prf =
  let
    val params = Logic.strip_params Q;
    val Hs = Logic.strip_assums_hyp P;
    val Hs' = Logic.strip_assums_hyp Q;
    val k = length Hs;
    val l = length params;
    fun mk_prf i j Hs Hs' (Const ("Pure.all", _) $ Abs (_, _, P)) prf =
          mk_prf i (j - 1) Hs Hs' P (prf %> Bound j)
      | mk_prf i j (H :: Hs) (H' :: Hs') (Const ("Pure.imp", _) $ _ $ P) prf =
          mk_prf (i - 1) j Hs Hs' P (prf %% un_hhf_proof H' H (PBound i))
      | mk_prf _ _ _ _ _ prf = prf
  in
    prf |> Proofterm.incr_pboundvars k l |> mk_prf (k - 1) (l - 1) Hs Hs' P |>
    fold_rev (fn P => fn prf => AbsP ("H", SOME P, prf)) Hs' |>
    fold_rev (fn (s, T) => fn prf => Abst (s, SOME T, prf)) params
  end
and un_hhf_proof P Q prf =
  let
    val params = Logic.strip_params Q;
    val Hs = Logic.strip_assums_hyp P;
    val Hs' = Logic.strip_assums_hyp Q;
    val k = length Hs;
    val l = length params;
    fun mk_prf (Const ("Pure.all", _) $ Abs (s, T, P)) prf =
          Abst (s, SOME T, mk_prf P prf)
      | mk_prf (Const ("Pure.imp", _) $ P $ Q) prf =
          AbsP ("H", SOME P, mk_prf Q prf)
      | mk_prf _ prf = prf
  in
    prf |> Proofterm.incr_pboundvars k l |>
    fold (fn i => fn prf => prf %> Bound i) (l - 1 downto 0) |>
    fold (fn ((H, H'), i) => fn prf => prf %% hhf_proof H' H (PBound i))
      (Hs ~~ Hs' ~~ (k - 1 downto 0)) |>
    mk_prf Q
  end;


(**** expand OfClass proofs ****)

fun mk_of_sort_proof thy hs (T, S) =
  let
    val hs' = map
      (fn SOME t => (SOME (Logic.dest_of_class t) handle TERM _ => NONE)
        | NONE => NONE) hs;
    val sorts = AList.coalesce (op =) (rev (map_filter I hs'));
    fun get_sort T = the_default [] (AList.lookup (op =) sorts T);
    val subst = map_atyps
      (fn T as TVar (ixn, _) => TVar (ixn, get_sort T)
        | T as TFree (s, _) => TFree (s, get_sort T));
    fun hyp T_c = case find_index (equal (SOME T_c)) hs' of
        ~1 => error "expand_of_class: missing class hypothesis"
      | i => PBound i;
    fun reconstruct prf prop = prf |>
      Reconstruct.reconstruct_proof thy prop |>
      Reconstruct.expand_proof thy [("", NONE)] |>
      Same.commit (Proofterm.map_proof_same Same.same Same.same hyp)
  in
    map2 reconstruct
      (Proofterm.of_sort_proof thy (OfClass o apfst Type.strip_sorts) (subst T, S))
      (Logic.mk_of_sort (T, S))
  end;

fun expand_of_class thy Ts hs (OfClass (T, c)) =
      mk_of_sort_proof thy hs (T, [c]) |>
      hd |> rpair Proofterm.no_skel |> SOME
  | expand_of_class thy Ts hs _ = NONE;

end;
