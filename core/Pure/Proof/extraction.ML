(*  Title:      Pure/Proof/extraction.ML
    Author:     Stefan Berghofer, TU Muenchen

Extraction of programs from proofs.
*)

signature EXTRACTION =
sig
  val set_preprocessor : (theory -> Proofterm.proof -> Proofterm.proof) -> theory -> theory
  val add_realizes_eqns_i : ((term * term) list * (term * term)) list -> theory -> theory
  val add_realizes_eqns : string list -> theory -> theory
  val add_typeof_eqns_i : ((term * term) list * (term * term)) list -> theory -> theory
  val add_typeof_eqns : string list -> theory -> theory
  val add_realizers_i : (string * (string list * term * Proofterm.proof)) list
    -> theory -> theory
  val add_realizers : (thm * (string list * string * string)) list
    -> theory -> theory
  val add_expand_thm : bool -> thm -> theory -> theory
  val add_types : (xstring * ((term -> term option) list *
    (term -> typ -> term -> typ -> term) option)) list -> theory -> theory
  val extract : (thm * string list) list -> theory -> theory
  val nullT : typ
  val nullt : term
  val mk_typ : typ -> term
  val etype_of : theory -> string list -> typ list -> term -> typ
  val realizes_of: theory -> string list -> term -> term -> term
  val abs_corr_shyps: theory -> thm -> string list -> term list -> Proofterm.proof -> Proofterm.proof
end;

structure Extraction : EXTRACTION =
struct

(**** tools ****)

val typ = Simple_Syntax.read_typ;

val add_syntax =
  Sign.root_path
  #> Sign.add_types_global
    [(Binding.make ("Type", @{here}), 0, NoSyn),
     (Binding.make ("Null", @{here}), 0, NoSyn)]
  #> Sign.add_consts
    [(Binding.make ("typeof", @{here}), typ "'b => Type", NoSyn),
     (Binding.make ("Type", @{here}), typ "'a itself => Type", NoSyn),
     (Binding.make ("Null", @{here}), typ "Null", NoSyn),
     (Binding.make ("realizes", @{here}), typ "'a => 'b => 'b", NoSyn)];

val nullT = Type ("Null", []);
val nullt = Const ("Null", nullT);

fun mk_typ T =
  Const ("Type", Term.itselfT T --> Type ("Type", [])) $ Logic.mk_type T;

fun typeof_proc defaultS vs (Const ("typeof", _) $ u) =
      SOME (mk_typ (case strip_comb u of
          (Var ((a, i), _), _) =>
            if member (op =) vs a then TFree ("'" ^ a ^ ":" ^ string_of_int i, defaultS)
            else nullT
        | (Free (a, _), _) =>
            if member (op =) vs a then TFree ("'" ^ a, defaultS) else nullT
        | _ => nullT))
  | typeof_proc _ _ _ = NONE;

fun rlz_proc (Const ("realizes", Type (_, [Type ("Null", []), _])) $ _ $ t) = SOME t
  | rlz_proc (Const ("realizes", Type (_, [T, _])) $ r $ t) =
      (case strip_comb t of
         (Var (ixn, U), ts) => SOME (list_comb (Var (ixn, T --> U), r :: ts))
       | (Free (s, U), ts) => SOME (list_comb (Free (s, T --> U), r :: ts))
       | _ => NONE)
  | rlz_proc _ = NONE;

val unpack_ixn = apfst implode o apsnd (fst o read_int o tl) o
  take_prefix (fn s => s <> ":") o raw_explode;

type rules =
  {next: int, rs: ((term * term) list * (term * term)) list,
   net: (int * ((term * term) list * (term * term))) Net.net};

val empty_rules : rules = {next = 0, rs = [], net = Net.empty};

fun add_rule (r as (_, (lhs, _))) ({next, rs, net} : rules) =
  {next = next - 1, rs = r :: rs, net = Net.insert_term (K false)
     (Envir.eta_contract lhs, (next, r)) net};

fun merge_rules ({next, rs = rs1, net} : rules) ({rs = rs2, ...} : rules) =
  fold_rev add_rule (subtract (op =) rs1 rs2) {next = next, rs = rs1, net = net};

fun condrew thy rules procs =
  let
    fun rew tm =
      Pattern.rewrite_term thy [] (condrew' :: procs) tm
    and condrew' tm =
      let
        val cache = Unsynchronized.ref ([] : (term * term) list);
        fun lookup f x = (case AList.lookup (op =) (!cache) x of
            NONE =>
              let val y = f x
              in (cache := (x, y) :: !cache; y) end
          | SOME y => y);
      in
        get_first (fn (_, (prems, (tm1, tm2))) =>
        let
          fun ren t = the_default t (Term.rename_abs tm1 tm t);
          val inc = Logic.incr_indexes ([], maxidx_of_term tm + 1);
          val env as (Tenv, tenv) = Pattern.match thy (inc tm1, tm) (Vartab.empty, Vartab.empty);
          val prems' = map (pairself (Envir.subst_term env o inc o ren)) prems;
          val env' = Envir.Envir
            {maxidx = fold (fn (t, u) => Term.maxidx_term t #> Term.maxidx_term u) prems' ~1,
             tenv = tenv, tyenv = Tenv};
          val env'' = fold (Pattern.unify thy o pairself (lookup rew)) prems' env';
        in SOME (Envir.norm_term env'' (inc (ren tm2)))
        end handle Pattern.MATCH => NONE | Pattern.Unif => NONE)
          (sort (int_ord o pairself fst)
            (Net.match_term rules (Envir.eta_contract tm)))
      end;

  in rew end;

val chtype = Proofterm.change_type o SOME;

fun extr_name s vs = Long_Name.append "extr" (space_implode "_" (s :: vs));
fun corr_name s vs = extr_name s vs ^ "_correctness";

fun msg d s = Output.urgent_message (Pretty.spaces d ^ s);

fun vars_of t = map Var (rev (Term.add_vars t []));
fun frees_of t = map Free (rev (Term.add_frees t []));
fun vfs_of t = vars_of t @ frees_of t;

val mkabs = fold_rev (fn v => fn t => Abs ("x", fastype_of v, abstract_over (v, t)));

val mkabsp = fold_rev (fn t => fn prf => AbsP ("H", SOME t, prf));

fun strip_abs 0 t = t
  | strip_abs n (Abs (_, _, t)) = strip_abs (n-1) t
  | strip_abs _ _ = error "strip_abs: not an abstraction";

val prf_subst_TVars = Proofterm.map_proof_types o typ_subst_TVars;

fun relevant_vars types prop =
  List.foldr
    (fn (Var ((a, _), T), vs) =>
        (case body_type T of
          Type (s, _) => if member (op =) types s then a :: vs else vs
        | _ => vs)
      | (_, vs) => vs) [] (vars_of prop);

fun tname_of (Type (s, _)) = s
  | tname_of _ = "";

fun get_var_type t =
  let
    val vs = Term.add_vars t [];
    val fs = Term.add_frees t [];
  in
    fn Var (ixn, _) =>
        (case AList.lookup (op =) vs ixn of
          NONE => error "get_var_type: no such variable in term"
        | SOME T => Var (ixn, T))
     | Free (s, _) =>
        (case AList.lookup (op =) fs s of
          NONE => error "get_var_type: no such variable in term"
        | SOME T => Free (s, T))
    | _ => error "get_var_type: not a variable"
  end;

fun read_term thy T s =
  let
    val ctxt = Proof_Context.init_global thy
      |> Config.put Type_Infer_Context.const_sorts false
      |> Proof_Context.set_defsort [];
    val parse = if T = propT then Syntax.parse_prop else Syntax.parse_term;
  in parse ctxt s |> Type.constraint T |> Syntax.check_term ctxt end;


(**** theory data ****)

(* theory data *)

structure ExtractionData = Theory_Data
(
  type T =
    {realizes_eqns : rules,
     typeof_eqns : rules,
     types : (string * ((term -> term option) list *
       (term -> typ -> term -> typ -> term) option)) list,
     realizers : (string list * (term * proof)) list Symtab.table,
     defs : thm list,
     expand : string list,
     prep : (theory -> proof -> proof) option}

  val empty =
    {realizes_eqns = empty_rules,
     typeof_eqns = empty_rules,
     types = [],
     realizers = Symtab.empty,
     defs = [],
     expand = [],
     prep = NONE};
  val extend = I;

  fun merge
    ({realizes_eqns = realizes_eqns1, typeof_eqns = typeof_eqns1, types = types1,
       realizers = realizers1, defs = defs1, expand = expand1, prep = prep1},
      {realizes_eqns = realizes_eqns2, typeof_eqns = typeof_eqns2, types = types2,
       realizers = realizers2, defs = defs2, expand = expand2, prep = prep2}) : T =
    {realizes_eqns = merge_rules realizes_eqns1 realizes_eqns2,
     typeof_eqns = merge_rules typeof_eqns1 typeof_eqns2,
     types = AList.merge (op =) (K true) (types1, types2),
     realizers = Symtab.merge_list (eq_set (op =) o pairself #1) (realizers1, realizers2),
     defs = Library.merge Thm.eq_thm (defs1, defs2),
     expand = Library.merge (op =) (expand1, expand2),
     prep = if is_some prep1 then prep1 else prep2};
);

fun read_condeq thy =
  let val thy' = add_syntax thy
  in fn s =>
    let val t = Logic.varify_global (read_term thy' propT s)
    in
      (map Logic.dest_equals (Logic.strip_imp_prems t),
        Logic.dest_equals (Logic.strip_imp_concl t))
      handle TERM _ => error ("Not a (conditional) meta equality:\n" ^ s)
    end
  end;

(** preprocessor **)

fun set_preprocessor prep thy =
  let val {realizes_eqns, typeof_eqns, types, realizers,
    defs, expand, ...} = ExtractionData.get thy
  in
    ExtractionData.put
      {realizes_eqns = realizes_eqns, typeof_eqns = typeof_eqns, types = types,
       realizers = realizers, defs = defs, expand = expand, prep = SOME prep} thy
  end;

(** equations characterizing realizability **)

fun gen_add_realizes_eqns prep_eq eqns thy =
  let val {realizes_eqns, typeof_eqns, types, realizers,
    defs, expand, prep} = ExtractionData.get thy;
  in
    ExtractionData.put
      {realizes_eqns = fold_rev add_rule (map (prep_eq thy) eqns) realizes_eqns,
       typeof_eqns = typeof_eqns, types = types, realizers = realizers,
       defs = defs, expand = expand, prep = prep} thy
  end

val add_realizes_eqns_i = gen_add_realizes_eqns (K I);
val add_realizes_eqns = gen_add_realizes_eqns read_condeq;

(** equations characterizing type of extracted program **)

fun gen_add_typeof_eqns prep_eq eqns thy =
  let
    val {realizes_eqns, typeof_eqns, types, realizers,
      defs, expand, prep} = ExtractionData.get thy;
    val eqns' = map (prep_eq thy) eqns
  in
    ExtractionData.put
      {realizes_eqns = realizes_eqns, realizers = realizers,
       typeof_eqns = fold_rev add_rule eqns' typeof_eqns,
       types = types, defs = defs, expand = expand, prep = prep} thy
  end

val add_typeof_eqns_i = gen_add_typeof_eqns (K I);
val add_typeof_eqns = gen_add_typeof_eqns read_condeq;

fun thaw (T as TFree (a, S)) =
      if exists_string (fn s => s = ":") a then TVar (unpack_ixn a, S) else T
  | thaw (Type (a, Ts)) = Type (a, map thaw Ts)
  | thaw T = T;

fun freeze (TVar ((a, i), S)) = TFree (a ^ ":" ^ string_of_int i, S)
  | freeze (Type (a, Ts)) = Type (a, map freeze Ts)
  | freeze T = T;

fun freeze_thaw f x =
  map_types thaw (f (map_types freeze x));

fun etype_of thy vs Ts t =
  let
    val {typeof_eqns, ...} = ExtractionData.get thy;
    fun err () = error ("Unable to determine type of extracted program for\n" ^
      Syntax.string_of_term_global thy t)
  in
    (case
      strip_abs_body
        (freeze_thaw (condrew thy (#net typeof_eqns) [typeof_proc [] vs])
          (fold (Term.abs o pair "x") Ts
            (Const ("typeof", fastype_of1 (Ts, t) --> Type ("Type", [])) $ t))) of
      Const ("Type", _) $ u => (Logic.dest_type u handle TERM _ => err ())
    | _ => err ())
  end;

(** realizers for axioms / theorems, together with correctness proofs **)

fun gen_add_realizers prep_rlz rs thy =
  let val {realizes_eqns, typeof_eqns, types, realizers,
    defs, expand, prep} = ExtractionData.get thy
  in
    ExtractionData.put
      {realizes_eqns = realizes_eqns, typeof_eqns = typeof_eqns, types = types,
       realizers = fold (Symtab.cons_list o prep_rlz thy) rs realizers,
       defs = defs, expand = expand, prep = prep} thy
  end

fun prep_realizer thy =
  let
    val {realizes_eqns, typeof_eqns, defs, types, ...} =
      ExtractionData.get thy;
    val procs = maps (fst o snd) types;
    val rtypes = map fst types;
    val eqns = Net.merge (K false) (#net realizes_eqns, #net typeof_eqns);
    val thy' = add_syntax thy;
    val rd = Proof_Syntax.read_proof thy' true false;
  in fn (thm, (vs, s1, s2)) =>
    let
      val name = Thm.derivation_name thm;
      val _ = name <> "" orelse error "add_realizers: unnamed theorem";
      val prop = Thm.unconstrainT thm |> prop_of |>
        Pattern.rewrite_term thy' (map (Logic.dest_equals o prop_of) defs) [];
      val vars = vars_of prop;
      val vars' = filter_out (fn v =>
        member (op =) rtypes (tname_of (body_type (fastype_of v)))) vars;
      val shyps = maps (fn Var ((x, i), _) =>
        if member (op =) vs x then Logic.mk_of_sort
          (TVar (("'" ^ x, i), []), Sign.defaultS thy')
        else []) vars;
      val T = etype_of thy' vs [] prop;
      val (T', thw) = Type.legacy_freeze_thaw_type
        (if T = nullT then nullT else map fastype_of vars' ---> T);
      val t = map_types thw (read_term thy' T' s1);
      val r' = freeze_thaw (condrew thy' eqns
        (procs @ [typeof_proc [] vs, rlz_proc]))
          (Const ("realizes", T --> propT --> propT) $
            (if T = nullT then t else list_comb (t, vars')) $ prop);
      val r = Logic.list_implies (shyps,
        fold_rev Logic.all (map (get_var_type r') vars) r');
      val prf = Reconstruct.reconstruct_proof thy' r (rd s2);
    in (name, (vs, (t, prf))) end
  end;

val add_realizers_i = gen_add_realizers
  (fn _ => fn (name, (vs, t, prf)) => (name, (vs, (t, prf))));
val add_realizers = gen_add_realizers prep_realizer;

fun realizes_of thy vs t prop =
  let
    val thy' = add_syntax thy;
    val {realizes_eqns, typeof_eqns, defs, types, ...} =
      ExtractionData.get thy';
    val procs = maps (rev o fst o snd) types;
    val eqns = Net.merge (K false) (#net realizes_eqns, #net typeof_eqns);
    val prop' = Pattern.rewrite_term thy'
      (map (Logic.dest_equals o prop_of) defs) [] prop;
  in freeze_thaw (condrew thy' eqns
    (procs @ [typeof_proc [] vs, rlz_proc]))
      (Const ("realizes", fastype_of t --> propT --> propT) $ t $ prop')
  end;

fun abs_corr_shyps thy thm vs xs prf =
  let
    val S = Sign.defaultS thy;
    val ((atyp_map, constraints, _), prop') =
      Logic.unconstrainT (#shyps (rep_thm thm)) (prop_of thm);
    val atyps = fold_types (fold_atyps (insert (op =))) (prop_of thm) [];
    val Ts = map_filter (fn ((v, i), _) => if member (op =) vs v then
        SOME (TVar (("'" ^ v, i), [])) else NONE)
      (rev (Term.add_vars prop' []));
    val cs = maps (fn T => map (pair T) S) Ts;
    val constraints' = map Logic.mk_of_class cs;
    fun typ_map T = Type.strip_sorts
      (map_atyps (fn U => if member (op =) atyps U then atyp_map U else U) T);
    fun mk_hyp (T, c) = Hyp (Logic.mk_of_class (typ_map T, c));
    val xs' = map (map_types typ_map) xs
  in
    prf |>
    Same.commit (Proofterm.map_proof_same (map_types typ_map) typ_map mk_hyp) |>
    fold_rev Proofterm.implies_intr_proof' (map snd constraints) |>
    fold_rev Proofterm.forall_intr_proof' xs' |>
    fold_rev Proofterm.implies_intr_proof' constraints'
  end;

(** expanding theorems / definitions **)

fun add_expand_thm is_def thm thy =
  let
    val {realizes_eqns, typeof_eqns, types, realizers,
      defs, expand, prep} = ExtractionData.get thy;

    val name = Thm.derivation_name thm;
    val _ = name <> "" orelse error "add_expand_thm: unnamed theorem";
  in
    thy |> ExtractionData.put
      (if is_def then
        {realizes_eqns = realizes_eqns,
         typeof_eqns = add_rule ([], Logic.dest_equals (map_types
           Type.strip_sorts (prop_of (Drule.abs_def thm)))) typeof_eqns,
         types = types,
         realizers = realizers, defs = insert Thm.eq_thm thm defs,
         expand = expand, prep = prep}
      else
        {realizes_eqns = realizes_eqns, typeof_eqns = typeof_eqns, types = types,
         realizers = realizers, defs = defs,
         expand = insert (op =) name expand, prep = prep})
  end;

fun extraction_expand is_def =
  Thm.declaration_attribute (fn th => Context.mapping (add_expand_thm is_def th) I);


(** types with computational content **)

fun add_types tys thy =
  ExtractionData.map
    (fn {realizes_eqns, typeof_eqns, types, realizers, defs, expand, prep} =>
      {realizes_eqns = realizes_eqns, typeof_eqns = typeof_eqns,
       types = fold (AList.update (op =) o apfst (Sign.intern_type thy)) tys types,
       realizers = realizers, defs = defs, expand = expand, prep = prep})
    thy;


(** Pure setup **)

val _ = Theory.setup
  (add_types [("prop", ([], NONE))] #>

   add_typeof_eqns
     ["(typeof (PROP P)) == (Type (TYPE(Null))) ==>  \
    \  (typeof (PROP Q)) == (Type (TYPE('Q))) ==>  \
    \    (typeof (PROP P ==> PROP Q)) == (Type (TYPE('Q)))",

      "(typeof (PROP Q)) == (Type (TYPE(Null))) ==>  \
    \    (typeof (PROP P ==> PROP Q)) == (Type (TYPE(Null)))",

      "(typeof (PROP P)) == (Type (TYPE('P))) ==>  \
    \  (typeof (PROP Q)) == (Type (TYPE('Q))) ==>  \
    \    (typeof (PROP P ==> PROP Q)) == (Type (TYPE('P => 'Q)))",

      "(%x. typeof (PROP P (x))) == (%x. Type (TYPE(Null))) ==>  \
    \    (typeof (!!x. PROP P (x))) == (Type (TYPE(Null)))",

      "(%x. typeof (PROP P (x))) == (%x. Type (TYPE('P))) ==>  \
    \    (typeof (!!x::'a. PROP P (x))) == (Type (TYPE('a => 'P)))",

      "(%x. typeof (f (x))) == (%x. Type (TYPE('f))) ==>  \
    \    (typeof (f)) == (Type (TYPE('f)))"] #>

   add_realizes_eqns
     ["(typeof (PROP P)) == (Type (TYPE(Null))) ==>  \
    \    (realizes (r) (PROP P ==> PROP Q)) ==  \
    \    (PROP realizes (Null) (PROP P) ==> PROP realizes (r) (PROP Q))",

      "(typeof (PROP P)) == (Type (TYPE('P))) ==>  \
    \  (typeof (PROP Q)) == (Type (TYPE(Null))) ==>  \
    \    (realizes (r) (PROP P ==> PROP Q)) ==  \
    \    (!!x::'P. PROP realizes (x) (PROP P) ==> PROP realizes (Null) (PROP Q))",

      "(realizes (r) (PROP P ==> PROP Q)) ==  \
    \  (!!x. PROP realizes (x) (PROP P) ==> PROP realizes (r (x)) (PROP Q))",

      "(%x. typeof (PROP P (x))) == (%x. Type (TYPE(Null))) ==>  \
    \    (realizes (r) (!!x. PROP P (x))) ==  \
    \    (!!x. PROP realizes (Null) (PROP P (x)))",

      "(realizes (r) (!!x. PROP P (x))) ==  \
    \  (!!x. PROP realizes (r (x)) (PROP P (x)))"] #>

   Attrib.setup @{binding extraction_expand} (Scan.succeed (extraction_expand false))
     "specify theorems to be expanded during extraction" #>
   Attrib.setup @{binding extraction_expand_def} (Scan.succeed (extraction_expand true))
     "specify definitions to be expanded during extraction");


(**** extract program ****)

val dummyt = Const ("dummy", dummyT);

fun extract thms thy =
  let
    val thy' = add_syntax thy;
    val {realizes_eqns, typeof_eqns, types, realizers, defs, expand, prep} =
      ExtractionData.get thy;
    val procs = maps (rev o fst o snd) types;
    val rtypes = map fst types;
    val typroc = typeof_proc [];
    val prep = the_default (K I) prep thy' o ProofRewriteRules.elim_defs thy' false defs o
      Reconstruct.expand_proof thy' (map (rpair NONE) ("" :: expand));
    val rrews = Net.merge (K false) (#net realizes_eqns, #net typeof_eqns);

    fun find_inst prop Ts ts vs =
      let
        val rvs = relevant_vars rtypes prop;
        val vars = vars_of prop;
        val n = Int.min (length vars, length ts);

        fun add_args (Var ((a, i), _), t) (vs', tye) =
          if member (op =) rvs a then
            let val T = etype_of thy' vs Ts t
            in if T = nullT then (vs', tye)
               else (a :: vs', (("'" ^ a, i), T) :: tye)
            end
          else (vs', tye)

      in fold_rev add_args (take n vars ~~ take n ts) ([], []) end;

    fun mk_shyps tye = maps (fn (ixn, _) =>
      Logic.mk_of_sort (TVar (ixn, []), Sign.defaultS thy)) tye;

    fun mk_sprfs cs tye = maps (fn (_, T) =>
      ProofRewriteRules.mk_of_sort_proof thy (map SOME cs)
        (T, Sign.defaultS thy)) tye;

    fun find (vs: string list) = Option.map snd o find_first (curry (eq_set (op =)) vs o fst);
    fun find' (s: string) = map_filter (fn (s', x) => if s = s' then SOME x else NONE);

    fun app_rlz_rews Ts vs t =
      strip_abs (length Ts)
        (freeze_thaw (condrew thy' rrews (procs @ [typroc vs, rlz_proc]))
          (fold (Term.abs o pair "x") Ts t));

    fun realizes_null vs prop = app_rlz_rews [] vs
      (Const ("realizes", nullT --> propT --> propT) $ nullt $ prop);

    fun corr d vs ts Ts hs cs _ (PBound i) _ defs = (PBound i, defs)

      | corr d vs ts Ts hs cs t (Abst (s, SOME T, prf)) (Abst (_, _, prf')) defs =
          let val (corr_prf, defs') = corr d vs [] (T :: Ts)
            (dummyt :: hs) cs (case t of SOME (Abs (_, _, u)) => SOME u | _ => NONE)
            prf (Proofterm.incr_pboundvars 1 0 prf') defs
          in (Abst (s, SOME T, corr_prf), defs') end

      | corr d vs ts Ts hs cs t (AbsP (s, SOME prop, prf)) (AbsP (_, _, prf')) defs =
          let
            val T = etype_of thy' vs Ts prop;
            val u = if T = nullT then
                (case t of SOME u => SOME (incr_boundvars 1 u) | NONE => NONE)
              else (case t of SOME (Abs (_, _, u)) => SOME u | _ => NONE);
            val (corr_prf, defs') =
              corr d vs [] (T :: Ts) (prop :: hs)
                (prop :: cs) u (Proofterm.incr_pboundvars 0 1 prf)
                (Proofterm.incr_pboundvars 0 1 prf') defs;
            val rlz = Const ("realizes", T --> propT --> propT)
          in (
            if T = nullT then AbsP ("R",
              SOME (app_rlz_rews Ts vs (rlz $ nullt $ prop)),
                Proofterm.prf_subst_bounds [nullt] corr_prf)
            else Abst (s, SOME T, AbsP ("R",
              SOME (app_rlz_rews (T :: Ts) vs
                (rlz $ Bound 0 $ incr_boundvars 1 prop)), corr_prf)), defs')
          end

      | corr d vs ts Ts hs cs t' (prf % SOME t) (prf' % _) defs =
          let
            val (Us, T) = strip_type (fastype_of1 (Ts, t));
            val (corr_prf, defs') = corr d vs (t :: ts) Ts hs cs
              (if member (op =) rtypes (tname_of T) then t'
               else (case t' of SOME (u $ _) => SOME u | _ => NONE))
               prf prf' defs;
            val u = if not (member (op =) rtypes (tname_of T)) then t else
              let
                val eT = etype_of thy' vs Ts t;
                val (r, Us') = if eT = nullT then (nullt, Us) else
                  (Bound (length Us), eT :: Us);
                val u = list_comb (incr_boundvars (length Us') t,
                  map Bound (length Us - 1 downto 0));
                val u' = (case AList.lookup (op =) types (tname_of T) of
                    SOME ((_, SOME f)) => f r eT u T
                  | _ => Const ("realizes", eT --> T --> T) $ r $ u)
              in app_rlz_rews Ts vs (fold_rev (Term.abs o pair "x") Us' u') end
          in (corr_prf % SOME u, defs') end

      | corr d vs ts Ts hs cs t (prf1 %% prf2) (prf1' %% prf2') defs =
          let
            val prop = Reconstruct.prop_of' hs prf2';
            val T = etype_of thy' vs Ts prop;
            val (f, u, defs1) = if T = nullT then (t, NONE, defs) else
              (case t of
                 SOME (f $ u) => (SOME f, SOME u, defs)
               | _ =>
                 let val (u, defs1) = extr d vs [] Ts hs prf2' defs
                 in (NONE, SOME u, defs1) end)
            val ((corr_prf1, corr_prf2), defs2) =
              defs1
              |> corr d vs [] Ts hs cs f prf1 prf1'
              ||>> corr d vs [] Ts hs cs u prf2 prf2';
          in
            if T = nullT then (corr_prf1 %% corr_prf2, defs2) else
              (corr_prf1 % u %% corr_prf2, defs2)
          end

      | corr d vs ts Ts hs cs _ (prf0 as PThm (_, ((name, prop, SOME Ts'), body))) _ defs =
          let
            val prf = Proofterm.join_proof body;
            val (vs', tye) = find_inst prop Ts ts vs;
            val shyps = mk_shyps tye;
            val sprfs = mk_sprfs cs tye;
            val tye' = (map fst (Term.add_tvars prop [] |> rev) ~~ Ts') @ tye;
            val T = etype_of thy' vs' [] prop;
            val defs' = if T = nullT then defs
              else snd (extr d vs ts Ts hs prf0 defs)
          in
            if T = nullT andalso realizes_null vs' prop aconv prop then (prf0, defs)
            else (case Symtab.lookup realizers name of
              NONE => (case find vs' (find' name defs') of
                NONE =>
                  let
                    val _ = T = nullT orelse error "corr: internal error";
                    val _ = msg d ("Building correctness proof for " ^ quote name ^
                      (if null vs' then ""
                       else " (relevant variables: " ^ commas_quote vs' ^ ")"));
                    val prf' = prep (Reconstruct.reconstruct_proof thy' prop prf);
                    val (corr_prf0, defs'') = corr (d + 1) vs' [] [] []
                      (rev shyps) NONE prf' prf' defs';
                    val corr_prf = mkabsp shyps corr_prf0;
                    val corr_prop = Reconstruct.prop_of corr_prf;
                    val corr_prf' =
                      Proofterm.proof_combP (Proofterm.proof_combt
                         (PThm (serial (),
                          ((corr_name name vs', corr_prop, SOME (map TVar (Term.add_tvars corr_prop [] |> rev))),
                            Future.value (Proofterm.approximate_proof_body corr_prf))),
                              vfs_of corr_prop),
                              map PBound (length shyps - 1 downto 0)) |>
                      fold_rev Proofterm.forall_intr_proof'
                        (map (get_var_type corr_prop) (vfs_of prop)) |>
                      mkabsp shyps
                  in
                    (Proofterm.proof_combP (prf_subst_TVars tye' corr_prf', sprfs),
                      (name, (vs', ((nullt, nullt), (corr_prf, corr_prf')))) :: defs'')
                  end
              | SOME (_, (_, prf')) =>
                  (Proofterm.proof_combP (prf_subst_TVars tye' prf', sprfs), defs'))
            | SOME rs => (case find vs' rs of
                SOME (_, prf') => (Proofterm.proof_combP (prf_subst_TVars tye' prf', sprfs), defs')
              | NONE => error ("corr: no realizer for instance of theorem " ^
                  quote name ^ ":\n" ^ Syntax.string_of_term_global thy' (Envir.beta_norm
                    (Reconstruct.prop_of (Proofterm.proof_combt (prf0, ts)))))))
          end

      | corr d vs ts Ts hs cs _ (prf0 as PAxm (s, prop, SOME Ts')) _ defs =
          let
            val (vs', tye) = find_inst prop Ts ts vs;
            val tye' = (map fst (Term.add_tvars prop [] |> rev) ~~ Ts') @ tye
          in
            if etype_of thy' vs' [] prop = nullT andalso
              realizes_null vs' prop aconv prop then (prf0, defs)
            else case find vs' (Symtab.lookup_list realizers s) of
              SOME (_, prf) => (Proofterm.proof_combP (prf_subst_TVars tye' prf, mk_sprfs cs tye),
                defs)
            | NONE => error ("corr: no realizer for instance of axiom " ^
                quote s ^ ":\n" ^ Syntax.string_of_term_global thy' (Envir.beta_norm
                  (Reconstruct.prop_of (Proofterm.proof_combt (prf0, ts)))))
          end

      | corr d vs ts Ts hs _ _ _ _ defs = error "corr: bad proof"

    and extr d vs ts Ts hs (PBound i) defs = (Bound i, defs)

      | extr d vs ts Ts hs (Abst (s, SOME T, prf)) defs =
          let val (t, defs') = extr d vs []
            (T :: Ts) (dummyt :: hs) (Proofterm.incr_pboundvars 1 0 prf) defs
          in (Abs (s, T, t), defs') end

      | extr d vs ts Ts hs (AbsP (s, SOME t, prf)) defs =
          let
            val T = etype_of thy' vs Ts t;
            val (t, defs') =
              extr d vs [] (T :: Ts) (t :: hs) (Proofterm.incr_pboundvars 0 1 prf) defs
          in
            (if T = nullT then subst_bound (nullt, t) else Abs (s, T, t), defs')
          end

      | extr d vs ts Ts hs (prf % SOME t) defs =
          let val (u, defs') = extr d vs (t :: ts) Ts hs prf defs
          in (if member (op =) rtypes (tname_of (body_type (fastype_of1 (Ts, t)))) then u
            else u $ t, defs')
          end

      | extr d vs ts Ts hs (prf1 %% prf2) defs =
          let
            val (f, defs') = extr d vs [] Ts hs prf1 defs;
            val prop = Reconstruct.prop_of' hs prf2;
            val T = etype_of thy' vs Ts prop
          in
            if T = nullT then (f, defs') else
              let val (t, defs'') = extr d vs [] Ts hs prf2 defs'
              in (f $ t, defs'') end
          end

      | extr d vs ts Ts hs (prf0 as PThm (_, ((s, prop, SOME Ts'), body))) defs =
          let
            val prf = Proofterm.join_proof body;
            val (vs', tye) = find_inst prop Ts ts vs;
            val shyps = mk_shyps tye;
            val tye' = (map fst (Term.add_tvars prop [] |> rev) ~~ Ts') @ tye
          in
            case Symtab.lookup realizers s of
              NONE => (case find vs' (find' s defs) of
                NONE =>
                  let
                    val _ = msg d ("Extracting " ^ quote s ^
                      (if null vs' then ""
                       else " (relevant variables: " ^ commas_quote vs' ^ ")"));
                    val prf' = prep (Reconstruct.reconstruct_proof thy' prop prf);
                    val (t, defs') = extr (d + 1) vs' [] [] [] prf' defs;
                    val (corr_prf, defs'') = corr (d + 1) vs' [] [] []
                      (rev shyps) (SOME t) prf' prf' defs';

                    val nt = Envir.beta_norm t;
                    val args = filter_out (fn v => member (op =) rtypes
                      (tname_of (body_type (fastype_of v)))) (vfs_of prop);
                    val args' = filter (fn v => Logic.occs (v, nt)) args;
                    val t' = mkabs args' nt;
                    val T = fastype_of t';
                    val cname = extr_name s vs';
                    val c = Const (cname, T);
                    val u = mkabs args (list_comb (c, args'));
                    val eqn = Logic.mk_equals (c, t');
                    val rlz =
                      Const ("realizes", fastype_of nt --> propT --> propT);
                    val lhs = app_rlz_rews [] vs' (rlz $ nt $ prop);
                    val rhs = app_rlz_rews [] vs' (rlz $ list_comb (c, args') $ prop);
                    val f = app_rlz_rews [] vs'
                      (Abs ("x", T, rlz $ list_comb (Bound 0, args') $ prop));

                    val corr_prf' = mkabsp shyps
                      (chtype [] Proofterm.equal_elim_axm %> lhs %> rhs %%
                       (chtype [propT] Proofterm.symmetric_axm %> rhs %> lhs %%
                         (chtype [T, propT] Proofterm.combination_axm %> f %> f %> c %> t' %%
                           (chtype [T --> propT] Proofterm.reflexive_axm %> f) %%
                           PAxm (Thm.def_name cname, eqn,
                             SOME (map TVar (Term.add_tvars eqn [] |> rev))))) %% corr_prf);
                    val corr_prop = Reconstruct.prop_of corr_prf';
                    val corr_prf'' =
                      Proofterm.proof_combP (Proofterm.proof_combt
                        (PThm (serial (),
                         ((corr_name s vs', corr_prop, SOME (map TVar (Term.add_tvars corr_prop [] |> rev))),
                           Future.value (Proofterm.approximate_proof_body corr_prf'))),
                            vfs_of corr_prop),
                             map PBound (length shyps - 1 downto 0)) |>
                      fold_rev Proofterm.forall_intr_proof'
                        (map (get_var_type corr_prop) (vfs_of prop)) |>
                      mkabsp shyps
                  in
                    (subst_TVars tye' u,
                      (s, (vs', ((t', u), (corr_prf', corr_prf'')))) :: defs'')
                  end
              | SOME ((_, u), _) => (subst_TVars tye' u, defs))
            | SOME rs => (case find vs' rs of
                SOME (t, _) => (subst_TVars tye' t, defs)
              | NONE => error ("extr: no realizer for instance of theorem " ^
                  quote s ^ ":\n" ^ Syntax.string_of_term_global thy' (Envir.beta_norm
                    (Reconstruct.prop_of (Proofterm.proof_combt (prf0, ts))))))
          end

      | extr d vs ts Ts hs (prf0 as PAxm (s, prop, SOME Ts')) defs =
          let
            val (vs', tye) = find_inst prop Ts ts vs;
            val tye' = (map fst (Term.add_tvars prop [] |> rev) ~~ Ts') @ tye
          in
            case find vs' (Symtab.lookup_list realizers s) of
              SOME (t, _) => (subst_TVars tye' t, defs)
            | NONE => error ("extr: no realizer for instance of axiom " ^
                quote s ^ ":\n" ^ Syntax.string_of_term_global thy' (Envir.beta_norm
                  (Reconstruct.prop_of (Proofterm.proof_combt (prf0, ts)))))
          end

      | extr d vs ts Ts hs _ defs = error "extr: bad proof";

    fun prep_thm (thm, vs) =
      let
        val thy = Thm.theory_of_thm thm;
        val prop = Thm.prop_of thm;
        val prf = Thm.proof_of thm;
        val name = Thm.derivation_name thm;
        val _ = name <> "" orelse error "extraction: unnamed theorem";
        val _ = etype_of thy' vs [] prop <> nullT orelse error ("theorem " ^
          quote name ^ " has no computational content")
      in (Reconstruct.reconstruct_proof thy prop prf, vs) end;

    val defs =
      fold (fn (prf, vs) => snd o extr 0 vs [] [] [] prf)
        (map prep_thm thms) [];

    fun add_def (s, (vs, ((t, u), (prf, _)))) thy =
      (case Sign.const_type thy (extr_name s vs) of
         NONE =>
           let
             val corr_prop = Reconstruct.prop_of prf;
             val ft = Type.legacy_freeze t;
             val fu = Type.legacy_freeze u;
             val (def_thms, thy') = if t = nullt then ([], thy) else
               thy
               |> Sign.add_consts [(Binding.qualified_name (extr_name s vs), fastype_of ft, NoSyn)]
               |> Global_Theory.add_defs false
                  [((Binding.qualified_name (Thm.def_name (extr_name s vs)),
                    Logic.mk_equals (head_of (strip_abs_body fu), ft)), [])]
           in
             thy'
             |> Global_Theory.store_thm (Binding.qualified_name (corr_name s vs),
                  Thm.varifyT_global (funpow (length (vars_of corr_prop))
                    (Thm.forall_elim_var 0) (Thm.forall_intr_frees
                      (Proof_Checker.thm_of_proof thy'
                       (fst (Proofterm.freeze_thaw_prf prf))))))
             |> snd
             |> fold Code.add_default_eqn def_thms
           end
       | SOME _ => thy);

  in
    thy
    |> Sign.root_path
    |> fold_rev add_def defs
    |> Sign.restore_naming thy
  end;

val etype_of = etype_of o add_syntax;

end;
