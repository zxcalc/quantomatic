(*  Title:      Pure/Isar/class.ML
    Author:     Florian Haftmann, TU Muenchen

Type classes derived from primitive axclasses and locales.
*)

signature CLASS =
sig
  (*classes*)
  val is_class: theory -> class -> bool
  val these_params: theory -> sort -> (string * (class * (string * typ))) list
  val base_sort: theory -> class -> sort
  val rules: theory -> class -> thm option * thm
  val these_defs: theory -> sort -> thm list
  val these_operations: theory -> sort -> (string * (class * ((typ * term) * bool))) list
  val print_classes: Proof.context -> unit
  val init: class -> theory -> Proof.context
  val begin: class list -> sort -> Proof.context -> Proof.context
  val const: class -> (binding * mixfix) * term -> term list * term list ->
    local_theory -> local_theory
  val abbrev: class -> Syntax.mode -> (binding * mixfix) * term -> local_theory ->
    (term * term) * local_theory
  val redeclare_operations: theory -> sort -> Proof.context -> Proof.context
  val class_prefix: string -> string
  val register: class -> class list -> ((string * typ) * (string * typ)) list ->
    sort -> morphism -> morphism -> thm option -> thm option -> thm -> theory -> theory

  (*instances*)
  val instantiation: string list * (string * sort) list * sort -> theory -> local_theory
  val instantiation_instance: (local_theory -> local_theory)
    -> local_theory -> Proof.state
  val prove_instantiation_instance: (Proof.context -> tactic)
    -> local_theory -> local_theory
  val prove_instantiation_exit: (Proof.context -> tactic)
    -> local_theory -> theory
  val prove_instantiation_exit_result: (morphism -> 'a -> 'b)
    -> (Proof.context -> 'b -> tactic) -> 'a -> local_theory -> 'b * theory
  val read_multi_arity: theory -> xstring list * xstring list * xstring
    -> string list * (string * sort) list * sort
  val instantiation_cmd: xstring list * xstring list * xstring -> theory -> local_theory
  val instance_arity_cmd: xstring list * xstring list * xstring -> theory -> Proof.state

  (*subclasses*)
  val classrel: class * class -> theory -> Proof.state
  val classrel_cmd: xstring * xstring -> theory -> Proof.state
  val register_subclass: class * class -> morphism option -> Element.witness option
    -> morphism -> local_theory -> local_theory

  (*tactics*)
  val intro_classes_tac: Proof.context -> thm list -> tactic
  val standard_intro_classes_tac: Proof.context -> thm list -> tactic

  (*diagnostics*)
  val pretty_specification: theory -> class -> Pretty.T list
end;

structure Class: CLASS =
struct

(** class data **)

datatype class_data = Class_Data of {

  (* static part *)
  consts: (string * string) list
    (*locale parameter ~> constant name*),
  base_sort: sort,
  base_morph: morphism
    (*static part of canonical morphism*),
  export_morph: morphism,
  assm_intro: thm option,
  of_class: thm,
  axiom: thm option,

  (* dynamic part *)
  defs: thm list,
  operations: (string * (class * ((typ * term) * bool))) list

  (* n.b.
    params = logical parameters of class
    operations = operations participating in user-space type system
  *)
};

fun make_class_data ((consts, base_sort, base_morph, export_morph, assm_intro, of_class, axiom),
    (defs, operations)) =
  Class_Data {consts = consts, base_sort = base_sort,
    base_morph = base_morph, export_morph = export_morph, assm_intro = assm_intro,
    of_class = of_class, axiom = axiom, defs = defs, operations = operations};
fun map_class_data f (Class_Data {consts, base_sort, base_morph, export_morph, assm_intro,
    of_class, axiom, defs, operations}) =
  make_class_data (f ((consts, base_sort, base_morph, export_morph, assm_intro, of_class, axiom),
    (defs, operations)));
fun merge_class_data _ (Class_Data {consts = consts,
    base_sort = base_sort, base_morph = base_morph, export_morph = export_morph, assm_intro = assm_intro,
    of_class = of_class, axiom = axiom, defs = defs1, operations = operations1},
  Class_Data {consts = _, base_sort = _, base_morph = _, export_morph = _, assm_intro = _,
    of_class = _, axiom = _, defs = defs2, operations = operations2}) =
  make_class_data ((consts, base_sort, base_morph, export_morph, assm_intro, of_class, axiom),
    (Thm.merge_thms (defs1, defs2),
      AList.merge (op =) (K true) (operations1, operations2)));

structure Class_Data = Theory_Data
(
  type T = class_data Graph.T
  val empty = Graph.empty;
  val extend = I;
  val merge = Graph.join merge_class_data;
);


(* queries *)

fun lookup_class_data thy class =
  (case try (Graph.get_node (Class_Data.get thy)) class of
    SOME (Class_Data data) => SOME data
  | NONE => NONE);

fun the_class_data thy class =
  (case lookup_class_data thy class of
    NONE => error ("Undeclared class " ^ quote class)
  | SOME data => data);

val is_class = is_some oo lookup_class_data;

val ancestry = Graph.all_succs o Class_Data.get;
val heritage = Graph.all_preds o Class_Data.get;

fun these_params thy =
  let
    fun params class =
      let
        val const_typs = (#params o Axclass.get_info thy) class;
        val const_names = (#consts o the_class_data thy) class;
      in
        (map o apsnd)
          (fn c => (class, (c, (the o AList.lookup (op =) const_typs) c))) const_names
      end;
  in maps params o ancestry thy end;

val base_sort = #base_sort oo the_class_data;

fun rules thy class =
  let val {axiom, of_class, ...} = the_class_data thy class
  in (axiom, of_class) end;

fun all_assm_intros thy =
  Graph.fold (fn (_, (Class_Data {assm_intro, ...}, _)) => fold (insert Thm.eq_thm)
    (the_list assm_intro)) (Class_Data.get thy) [];

fun these_defs thy = maps (#defs o the_class_data thy) o ancestry thy;
fun these_operations thy = maps (#operations o the_class_data thy) o ancestry thy;

val base_morphism = #base_morph oo the_class_data;

fun morphism thy class =
  (case Element.eq_morphism thy (these_defs thy [class]) of
    SOME eq_morph => base_morphism thy class $> eq_morph
  | NONE => base_morphism thy class);

val export_morphism = #export_morph oo the_class_data;

fun pretty_param ctxt (c, ty) =
  Pretty.block
   [Name_Space.pretty ctxt (Proof_Context.const_space ctxt) c, Pretty.str " ::",
    Pretty.brk 1, Syntax.pretty_typ ctxt ty];

fun print_classes ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;
    val algebra = Sign.classes_of thy;

    val class_space = Proof_Context.class_space ctxt;
    val type_space = Proof_Context.type_space ctxt;

    val arities =
      Symtab.empty
      |> Symtab.fold (fn (tyco, arities) => fold (fn (class, _) =>
           Symtab.map_default (class, []) (insert (op =) tyco)) arities)
             (Sorts.arities_of algebra);

    fun prt_supersort class =
      Syntax.pretty_sort ctxt (Sign.minimize_sort thy (Sign.super_classes thy class));

    fun prt_arity class tyco =
      let
        val Ss = Sorts.mg_domain algebra tyco [class];
      in Syntax.pretty_arity ctxt (tyco, Ss, [class]) end;

    fun prt_param (c, ty) = pretty_param ctxt (c, Type.strip_sorts_dummy ty);

    fun prt_entry class =
      Pretty.block
        ([Pretty.keyword1 "class", Pretty.brk 1,
          Name_Space.pretty ctxt class_space class, Pretty.str ":", Pretty.fbrk,
          Pretty.block [Pretty.str "supersort: ", prt_supersort class]] @
          (case try (Axclass.get_info thy) class of
            NONE => []
          | SOME {params, ...} =>
              [Pretty.fbrk, Pretty.big_list "parameters:" (map prt_param params)]) @
          (case Symtab.lookup arities class of
            NONE => []
          | SOME ars =>
              [Pretty.fbrk, Pretty.big_list "instances:"
                (map (prt_arity class) (sort (Name_Space.extern_ord ctxt type_space) ars))]));
  in
    Sorts.all_classes algebra
    |> sort (Name_Space.extern_ord ctxt class_space)
    |> map prt_entry
    |> Pretty.writeln_chunks2
  end;


(* updaters *)

fun register class sups params base_sort base_morph export_morph
    some_axiom some_assm_intro of_class thy =
  let
    val operations = map (fn (v_ty as (_, ty), (c, _)) =>
      (c, (class, ((ty, Free v_ty), false)))) params;
    val add_class = Graph.new_node (class,
        make_class_data (((map o apply2) fst params, base_sort,
          base_morph, export_morph, Option.map Thm.trim_context some_assm_intro,
          Thm.trim_context of_class, Option.map Thm.trim_context some_axiom), ([], operations)))
      #> fold (curry Graph.add_edge class) sups;
  in Class_Data.map add_class thy end;

fun activate_defs class thms thy =
  (case Element.eq_morphism thy thms of
    SOME eq_morph => fold (fn cls => fn thy =>
      Context.theory_map (Locale.amend_registration (cls, base_morphism thy cls)
        (eq_morph, true) (export_morphism thy cls)) thy) (heritage thy [class]) thy
  | NONE => thy);

fun register_operation class (c, t) input_only thy =
  let
    val base_sort = base_sort thy class;
    val prep_typ = map_type_tfree
      (fn (v, sort) => if Name.aT = v
        then TFree (v, base_sort) else TVar ((v, 0), sort));
    val t' = map_types prep_typ t;
    val ty' = Term.fastype_of t';
  in
    thy
    |> (Class_Data.map o Graph.map_node class o map_class_data o apsnd o apsnd)
        (cons (c, (class, ((ty', t'), input_only))))
  end;

fun register_def class def_thm thy =
  let
    val sym_thm = Thm.trim_context (Thm.symmetric def_thm)
  in
    thy
    |> (Class_Data.map o Graph.map_node class o map_class_data o apsnd o apfst)
        (cons sym_thm)
    |> activate_defs class [sym_thm]
  end;


(** classes and class target **)

(* class context syntax *)

fun make_rewrite t c_ty =
  let
    val (vs, t') = strip_abs t;
    val vts = map snd vs
      |> Name.invent_names Name.context Name.uu
      |> map (fn (v, T) => Var ((v, 0), T));
  in (betapplys (t, vts), betapplys (Const c_ty, vts)) end;

fun these_unchecks thy =
  these_operations thy
  #> map_filter (fn (c, (_, ((ty, t), input_only))) =>
    if input_only then NONE else SOME (make_rewrite t (c, ty)));

fun these_unchecks_reversed thy =
  these_operations thy
  #> map (fn (c, (_, ((ty, t), _))) => (Const (c, ty), t));

fun redeclare_const thy c =
  let val b = Long_Name.base_name c
  in Sign.intern_const thy b = c ? Variable.declare_const (b, c) end;

fun synchronize_class_syntax sort base_sort ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;
    val algebra = Sign.classes_of thy;
    val operations = these_operations thy sort;
    fun subst_class_typ sort = map_type_tfree (K (TVar ((Name.aT, 0), sort)));
    val primary_constraints =
      (map o apsnd) (subst_class_typ base_sort o fst o fst o snd) operations;
    val secondary_constraints =
      (map o apsnd) (fn (class, ((ty, _), _)) => subst_class_typ [class] ty) operations;
    fun improve (c, ty) =
      (case AList.lookup (op =) primary_constraints c of
        SOME ty' =>
          (case try (Type.raw_match (ty', ty)) Vartab.empty of
            SOME tyenv =>
              (case Vartab.lookup tyenv (Name.aT, 0) of
                SOME (_, ty' as TVar (vi, sort)) =>
                  if Type_Infer.is_param vi andalso Sorts.sort_le algebra (base_sort, sort)
                  then SOME (ty', TFree (Name.aT, base_sort))
                  else NONE
              | _ => NONE)
          | NONE => NONE)
      | NONE => NONE);
    fun subst (c, _) = Option.map (fst o snd) (AList.lookup (op =) operations c);
    val unchecks = these_unchecks thy sort;
  in
    ctxt
    |> fold (redeclare_const thy o fst) primary_constraints
    |> Overloading.map_improvable_syntax (K {primary_constraints = primary_constraints,
      secondary_constraints = secondary_constraints, improve = improve, subst = subst,
      no_subst_in_abbrev_mode = true, unchecks = unchecks})
    |> Overloading.set_primary_constraints
  end;

fun synchronize_class_syntax_target class lthy =
  lthy
  |> Local_Theory.map_contexts
      (K (synchronize_class_syntax [class] (base_sort (Proof_Context.theory_of lthy) class)));

fun redeclare_operations thy sort =
  fold (redeclare_const thy o fst) (these_operations thy sort);

fun begin sort base_sort ctxt =
  ctxt
  |> Variable.declare_term
      (Logic.mk_type (TFree (Name.aT, base_sort)))
  |> synchronize_class_syntax sort base_sort
  |> Overloading.activate_improvable_syntax;

fun init class thy =
  thy
  |> Locale.init class
  |> begin [class] (base_sort thy class);


(* class target *)

val class_prefix = Logic.const_of_class o Long_Name.base_name;

fun guess_morphism_identity (b, rhs) phi1 phi2 =
  let
    (*FIXME proper concept to identify morphism instead of educated guess*)
    val name_of_binding = Name_Space.full_name Name_Space.global_naming;
    val n1 = (name_of_binding o Morphism.binding phi1) b;
    val n2 = (name_of_binding o Morphism.binding phi2) b;
    val rhs1 = Morphism.term phi1 rhs;
    val rhs2 = Morphism.term phi2 rhs;
  in n1 = n2 andalso Term.aconv_untyped (rhs1, rhs2) end;

fun target_const class phi0 prmode (b, rhs) lthy =
  let
    val export = Variable.export_morphism lthy (Local_Theory.target_of lthy);
    val guess_identity = guess_morphism_identity (b, rhs) export;
    val guess_canonical = guess_morphism_identity (b, rhs) (export $> phi0);
  in
    lthy
    |> Generic_Target.locale_target_const class
      (not o (guess_identity orf guess_canonical)) prmode ((b, NoSyn), rhs)
  end;

local

fun dangling_params_for lthy class (type_params, term_params) =
  let
    val class_param_names =
      map fst (these_params (Proof_Context.theory_of lthy) [class]);
    val dangling_term_params =
      subtract (fn (v, Free (w, _)) => v = w | _ => false) class_param_names term_params;
  in (type_params, dangling_term_params) end;

fun global_def (b, eq) thy =
  thy
  |> Thm.add_def_global false false (b, eq)
  |>> (Thm.varifyT_global o snd)
  |-> (fn def_thm => Global_Theory.store_thm (b, def_thm)
      #> snd
      #> pair def_thm);

fun canonical_const class phi dangling_params ((b, mx), rhs) thy =
  let
    val b_def = Binding.suffix_name "_dict" b;
    val c = Sign.full_name thy b;
    val ty = map Term.fastype_of dangling_params ---> Term.fastype_of rhs;
    val def_eq = Logic.mk_equals (list_comb (Const (c, ty), dangling_params), rhs)
      |> map_types Type.strip_sorts;
  in
    thy
    |> Sign.declare_const_global ((b, Type.strip_sorts ty), mx)
    |> snd
    |> global_def (b_def, def_eq)
    |-> (fn def_thm => register_def class def_thm)
    |> null dangling_params ? register_operation class (c, rhs) false
    |> Sign.add_const_constraint (c, SOME ty)
  end;

in

fun const class ((b, mx), lhs) params lthy =
  let
    val phi = morphism (Proof_Context.theory_of lthy) class;
    val dangling_params = map (Morphism.term phi) (uncurry append (dangling_params_for lthy class params));
  in
    lthy
    |> target_const class phi Syntax.mode_default (b, lhs)
    |> Local_Theory.raw_theory (canonical_const class phi dangling_params
         ((Morphism.binding phi b, if null dangling_params then mx else NoSyn), Morphism.term phi lhs))
    |> Generic_Target.standard_const (fn (this, other) => other <> 0 andalso this <> other)
         Syntax.mode_default ((b, if null dangling_params then NoSyn else mx), lhs)
    |> synchronize_class_syntax_target class
  end;

end;

local

fun canonical_abbrev class phi prmode with_syntax ((b, mx), rhs) thy =
  let
    val c = Sign.full_name thy b;
    val constrain = map_atyps (fn T as TFree (v, _) =>
      if v = Name.aT then TFree (v, [class]) else T | T => T);
    val rhs' = map_types constrain rhs;
  in
    thy
    |> Sign.add_abbrev (#1 prmode) (b, Logic.varify_types_global rhs')
    |> snd
    |> with_syntax ? Sign.notation true prmode [(Const (c, fastype_of rhs), mx)]
    |> with_syntax ? register_operation class (c, rhs)
        (#1 prmode = Print_Mode.input)
    |> Sign.add_const_constraint (c, SOME (fastype_of rhs'))
  end;

fun canonical_abbrev_target class phi prmode ((b, mx), rhs) lthy =
  let
    val thy = Proof_Context.theory_of lthy;
    val preprocess = perhaps (try (Pattern.rewrite_term_top thy (these_unchecks thy [class]) []));
    val (global_rhs, (extra_tfrees, (type_params, term_params))) =
      Generic_Target.export_abbrev lthy preprocess rhs;
    val mx' = Generic_Target.check_mixfix_global (b, null term_params) mx;
  in
    lthy
    |> Local_Theory.raw_theory (canonical_abbrev class phi prmode (null term_params)
        ((Morphism.binding phi b, mx'), Logic.unvarify_types_global global_rhs))
  end;

fun further_abbrev_target class phi prmode (b, mx) rhs params =
  Generic_Target.background_abbrev (b, rhs) (snd params)
  #-> (fn (lhs, _) => target_const class phi prmode (b, lhs)
    #> Generic_Target.standard_const (fn (this, other) => other <> 0 andalso this <> other) prmode ((b, mx), lhs))

in

fun abbrev class prmode ((b, mx), rhs) lthy =
  let
    val thy = Proof_Context.theory_of lthy;
    val phi = morphism thy class;
    val rhs_generic =
      perhaps (try (Pattern.rewrite_term_top thy (these_unchecks_reversed thy [class]) [])) rhs;
  in
    lthy
    |> canonical_abbrev_target class phi prmode ((b, mx), rhs)
    |> Generic_Target.abbrev (further_abbrev_target class phi) prmode ((b, mx), rhs_generic)
    ||> synchronize_class_syntax_target class
  end;

end;


(* subclasses *)

fun register_subclass (sub, sup) some_dep_morph some_witn export lthy =
  let
    val thy = Proof_Context.theory_of lthy;
    val intros = (snd o rules thy) sup :: map_filter I
      [Option.map (Drule.export_without_context_open o Element.conclude_witness lthy) some_witn,
        (fst o rules thy) sub];
    val classrel =
      Goal.prove_sorry_global thy [] [] (Logic.mk_classrel (sub, sup))
        (fn {context = ctxt, ...} => EVERY (map (TRYALL o resolve_tac ctxt o single) intros));
    val diff_sort = Sign.complete_sort thy [sup]
      |> subtract (op =) (Sign.complete_sort thy [sub])
      |> filter (is_class thy);
    fun add_dependency some_wit = case some_dep_morph of
        SOME dep_morph => Generic_Target.locale_dependency sub
          (sup, dep_morph $> Element.satisfy_morphism (the_list some_witn)) NONE export
      | NONE => I;
  in
    lthy
    |> Local_Theory.raw_theory
      (Axclass.add_classrel classrel
      #> Class_Data.map (Graph.add_edge (sub, sup))
      #> activate_defs sub (these_defs thy diff_sort))
    |> add_dependency some_witn
    |> synchronize_class_syntax_target sub
  end;

local

fun gen_classrel mk_prop classrel thy =
  let
    fun after_qed results =
      Proof_Context.background_theory ((fold o fold) Axclass.add_classrel results);
  in
    thy
    |> Proof_Context.init_global
    |> Proof.theorem NONE after_qed [[(mk_prop thy classrel, [])]]
  end;

in

val classrel =
  gen_classrel (Logic.mk_classrel oo Axclass.cert_classrel);
val classrel_cmd =
  gen_classrel (Logic.mk_classrel oo Axclass.read_classrel);

end; (*local*)


(** instantiation target **)

(* bookkeeping *)

datatype instantiation = Instantiation of {
  arities: string list * (string * sort) list * sort,
  params: ((string * string) * (string * typ)) list
    (*(instantiation parameter, type constructor), (local instantiation parameter, typ)*)
}

fun make_instantiation (arities, params) =
  Instantiation {arities = arities, params = params};

val empty_instantiation = make_instantiation (([], [], []), []);

structure Instantiation = Proof_Data
(
  type T = instantiation;
  fun init _ = empty_instantiation;
);

val get_instantiation =
  (fn Instantiation data => data) o Instantiation.get o Local_Theory.target_of;

fun map_instantiation f =
  (Local_Theory.target o Instantiation.map)
    (fn Instantiation {arities, params} => make_instantiation (f (arities, params)));

fun the_instantiation lthy =
  (case get_instantiation lthy of
    {arities = ([], [], []), ...} => error "No instantiation target"
  | data => data);

val instantiation_params = #params o get_instantiation;

fun instantiation_param lthy b = instantiation_params lthy
  |> find_first (fn (_, (v, _)) => Binding.name_of b = v)
  |> Option.map (fst o fst);

fun read_multi_arity thy (raw_tycos, raw_sorts, raw_sort) =
  let
    val ctxt = Proof_Context.init_global thy;
    val all_arities = map (fn raw_tyco => Proof_Context.read_arity ctxt
      (raw_tyco, raw_sorts, raw_sort)) raw_tycos;
    val tycos = map #1 all_arities;
    val (_, sorts, sort) = hd all_arities;
    val vs = Name.invent_names Name.context Name.aT sorts;
  in (tycos, vs, sort) end;


(* syntax *)

fun synchronize_inst_syntax ctxt =
  let
    val Instantiation {params, ...} = Instantiation.get ctxt;

    val lookup_inst_param = Axclass.lookup_inst_param
      (Sign.consts_of (Proof_Context.theory_of ctxt)) params;
    fun subst (c, ty) =
      (case lookup_inst_param (c, ty) of
        SOME (v_ty as (_, ty)) => SOME (ty, Free v_ty)
      | NONE => NONE);
    val unchecks =
      map (fn ((c, _), v_ty as (_, ty)) => (Free v_ty, Const (c, ty))) params;
  in
    ctxt
    |> Overloading.map_improvable_syntax (fn {primary_constraints, improve, ...} =>
      {primary_constraints = primary_constraints, secondary_constraints = [],
        improve = improve, subst = subst, no_subst_in_abbrev_mode = false,
        unchecks = unchecks})
  end;

fun resort_terms ctxt algebra consts constraints ts =
  let
    fun matchings (Const (c_ty as (c, _))) =
          (case constraints c of
            NONE => I
          | SOME sorts =>
              fold2 (curry (Sorts.meet_sort algebra)) (Consts.typargs consts c_ty) sorts)
      | matchings _ = I;
    val tvartab = (fold o fold_aterms) matchings ts Vartab.empty
      handle Sorts.CLASS_ERROR e => error (Sorts.class_error (Context.Proof ctxt) e);
    val inst = map_type_tvar
      (fn (vi, sort) => TVar (vi, the_default sort (Vartab.lookup tvartab vi)));
  in if Vartab.is_empty tvartab then ts else (map o map_types) inst ts end;


(* target *)

fun define_overloaded (c, U) b (b_def, rhs) lthy =
  let
    val name = Binding.name_of b;
    val pos = Binding.pos_of b;
    val _ =
      if Context_Position.is_reported lthy pos then
        Position.report_text pos Markup.class_parameter
          (Pretty.string_of
            (Pretty.block [Pretty.keyword1 "class", Pretty.brk 1,
                Pretty.str "parameter", Pretty.brk 1, pretty_param lthy (c, U)]))
      else ();
  in
    lthy |> Local_Theory.background_theory_result
      (Axclass.declare_overloaded (c, U) ##>> Axclass.define_overloaded b_def (c, rhs))
    ||> (map_instantiation o apsnd) (filter_out (fn (_, (v', _)) => v' = name))
    ||> Local_Theory.map_contexts (K synchronize_inst_syntax)
  end;

fun foundation (((b, U), mx), (b_def, rhs)) params lthy =
  (case instantiation_param lthy b of
    SOME c =>
      if Mixfix.is_empty mx then lthy |> define_overloaded (c, U) b (b_def, rhs)
      else error ("Illegal mixfix syntax for overloaded constant " ^ quote c)
  | NONE => lthy |> Generic_Target.theory_target_foundation (((b, U), mx), (b_def, rhs)) params);

fun pretty lthy =
  let
    val {arities = (tycos, vs, sort), params} = the_instantiation lthy;
    fun pr_arity tyco = Syntax.pretty_arity lthy (tyco, map snd vs, sort);
    fun pr_param ((c, _), (v, ty)) =
      Pretty.block (Pretty.breaks
        [Pretty.str v, Pretty.str "==", Proof_Context.pretty_const lthy c,
          Pretty.str "::", Syntax.pretty_typ lthy ty]);
  in
    [Pretty.block
      (Pretty.fbreaks (Pretty.keyword1 "instantiation" :: map pr_arity tycos @ map pr_param params))]
  end;

fun conclude lthy =
  let
    val (tycos, vs, sort) = #arities (the_instantiation lthy);
    val thy = Proof_Context.theory_of lthy;
    val _ = tycos |> List.app (fn tyco =>
      if Sign.of_sort thy (Type (tyco, map TFree vs), sort) then ()
      else error ("Missing instance proof for type " ^ quote (Proof_Context.markup_type lthy tyco)));
  in lthy end;

fun instantiation (tycos, vs, sort) thy =
  let
    val _ = if null tycos then error "At least one arity must be given" else ();
    val class_params = these_params thy (filter (can (Axclass.get_info thy)) sort);
    fun get_param tyco (param, (_, (c, ty))) =
      if can (Axclass.param_of_inst thy) (c, tyco)
      then NONE else SOME ((c, tyco),
        (param ^ "_" ^ Long_Name.base_name tyco, map_atyps (K (Type (tyco, map TFree vs))) ty));
    val params = map_product get_param tycos class_params |> map_filter I;
    val _ = if null params andalso forall (fn tyco => can (Sign.arity_sorts thy tyco) sort) tycos
      then error "No parameters and no pending instance proof obligations in instantiation."
      else ();
    val primary_constraints = map (apsnd
      (map_atyps (K (TVar ((Name.aT, 0), [])))) o snd o snd) class_params;
    val algebra = Sign.classes_of thy
      |> fold (fn tyco => Sorts.add_arities (Context.Theory thy)
            (tyco, map (fn class => (class, map snd vs)) sort)) tycos;
    val consts = Sign.consts_of thy;
    val improve_constraints = AList.lookup (op =)
      (map (fn (_, (class, (c, _))) => (c, [[class]])) class_params);
    fun resort_check ctxt ts = resort_terms ctxt algebra consts improve_constraints ts;
    val lookup_inst_param = Axclass.lookup_inst_param consts params;
    fun improve (c, ty) =
      (case lookup_inst_param (c, ty) of
        SOME (_, ty') => if Sign.typ_instance thy (ty', ty) then SOME (ty, ty') else NONE
      | NONE => NONE);
  in
    thy
    |> Sign.change_begin
    |> Proof_Context.init_global
    |> Instantiation.put (make_instantiation ((tycos, vs, sort), params))
    |> fold (Variable.declare_typ o TFree) vs
    |> fold (Variable.declare_names o Free o snd) params
    |> (Overloading.map_improvable_syntax) (K {primary_constraints = primary_constraints,
      secondary_constraints = [], improve = improve, subst = K NONE,
      no_subst_in_abbrev_mode = false, unchecks = []})
    |> Overloading.activate_improvable_syntax
    |> Context.proof_map (Syntax_Phases.term_check 0 "resorting" resort_check)
    |> synchronize_inst_syntax
    |> Local_Theory.init (Sign.naming_of thy)
       {define = Generic_Target.define foundation,
        notes = Generic_Target.notes Generic_Target.theory_target_notes,
        abbrev = Generic_Target.abbrev Generic_Target.theory_target_abbrev,
        declaration = K Generic_Target.theory_declaration,
        theory_registration = Generic_Target.theory_registration,
        locale_dependency = fn _ => error "Not possible in instantiation target",
        pretty = pretty,
        exit = conclude #> Local_Theory.target_of #> Sign.change_end_local}
  end;

fun instantiation_cmd arities thy =
  instantiation (read_multi_arity thy arities) thy;

fun gen_instantiation_instance do_proof after_qed lthy =
  let
    val (tycos, vs, sort) = (#arities o the_instantiation) lthy;
    val arities_proof = maps (fn tyco => Logic.mk_arities (tyco, map snd vs, sort)) tycos;
    fun after_qed' results =
      Local_Theory.background_theory (fold (Axclass.add_arity o Thm.varifyT_global) results)
      #> after_qed;
  in
    lthy
    |> do_proof after_qed' arities_proof
  end;

val instantiation_instance = gen_instantiation_instance (fn after_qed => fn ts =>
  Proof.theorem NONE (after_qed o map the_single) (map (fn t => [(t, [])]) ts));

fun prove_instantiation_instance tac = gen_instantiation_instance (fn after_qed =>
  fn ts => fn lthy => after_qed (map (fn t => Goal.prove lthy [] [] t
    (fn {context, ...} => tac context)) ts) lthy) I;

fun prove_instantiation_exit tac = prove_instantiation_instance tac
  #> Local_Theory.exit_global;

fun prove_instantiation_exit_result f tac x lthy =
  let
    val morph = Proof_Context.export_morphism lthy
      (Proof_Context.init_global (Proof_Context.theory_of lthy));
    val y = f morph x;
  in
    lthy
    |> prove_instantiation_exit (fn ctxt => tac ctxt y)
    |> pair y
  end;


(* simplified instantiation interface with no class parameter *)

fun instance_arity_cmd raw_arities thy =
  let
    val (tycos, vs, sort) = read_multi_arity thy raw_arities;
    val sorts = map snd vs;
    val arities = maps (fn tyco => Logic.mk_arities (tyco, sorts, sort)) tycos;
    fun after_qed results =
      Proof_Context.background_theory ((fold o fold) Axclass.add_arity results);
  in
    thy
    |> Proof_Context.init_global
    |> Proof.theorem NONE after_qed (map (fn t => [(t, [])]) arities)
  end;



(** tactics and methods **)

fun intro_classes_tac ctxt facts st =
  let
    val thy = Proof_Context.theory_of ctxt;
    val classes = Sign.all_classes thy;
    val class_trivs = map (Thm.class_triv thy) classes;
    val class_intros = map_filter (try (#intro o Axclass.get_info thy)) classes;
    val assm_intros = all_assm_intros thy;
  in
    Method.intros_tac ctxt (class_trivs @ class_intros @ assm_intros) facts st
  end;

fun standard_intro_classes_tac ctxt facts st =
  if null facts andalso not (Thm.no_prems st) then
    (intro_classes_tac ctxt [] ORELSE Locale.intro_locales_tac true ctxt []) st
  else no_tac st;

fun standard_tac ctxt facts =
  HEADGOAL (Method.some_rule_tac ctxt [] facts) ORELSE
  standard_intro_classes_tac ctxt facts;

val _ = Theory.setup
 (Method.setup @{binding intro_classes} (Scan.succeed (METHOD o intro_classes_tac))
    "back-chain introduction rules of classes" #>
  Method.setup @{binding standard} (Scan.succeed (METHOD o standard_tac))
    "standard proof step: Pure intro/elim rule or class introduction");



(** diagnostics **)

fun pretty_specification thy class =
  if is_class thy class then
    let
      val class_ctxt = init class thy;
      val prt_class = Name_Space.pretty class_ctxt (Proof_Context.class_space class_ctxt);

      val super_classes = Sign.minimize_sort thy (Sign.super_classes thy class);

      val fix_args =
        #params (Axclass.get_info thy class)
        |> map (fn (c, T) => (Binding.name (Long_Name.base_name c), SOME T, NoSyn));
      val fixes = if null fix_args then [] else [Element.Fixes fix_args];
      val assumes = Locale.hyp_spec_of thy class;

      val header =
        [Pretty.keyword1 "class", Pretty.brk 1, prt_class class, Pretty.str " =", Pretty.brk 1] @
        Pretty.separate " +" (map prt_class super_classes) @
        (if null super_classes then [] else [Pretty.str " +"]);
      val body =
        if null fixes andalso null assumes then []
        else
          maps (Element.pretty_ctxt_no_attribs class_ctxt) (fixes @ assumes)
          |> maps (fn prt => [Pretty.fbrk, prt]);
    in if null body then [] else [Pretty.block (header @ body)] end
  else [];

end;
