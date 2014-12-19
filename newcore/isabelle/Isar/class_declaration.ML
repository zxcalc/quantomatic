(*  Title:      Pure/Isar/class_declaration.ML
    Author:     Florian Haftmann, TU Muenchen

Declaring classes and subclass relations.
*)

signature CLASS_DECLARATION =
sig
  val class: binding -> class list ->
    Element.context_i list -> theory -> string * local_theory
  val class_cmd: binding -> xstring list ->
    Element.context list -> theory -> string * local_theory
  val prove_subclass: tactic -> class ->
    local_theory -> local_theory
  val subclass: class -> local_theory -> Proof.state
  val subclass_cmd: xstring -> local_theory -> Proof.state
end;

structure Class_Declaration: CLASS_DECLARATION =
struct

(** class definitions **)

local

(* calculating class-related rules including canonical interpretation *)

fun calculate thy class sups base_sort param_map assm_axiom =
  let
    val empty_ctxt = Proof_Context.init_global thy;

    (* instantiation of canonical interpretation *)
    val aT = TFree (Name.aT, base_sort);
    val param_map_const = (map o apsnd) Const param_map;
    val param_map_inst = (map o apsnd)
      (Const o apsnd (map_atyps (K aT))) param_map;
    val const_morph = Element.inst_morphism thy
      (Symtab.empty, Symtab.make param_map_inst);
    val typ_morph = Element.inst_morphism thy
      (Symtab.empty |> Symtab.update (Name.aT, TFree (Name.aT, [class])), Symtab.empty);
    val (([raw_props], [(_, raw_inst_morph)], export_morph), _) = empty_ctxt
      |> Expression.cert_goal_expression ([(class, (("", false),
           Expression.Named param_map_const))], []);
    val (props, inst_morph) =
      if null param_map
      then (raw_props |> map (Morphism.term typ_morph),
        raw_inst_morph $> typ_morph)
      else (raw_props, raw_inst_morph); (*FIXME proper handling in
        locale.ML / expression.ML would be desirable*)

    (* witness for canonical interpretation *)
    val some_prop = try the_single props;
    val some_witn = Option.map (fn prop =>
      let
        val sup_axioms = map_filter (fst o Class.rules thy) sups;
        val loc_intro_tac =
          (case Locale.intros_of thy class of
            (_, NONE) => all_tac
          | (_, SOME intro) => ALLGOALS (rtac intro));
        val tac = loc_intro_tac
          THEN ALLGOALS (Proof_Context.fact_tac empty_ctxt (sup_axioms @ the_list assm_axiom));
      in Element.prove_witness empty_ctxt prop tac end) some_prop;
    val some_axiom = Option.map (Element.conclude_witness empty_ctxt) some_witn;

    (* canonical interpretation *)
    val base_morph = inst_morph
      $> Morphism.binding_morphism "class_binding" (Binding.prefix false (Class.class_prefix class))
      $> Element.satisfy_morphism (the_list some_witn);
    val eq_morph = Element.eq_morphism thy (Class.these_defs thy sups);

    (* assm_intro *)
    fun prove_assm_intro thm =
      let
        val ((_, [thm']), _) = Variable.import true [thm] empty_ctxt;
        val const_eq_morph =
          (case eq_morph of
            SOME eq_morph => const_morph $> eq_morph
          | NONE => const_morph);
        val thm'' = Morphism.thm const_eq_morph thm';
      in
        Goal.prove_sorry_global thy [] [] (Thm.prop_of thm'')
          (fn {context = ctxt, ...} => ALLGOALS (Proof_Context.fact_tac ctxt [thm'']))
      end;
    val some_assm_intro = Option.map prove_assm_intro (fst (Locale.intros_of thy class));

    (* of_class *)
    val of_class_prop_concl = Logic.mk_of_class (aT, class);
    val of_class_prop =
      (case some_prop of
        NONE => of_class_prop_concl
      | SOME prop => Logic.mk_implies (Morphism.term const_morph
          ((map_types o map_atyps) (K aT) prop), of_class_prop_concl));
    val sup_of_classes = map (snd o Class.rules thy) sups;
    val loc_axiom_intros = map Drule.export_without_context_open (Locale.axioms_of thy class);
    val axclass_intro = #intro (Axclass.get_info thy class);
    val base_sort_trivs = Thm.of_sort (Thm.ctyp_of thy aT, base_sort);
    val tac =
      REPEAT (SOMEGOAL
        (match_tac (axclass_intro :: sup_of_classes @ loc_axiom_intros @ base_sort_trivs)
          ORELSE' assume_tac));
    val of_class = Goal.prove_sorry_global thy [] [] of_class_prop (K tac);

  in (base_morph, eq_morph, export_morph, some_axiom, some_assm_intro, of_class) end;


(* reading and processing class specifications *)

fun prep_class_elems prep_decl thy sups raw_elems =
  let

    (* user space type system: only permits 'a type variable, improves towards 'a *)
    val algebra = Sign.classes_of thy;
    val inter_sort = curry (Sorts.inter_sort algebra);
    val proto_base_sort =
      if null sups then Sign.defaultS thy
      else fold inter_sort (map (Class.base_sort thy) sups) [];
    val base_constraints = (map o apsnd)
      (map_type_tfree (K (TVar ((Name.aT, 0), proto_base_sort))) o fst o snd)
        (Class.these_operations thy sups);
    fun singleton_fixate tms =
      let
        val tfrees = fold Term.add_tfrees tms [];
        val inferred_sort =
          (fold o fold_types o fold_atyps) (fn TVar (_, S) => inter_sort S | _ => I) tms [];
        val fixate_sort =
          (case tfrees of
            [] => inferred_sort
          | [(a, S)] =>
              if a <> Name.aT then
                error ("No type variable other than " ^ Name.aT ^ " allowed in class specification")
              else if Sorts.sort_le algebra (S, inferred_sort) then S
              else
                error ("Type inference imposes additional sort constraint " ^
                  Syntax.string_of_sort_global thy inferred_sort ^
                  " of type parameter " ^ Name.aT ^ " of sort " ^
                  Syntax.string_of_sort_global thy S)
          | _ => error "Multiple type variables in class specification");
        val fixateT = TFree (Name.aT, fixate_sort);
      in
        (map o map_types o map_atyps)
          (fn T as TVar (xi, _) => if Type_Infer.is_param xi then fixateT else T | T => T) tms
      end;
    fun after_infer_fixate tms =
      let
        val fixate_sort =
          (fold o fold_types o fold_atyps)
            (fn TVar (xi, S) => if Type_Infer.is_param xi then inter_sort S else I | _ => I) tms [];
      in
        (map o map_types o map_atyps)
          (fn T as TVar (xi, _) =>
              if Type_Infer.is_param xi then Type_Infer.param 0 (Name.aT, fixate_sort) else T
            | T => T) tms
      end;

    (* preprocessing elements, retrieving base sort from type-checked elements *)
    val raw_supexpr =
      (map (fn sup => (sup, (("", false), Expression.Positional []))) sups, []);
    val init_class_body =
      fold (Proof_Context.add_const_constraint o apsnd SOME) base_constraints
      #> Class.redeclare_operations thy sups
      #> Context.proof_map (Syntax_Phases.term_check 0 "singleton_fixate" (K singleton_fixate));
    val ((raw_supparams, _, raw_inferred_elems, _), _) =
      Proof_Context.init_global thy
      |> Context.proof_map (Syntax_Phases.term_check 0 "after_infer_fixate" (K after_infer_fixate))
      |> prep_decl raw_supexpr init_class_body raw_elems;
    fun filter_element (Element.Fixes []) = NONE
      | filter_element (e as Element.Fixes _) = SOME e
      | filter_element (Element.Constrains []) = NONE
      | filter_element (e as Element.Constrains _) = SOME e
      | filter_element (Element.Assumes []) = NONE
      | filter_element (e as Element.Assumes _) = SOME e
      | filter_element (Element.Defines _) =
          error ("\"defines\" element not allowed in class specification.")
      | filter_element (Element.Notes _) =
          error ("\"notes\" element not allowed in class specification.");
    val inferred_elems = map_filter filter_element raw_inferred_elems;
    fun fold_element_types f (Element.Fixes fxs) = fold (fn (_, SOME T, _) => f T) fxs
      | fold_element_types f (Element.Constrains cnstrs) = fold (f o snd) cnstrs
      | fold_element_types f (Element.Assumes assms) = fold (fold (fn (t, ts) =>
          fold_types f t #> (fold o fold_types) f ts) o snd) assms;
    val base_sort =
      if null inferred_elems then proto_base_sort
      else
        (case (fold o fold_element_types) Term.add_tfreesT inferred_elems [] of
          [] => error "No type variable in class specification"
        | [(_, sort)] => sort
        | _ => error "Multiple type variables in class specification");
    val supparams = map (fn ((c, T), _) =>
      (c, map_atyps (K (TFree (Name.aT, base_sort))) T)) raw_supparams;
    val supparam_names = map fst supparams;
    fun mk_param ((c, _), _) = Free (c, (the o AList.lookup (op =) supparams) c);
    val supexpr = (map (fn sup => (sup, (("", false),
      Expression.Positional (map (SOME o mk_param) (Locale.params_of thy sup))))) sups,
        map (fn (c, T) => (Binding.name c, SOME T, NoSyn)) supparams);

  in (base_sort, supparam_names, supexpr, inferred_elems) end;

val cert_class_elems = prep_class_elems Expression.cert_declaration;
val read_class_elems = prep_class_elems Expression.cert_read_declaration;

fun prep_class_spec prep_class prep_class_elems thy raw_supclasses raw_elems =
  let
    val thy_ctxt = Proof_Context.init_global thy;

    (* prepare import *)
    val inter_sort = curry (Sorts.inter_sort (Sign.classes_of thy));
    val sups = Sign.minimize_sort thy (map (prep_class thy_ctxt) raw_supclasses);
    val _ =
      (case filter_out (Class.is_class thy) sups of
        [] => ()
      | no_classes => error ("No (proper) classes: " ^ commas_quote no_classes));
    val raw_supparams = (map o apsnd) (snd o snd) (Class.these_params thy sups);
    val raw_supparam_names = map fst raw_supparams;
    val _ =
      if has_duplicates (op =) raw_supparam_names then
        error ("Duplicate parameter(s) in superclasses: " ^
          (commas_quote (duplicates (op =) raw_supparam_names)))
      else ();

    (* infer types and base sort *)
    val (base_sort, supparam_names, supexpr, inferred_elems) = prep_class_elems thy sups raw_elems;
    val sup_sort = inter_sort base_sort sups;

    (* process elements as class specification *)
    val class_ctxt = Class.begin sups base_sort thy_ctxt;
    val ((_, _, syntax_elems, _), _) = class_ctxt
      |> Expression.cert_declaration supexpr I inferred_elems;
    fun check_vars e vs =
      if null vs then
        error ("No type variable in part of specification element " ^
          Pretty.string_of (Pretty.chunks (Element.pretty_ctxt class_ctxt e)))
      else ();
    fun check_element (e as Element.Fixes fxs) =
          List.app (fn (_, SOME T, _) => check_vars e (Term.add_tfreesT T [])) fxs
      | check_element (e as Element.Assumes assms) =
          List.app (fn (_, ts_pss) =>
            List.app (fn (t, _) => check_vars e (Term.add_tfrees t [])) ts_pss) assms
      | check_element _ = ();
    val _ = List.app check_element syntax_elems;
    fun fork_syn (Element.Fixes xs) =
          fold_map (fn (c, ty, syn) => cons (c, syn) #> pair (c, ty, NoSyn)) xs
          #>> Element.Fixes
      | fork_syn x = pair x;
    val (elems, global_syntax) = fold_map fork_syn syntax_elems [];

  in (((sups, supparam_names), (sup_sort, base_sort, supexpr)), (elems, global_syntax)) end;

val cert_class_spec = prep_class_spec (K I) cert_class_elems;
val read_class_spec = prep_class_spec Proof_Context.read_class read_class_elems;


(* class establishment *)

fun add_consts class base_sort sups supparam_names global_syntax thy =
  let
    (*FIXME simplify*)
    val supconsts = supparam_names
      |> AList.make (snd o the o AList.lookup (op =) (Class.these_params thy sups))
      |> (map o apsnd o apsnd o map_atyps o K o TFree) (Name.aT, [class]);
    val all_params = Locale.params_of thy class;
    val raw_params = (snd o chop (length supparam_names)) all_params;
    fun add_const ((raw_c, raw_ty), _) thy =
      let
        val b = Binding.name raw_c;
        val c = Sign.full_name thy b;
        val ty = map_atyps (K (TFree (Name.aT, base_sort))) raw_ty;
        val ty0 = Type.strip_sorts ty;
        val ty' = map_atyps (K (TFree (Name.aT, [class]))) ty0;
        val syn = (the_default NoSyn o AList.lookup Binding.eq_name global_syntax) b;
      in
        thy
        |> Sign.declare_const_global ((b, ty0), syn)
        |> snd
        |> pair ((Variable.check_name b, ty), (c, ty'))
      end;
  in
    thy
    |> Sign.add_path (Class.class_prefix class)
    |> fold_map add_const raw_params
    ||> Sign.restore_naming thy
    |-> (fn params => pair (supconsts @ (map o apfst) fst params, params))
  end;

fun adjungate_axclass bname class base_sort sups supsort supparam_names global_syntax thy =
  let
    (*FIXME simplify*)
    fun globalize param_map = map_aterms
      (fn Free (v, ty) => Const ((fst o the o AList.lookup (op =) param_map) v, ty)
        | t => t);
    val raw_pred = Locale.intros_of thy class
      |> fst
      |> Option.map (Logic.unvarify_global o Logic.strip_imp_concl o Thm.prop_of);
    fun get_axiom thy =
      (case #axioms (Axclass.get_info thy class) of
         [] => NONE
      | [thm] => SOME thm);
  in
    thy
    |> add_consts class base_sort sups supparam_names global_syntax
    |-> (fn (param_map, params) => Axclass.define_class (bname, supsort)
          (map (fst o snd) params)
          [(Thm.empty_binding, Option.map (globalize param_map) raw_pred |> the_list)]
    #> snd
    #> `get_axiom
    #-> (fn assm_axiom => fold (Sign.add_const_constraint o apsnd SOME o snd) params
    #> pair (param_map, params, assm_axiom)))
  end;

fun gen_class prep_class_spec b raw_supclasses raw_elems thy =
  let
    val class = Sign.full_name thy b;
    val prefix = Binding.qualify true "class";
    val (((sups, supparam_names), (supsort, base_sort, supexpr)), (elems, global_syntax)) =
      prep_class_spec thy raw_supclasses raw_elems;
  in
    thy
    |> Expression.add_locale b (prefix b) supexpr elems
    |> snd |> Local_Theory.exit_global
    |> adjungate_axclass b class base_sort sups supsort supparam_names global_syntax
    |-> (fn (param_map, params, assm_axiom) =>
       `(fn thy => calculate thy class sups base_sort param_map assm_axiom)
    #-> (fn (base_morph, eq_morph, export_morph, some_axiom, some_assm_intro, of_class) =>
       Context.theory_map (Locale.add_registration (class, base_morph)
         (Option.map (rpair true) eq_morph) export_morph)
    #> Class.register class sups params base_sort base_morph export_morph some_axiom some_assm_intro of_class
    #> Global_Theory.store_thm (prefix (Binding.qualified_name (class ^ ".of_class.intro")), of_class)))
    |> snd
    |> Named_Target.init class
    |> pair class
  end;

in

val class = gen_class cert_class_spec;
val class_cmd = gen_class read_class_spec;

end; (*local*)



(** subclass relations **)

local

fun gen_subclass prep_class do_proof raw_sup lthy =
  let
    val thy = Proof_Context.theory_of lthy;
    val proto_sup = prep_class thy raw_sup;
    val proto_sub = case Named_Target.class_of lthy of
        SOME class => class
      | NONE => error "Not in a class target";
    val (sub, sup) = Axclass.cert_classrel thy (proto_sub, proto_sup);

    val expr = ([(sup, (("", false), Expression.Positional []))], []);
    val (([props], deps, export), goal_ctxt) =
      Expression.cert_goal_expression expr lthy;
    val some_prop = try the_single props;
    val some_dep_morph = try the_single (map snd deps);
    fun after_qed some_wit =
      Class.register_subclass (sub, sup) some_dep_morph some_wit export;
  in do_proof after_qed some_prop goal_ctxt end;

fun user_proof after_qed some_prop =
  Element.witness_proof (after_qed o try the_single o the_single)
    [the_list some_prop];

fun tactic_proof tac after_qed some_prop ctxt =
  after_qed (Option.map
    (fn prop => Element.prove_witness ctxt prop tac) some_prop) ctxt;

in

fun prove_subclass tac = gen_subclass (K I) (tactic_proof tac);

fun subclass x = gen_subclass (K I) user_proof x;
fun subclass_cmd x =
  gen_subclass (Proof_Context.read_class o Proof_Context.init_global) user_proof x;

end; (*local*)

end;
