(*  Title:      Pure/Isar/specification.ML
    Author:     Makarius

Derived local theory specifications --- with type-inference and
toplevel polymorphism.
*)

signature SPECIFICATION =
sig
  val read_props: string list -> (binding * string option * mixfix) list -> Proof.context ->
    term list * Proof.context
  val check_spec_open: (binding * typ option * mixfix) list ->
    (binding * typ option * mixfix) list -> term list -> term -> Proof.context ->
    ((binding * typ option * mixfix) list * string list * (string -> Position.T list) * term) *
    Proof.context
  val read_spec_open: (binding * string option * mixfix) list ->
    (binding * string option * mixfix) list -> string list -> string -> Proof.context ->
    ((binding * typ option * mixfix) list * string list * (string -> Position.T list) * term) *
    Proof.context
  type multi_specs =
    ((Attrib.binding * term) * term list * (binding * typ option * mixfix) list) list
  type multi_specs_cmd =
    ((Attrib.binding * string) * string list * (binding * string option * mixfix) list) list
  val check_multi_specs: (binding * typ option * mixfix) list -> multi_specs -> Proof.context ->
    (((binding * typ) * mixfix) list * (Attrib.binding * term) list) * Proof.context
  val read_multi_specs: (binding * string option * mixfix) list -> multi_specs_cmd -> Proof.context ->
    (((binding * typ) * mixfix) list * (Attrib.binding * term) list) * Proof.context
  val axiomatization: (binding * typ option * mixfix) list ->
    (binding * typ option * mixfix) list -> term list ->
    (Attrib.binding * term) list -> theory -> (term list * thm list) * theory
  val axiomatization_cmd: (binding * string option * mixfix) list ->
    (binding * string option * mixfix) list -> string list ->
    (Attrib.binding * string) list -> theory -> (term list * thm list) * theory
  val axiom: Attrib.binding * term -> theory -> thm * theory
  val definition: (binding * typ option * mixfix) option ->
    (binding * typ option * mixfix) list -> term list -> Attrib.binding * term ->
    local_theory -> (term * (string * thm)) * local_theory
  val definition': (binding * typ option * mixfix) option ->
    (binding * typ option * mixfix) list ->  term list -> Attrib.binding * term ->
    bool -> local_theory -> (term * (string * thm)) * local_theory
  val definition_cmd: (binding * string option * mixfix) option ->
    (binding * string option * mixfix) list -> string list -> Attrib.binding * string ->
    bool -> local_theory -> (term * (string * thm)) * local_theory
  val abbreviation: Syntax.mode -> (binding * typ option * mixfix) option ->
    (binding * typ option * mixfix) list -> term -> bool -> local_theory -> local_theory
  val abbreviation_cmd: Syntax.mode -> (binding * string option * mixfix) option ->
    (binding * string option * mixfix) list -> string -> bool -> local_theory -> local_theory
  val type_notation: bool -> Syntax.mode -> (typ * mixfix) list -> local_theory -> local_theory
  val type_notation_cmd: bool -> Syntax.mode -> (string * mixfix) list ->
    local_theory -> local_theory
  val notation: bool -> Syntax.mode -> (term * mixfix) list -> local_theory -> local_theory
  val notation_cmd: bool -> Syntax.mode -> (string * mixfix) list -> local_theory -> local_theory
  val theorems: string ->
    (Attrib.binding * Attrib.thms) list ->
    (binding * typ option * mixfix) list ->
    bool -> local_theory -> (string * thm list) list * local_theory
  val theorems_cmd: string ->
    (Attrib.binding * (Facts.ref * Token.src list) list) list ->
    (binding * string option * mixfix) list ->
    bool -> local_theory -> (string * thm list) list * local_theory
  val theorem: bool -> string -> Method.text option ->
    (thm list list -> local_theory -> local_theory) -> Attrib.binding ->
    string list -> Element.context_i list -> Element.statement_i ->
    bool -> local_theory -> Proof.state
  val theorem_cmd: bool -> string -> Method.text option ->
    (thm list list -> local_theory -> local_theory) -> Attrib.binding ->
    (xstring * Position.T) list -> Element.context list -> Element.statement ->
    bool -> local_theory -> Proof.state
  val schematic_theorem: bool -> string -> Method.text option ->
    (thm list list -> local_theory -> local_theory) -> Attrib.binding ->
    string list -> Element.context_i list -> Element.statement_i ->
    bool -> local_theory -> Proof.state
  val schematic_theorem_cmd: bool -> string -> Method.text option ->
    (thm list list -> local_theory -> local_theory) -> Attrib.binding ->
    (xstring * Position.T) list -> Element.context list -> Element.statement ->
    bool -> local_theory -> Proof.state
end;

structure Specification: SPECIFICATION =
struct

(* prepare propositions *)

fun read_props raw_props raw_fixes ctxt =
  let
    val (_, ctxt1) = ctxt |> Proof_Context.add_fixes_cmd raw_fixes;
    val props1 = map (Syntax.parse_prop ctxt1) raw_props;
    val (props2, ctxt2) = ctxt1 |> fold_map Variable.fix_dummy_patterns props1;
    val props3 = Syntax.check_props ctxt2 props2;
    val ctxt3 = ctxt2 |> fold Variable.declare_term props3;
  in (props3, ctxt3) end;


(* prepare specification *)

fun get_positions ctxt x =
  let
    fun get Cs (Const ("_type_constraint_", C) $ t) = get (C :: Cs) t
      | get Cs (Free (y, T)) =
          if x = y then
            map_filter Term_Position.decode_positionT
              (T :: map (Type.constraint_type ctxt) Cs)
          else []
      | get _ (t $ u) = get [] t @ get [] u
      | get _ (Abs (_, _, t)) = get [] t
      | get _ _ = [];
  in get [] end;

local

fun prep_decls prep_var raw_vars ctxt =
  let
    val (vars, ctxt') = fold_map prep_var raw_vars ctxt;
    val (xs, ctxt'') = ctxt'
      |> Context_Position.set_visible false
      |> Proof_Context.add_fixes vars
      ||> Context_Position.restore_visible ctxt';
    val _ =
      Context_Position.reports ctxt''
        (map (Binding.pos_of o #1) vars ~~
          map (Variable.markup_entity_def ctxt'' ##> Properties.remove Markup.kindN) xs);
  in ((vars, xs), ctxt'') end;

fun close_form ctxt ys prems concl =
  let
    val xs = rev (fold (Variable.add_free_names ctxt) (prems @ [concl]) (rev ys));

    val pos_props = Logic.strip_imp_concl concl :: Logic.strip_imp_prems concl @ prems;
    fun get_pos x = maps (get_positions ctxt x) pos_props;
    val _ = Context_Position.reports ctxt (maps (Syntax_Phases.reports_of_scope o get_pos) xs);
  in Logic.close_prop_constraint (Variable.default_type ctxt) (xs ~~ xs) prems concl end;

fun dummy_frees ctxt xs tss =
  let
    val names =
      Variable.names_of ((fold o fold) Variable.declare_term tss ctxt)
      |> fold Name.declare xs;
    val (tss', _) = (fold_map o fold_map) Term.free_dummy_patterns tss names;
  in tss' end;

fun prep_spec_open prep_var parse_prop raw_vars raw_params raw_prems raw_concl ctxt =
  let
    val ((vars, xs), vars_ctxt) = prep_decls prep_var raw_vars ctxt;
    val (ys, params_ctxt) = vars_ctxt |> fold_map prep_var raw_params |-> Proof_Context.add_fixes;

    val props =
      map (parse_prop params_ctxt) (raw_concl :: raw_prems)
      |> singleton (dummy_frees params_ctxt (xs @ ys));

    val concl :: prems = Syntax.check_props params_ctxt props;
    val spec = Logic.list_implies (prems, concl);
    val spec_ctxt = Variable.declare_term spec params_ctxt;

    fun get_pos x = maps (get_positions spec_ctxt x) props;
  in ((vars, xs, get_pos, spec), spec_ctxt) end;

fun prep_specs prep_var parse_prop prep_att raw_vars raw_specss ctxt =
  let
    val ((vars, xs), vars_ctxt) = prep_decls prep_var raw_vars ctxt;

    val propss0 =
      raw_specss |> map (fn ((_, raw_concl), raw_prems, raw_params) =>
        let val (ys, ctxt') = vars_ctxt |> fold_map prep_var raw_params |-> Proof_Context.add_fixes
        in (ys, map (pair ctxt') (raw_concl :: raw_prems)) end);
    val props =
      burrow (grouped 10 Par_List.map_independent (uncurry parse_prop)) (map #2 propss0)
      |> dummy_frees vars_ctxt xs
      |> map2 (fn (ys, _) => fn concl :: prems => close_form vars_ctxt ys prems concl) propss0;

    val specs = Syntax.check_props vars_ctxt props;
    val specs_ctxt = vars_ctxt |> fold Variable.declare_term specs;

    val ps = specs_ctxt |> fold_map Proof_Context.inferred_param xs |> fst;
    val params = map2 (fn (b, _, mx) => fn (_, T) => ((b, T), mx)) vars ps;
    val name_atts: Attrib.binding list =
      map (fn ((name, atts), _) => (name, map (prep_att ctxt) atts)) (map #1 raw_specss);
  in ((params, name_atts ~~ specs), specs_ctxt) end;

in

val check_spec_open = prep_spec_open Proof_Context.cert_var (K I);
val read_spec_open = prep_spec_open Proof_Context.read_var Syntax.parse_prop;

type multi_specs =
  ((Attrib.binding * term) * term list * (binding * typ option * mixfix) list) list;
type multi_specs_cmd =
  ((Attrib.binding * string) * string list * (binding * string option * mixfix) list) list;

fun check_multi_specs xs specs =
  prep_specs Proof_Context.cert_var (K I) (K I) xs specs;

fun read_multi_specs xs specs =
  prep_specs Proof_Context.read_var Syntax.parse_prop Attrib.check_src xs specs;

end;


(* axiomatization -- within global theory *)

fun gen_axioms prep_stmt prep_att raw_decls raw_fixes raw_prems raw_concls thy =
  let
    (*specification*)
    val ((vars, [prems, concls], _, _), vars_ctxt) =
      Proof_Context.init_global thy
      |> prep_stmt (raw_decls @ raw_fixes) ((map o map) (rpair []) [raw_prems, map snd raw_concls]);
    val (decls, fixes) = chop (length raw_decls) vars;

    val frees =
      rev ((fold o fold) (Variable.add_frees vars_ctxt) [prems, concls] [])
      |> map (fn (x, T) => (x, Free (x, T)));
    val close = Logic.close_prop (map #2 fixes @ frees) prems;
    val specs =
      map ((apsnd o map) (prep_att vars_ctxt) o fst) raw_concls ~~ map close concls;

    (*consts*)
    val (consts, consts_thy) = thy
      |> fold_map (fn ((b, _, mx), (_, t)) => Theory.specify_const ((b, Term.type_of t), mx)) decls;
    val subst = Term.subst_atomic (map (#2 o #2) decls ~~ consts);

    (*axioms*)
    val (axioms, axioms_thy) =
      (specs, consts_thy) |-> fold_map (fn ((b, atts), prop) =>
        Thm.add_axiom_global (b, subst prop) #>> (fn (_, th) => ((b, atts), [([th], [])])));

    (*facts*)
    val (facts, facts_lthy) = axioms_thy
      |> Named_Target.theory_init
      |> Spec_Rules.add Spec_Rules.Unknown (consts, maps (maps #1 o #2) axioms)
      |> Local_Theory.notes axioms;

  in ((consts, map (the_single o #2) facts), Local_Theory.exit_global facts_lthy) end;

val axiomatization = gen_axioms Proof_Context.cert_stmt (K I);
val axiomatization_cmd = gen_axioms Proof_Context.read_stmt Attrib.check_src;

fun axiom (b, ax) = axiomatization [] [] [] [(b, ax)] #>> (hd o snd);


(* definition *)

fun gen_def prep_spec prep_att raw_var raw_params raw_prems ((a, raw_atts), raw_spec) int lthy =
  let
    val atts = map (prep_att lthy) raw_atts;

    val ((vars, xs, get_pos, spec), _) = lthy
      |> prep_spec (the_list raw_var) raw_params raw_prems raw_spec;
    val (((x, T), rhs), prove) = Local_Defs.derived_def lthy get_pos {conditional = true} spec;
    val _ = Name.reject_internal (x, []);
    val (b, mx) =
      (case (vars, xs) of
        ([], []) => (Binding.make (x, (case get_pos x of [] => Position.none | p :: _ => p)), NoSyn)
      | ([(b, _, mx)], [y]) =>
          if x = y then (b, mx)
          else
            error ("Head of definition " ^ quote x ^ " differs from declaration " ^ quote y ^
              Position.here (Binding.pos_of b)));
    val name = Thm.def_binding_optional b a;
    val ((lhs, (_, raw_th)), lthy2) = lthy
      |> Local_Theory.define_internal ((b, mx), ((Binding.suffix_name "_raw" name, []), rhs));

    val th = prove lthy2 raw_th;
    val lthy3 = lthy2 |> Spec_Rules.add Spec_Rules.Equational ([lhs], [th]);

    val ([(def_name, [th'])], lthy4) = lthy3
      |> Local_Theory.notes [((name, Code.add_default_eqn_attrib Code.Equation :: atts), [([th], [])])];

    val lhs' = Morphism.term (Local_Theory.target_morphism lthy4) lhs;

    val _ =
      Proof_Display.print_consts int (Position.thread_data ()) lthy4
        (member (op =) (Term.add_frees lhs' [])) [(x, T)];
  in ((lhs, (def_name, th')), lthy4) end;

val definition' = gen_def check_spec_open (K I);
fun definition xs ys As B = definition' xs ys As B false;
val definition_cmd = gen_def read_spec_open Attrib.check_src;


(* abbreviation *)

fun gen_abbrev prep_spec mode raw_var raw_params raw_spec int lthy =
  let
    val lthy1 = lthy |> Proof_Context.set_syntax_mode mode;
    val ((vars, xs, get_pos, spec), _) = lthy
      |> Proof_Context.set_mode Proof_Context.mode_abbrev
      |> prep_spec (the_list raw_var) raw_params [] raw_spec;
    val ((x, T), rhs) = Local_Defs.abs_def (#2 (Local_Defs.cert_def lthy1 get_pos spec));
    val _ = Name.reject_internal (x, []);
    val (b, mx) =
      (case (vars, xs) of
        ([], []) => (Binding.make (x, (case get_pos x of [] => Position.none | p :: _ => p)), NoSyn)
      | ([(b, _, mx)], [y]) =>
          if x = y then (b, mx)
          else
            error ("Head of abbreviation " ^ quote x ^ " differs from declaration " ^ quote y ^
              Position.here (Binding.pos_of b)));
    val lthy2 = lthy1
      |> Local_Theory.abbrev mode ((b, mx), rhs) |> snd
      |> Proof_Context.restore_syntax_mode lthy;

    val _ = Proof_Display.print_consts int (Position.thread_data ()) lthy2 (K false) [(x, T)];
  in lthy2 end;

val abbreviation = gen_abbrev check_spec_open;
val abbreviation_cmd = gen_abbrev read_spec_open;


(* notation *)

local

fun gen_type_notation prep_type add mode args lthy =
  lthy |> Local_Theory.type_notation add mode (map (apfst (prep_type lthy)) args);

fun gen_notation prep_const add mode args lthy =
  lthy |> Local_Theory.notation add mode (map (apfst (prep_const lthy)) args);

in

val type_notation = gen_type_notation (K I);
val type_notation_cmd =
  gen_type_notation (Proof_Context.read_type_name {proper = true, strict = false});

val notation = gen_notation (K I);
val notation_cmd = gen_notation (Proof_Context.read_const {proper = false, strict = false});

end;


(* fact statements *)

local

fun gen_theorems prep_fact prep_att add_fixes
    kind raw_facts raw_fixes int lthy =
  let
    val facts = raw_facts |> map (fn ((name, atts), bs) =>
      ((name, map (prep_att lthy) atts),
        bs |> map (fn (b, more_atts) => (prep_fact lthy b, map (prep_att lthy) more_atts))));
    val (_, ctxt') = add_fixes raw_fixes lthy;

    val facts' = facts
      |> Attrib.partial_evaluation ctxt'
      |> Attrib.transform_facts (Proof_Context.export_morphism ctxt' lthy);
    val (res, lthy') = lthy |> Local_Theory.notes_kind kind facts';
    val _ = Proof_Display.print_results int (Position.thread_data ()) lthy' ((kind, ""), res);
  in (res, lthy') end;

in

val theorems = gen_theorems (K I) (K I) Proof_Context.add_fixes;
val theorems_cmd = gen_theorems Proof_Context.get_fact Attrib.check_src Proof_Context.add_fixes_cmd;

end;


(* complex goal statements *)

local

fun prep_statement prep_att prep_stmt raw_elems raw_stmt ctxt =
  let
    val (stmt, elems_ctxt) = prep_stmt raw_elems raw_stmt ctxt;
    val prems = Assumption.local_prems_of elems_ctxt ctxt;
    val stmt_ctxt = fold (fold (Variable.auto_fixes o fst) o snd) stmt elems_ctxt;
  in
    (case raw_stmt of
      Element.Shows _ =>
        let val stmt' = Attrib.map_specs (map prep_att) stmt
        in (([], prems, stmt', NONE), stmt_ctxt) end
    | Element.Obtains raw_obtains =>
        let
          val asms_ctxt = stmt_ctxt
            |> fold (fn ((name, _), asm) =>
                snd o Proof_Context.add_assms Assumption.assume_export
                  [((name, [Context_Rules.intro_query NONE]), asm)]) stmt;
          val that = Assumption.local_prems_of asms_ctxt stmt_ctxt;
          val ([(_, that')], that_ctxt) = asms_ctxt
            |> Proof_Context.set_stmt true
            |> Proof_Context.note_thmss "" [((Binding.name Auto_Bind.thatN, []), [(that, [])])]
            ||> Proof_Context.restore_stmt asms_ctxt;

          val stmt' = [(Binding.empty_atts, [(#2 (#1 (Obtain.obtain_thesis ctxt)), [])])];
        in ((Obtain.obtains_attribs raw_obtains, prems, stmt', SOME that'), that_ctxt) end)
  end;

fun gen_theorem schematic bundle_includes prep_att prep_stmt
    long kind before_qed after_qed (name, raw_atts) raw_includes raw_elems raw_concl int lthy =
  let
    val _ = Local_Theory.assert lthy;

    val elems = raw_elems |> map (Element.map_ctxt_attrib (prep_att lthy));
    val ((more_atts, prems, stmt, facts), goal_ctxt) = lthy
      |> bundle_includes raw_includes
      |> prep_statement (prep_att lthy) prep_stmt elems raw_concl;
    val atts = more_atts @ map (prep_att lthy) raw_atts;

    val pos = Position.thread_data ();
    fun after_qed' results goal_ctxt' =
      let
        val results' =
          burrow (map (Goal.norm_result lthy) o Proof_Context.export goal_ctxt' lthy) results;
        val (res, lthy') =
          if forall (Binding.is_empty_atts o fst) stmt then (map (pair "") results', lthy)
          else
            Local_Theory.notes_kind kind
              (map2 (fn (b, _) => fn ths => (b, [(ths, [])])) stmt results') lthy;
        val lthy'' =
          if Binding.is_empty_atts (name, atts) then
            (Proof_Display.print_results int pos lthy' ((kind, ""), res); lthy')
          else
            let
              val ([(res_name, _)], lthy'') =
                Local_Theory.notes_kind kind [((name, atts), [(maps #2 res, [])])] lthy';
              val _ = Proof_Display.print_results int pos lthy' ((kind, res_name), res);
            in lthy'' end;
      in after_qed results' lthy'' end;

    val prems_name = if long then Auto_Bind.assmsN else Auto_Bind.thatN;
  in
    goal_ctxt
    |> not (null prems) ?
      (Proof_Context.note_thmss "" [((Binding.name prems_name, []), [(prems, [])])] #> snd)
    |> Proof.theorem before_qed after_qed' (map snd stmt)
    |> (case facts of NONE => I | SOME ths => Proof.refine_insert ths)
    |> tap (fn state => not schematic andalso Proof.schematic_goal state andalso
        error "Illegal schematic goal statement")
  end;

in

val theorem =
  gen_theorem false Bundle.includes (K I) Expression.cert_statement;
val theorem_cmd =
  gen_theorem false Bundle.includes_cmd Attrib.check_src Expression.read_statement;

val schematic_theorem =
  gen_theorem true Bundle.includes (K I) Expression.cert_statement;
val schematic_theorem_cmd =
  gen_theorem true Bundle.includes_cmd Attrib.check_src Expression.read_statement;

end;

end;
