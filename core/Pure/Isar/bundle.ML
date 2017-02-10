(*  Title:      Pure/Isar/bundle.ML
    Author:     Makarius

Bundled declarations (notes etc.).
*)

signature BUNDLE =
sig
  val check: Proof.context -> xstring * Position.T -> string
  val get_bundle: Proof.context -> string -> Attrib.thms
  val get_bundle_cmd: Proof.context -> xstring * Position.T -> Attrib.thms
  val print_bundles: bool -> Proof.context -> unit
  val bundle: binding * Attrib.thms ->
    (binding * typ option * mixfix) list -> local_theory -> local_theory
  val bundle_cmd: binding * (Facts.ref * Token.src list) list ->
    (binding * string option * mixfix) list -> local_theory -> local_theory
  val init: binding -> theory -> local_theory
  val unbundle: string list -> local_theory -> local_theory
  val unbundle_cmd: (xstring * Position.T) list -> local_theory -> local_theory
  val includes: string list -> Proof.context -> Proof.context
  val includes_cmd: (xstring * Position.T) list -> Proof.context -> Proof.context
  val include_: string list -> Proof.state -> Proof.state
  val include_cmd: (xstring * Position.T) list -> Proof.state -> Proof.state
  val including: string list -> Proof.state -> Proof.state
  val including_cmd: (xstring * Position.T) list -> Proof.state -> Proof.state
  val context: string list -> Element.context_i list ->
    generic_theory -> Binding.scope * local_theory
  val context_cmd: (xstring * Position.T) list -> Element.context list ->
    generic_theory -> Binding.scope * local_theory
end;

structure Bundle: BUNDLE =
struct

(** context data **)

structure Data = Generic_Data
(
  type T = Attrib.thms Name_Space.table * Attrib.thms option;
  val empty : T = (Name_Space.empty_table "bundle", NONE);
  val extend = I;
  fun merge ((tab1, target1), (tab2, target2)) =
    (Name_Space.merge_tables (tab1, tab2), merge_options (target1, target2));
);


(* bundles *)

val get_bundles_generic = #1 o Data.get;
val get_bundles = get_bundles_generic o Context.Proof;

fun check ctxt = #1 o Name_Space.check (Context.Proof ctxt) (get_bundles ctxt);

val get_bundle_generic = Name_Space.get o get_bundles_generic;
val get_bundle = get_bundle_generic o Context.Proof;
fun get_bundle_cmd ctxt = get_bundle ctxt o check ctxt;

fun define_bundle def context =
  let
    val (name, bundles') = Name_Space.define context true def (get_bundles_generic context);
    val context' = (Data.map o apfst o K) bundles' context;
  in (name, context') end;


(* target -- bundle under construction *)

fun the_target thy =
  (case #2 (Data.get (Context.Theory thy)) of
    SOME thms => thms
  | NONE => error "Missing bundle target");

val reset_target = (Context.theory_map o Data.map o apsnd o K) NONE;
val set_target = Context.theory_map o Data.map o apsnd o K o SOME;

fun augment_target thms =
  Local_Theory.background_theory (fn thy => set_target (the_target thy @ thms) thy);


(* print bundles *)

fun pretty_bundle ctxt (markup_name, bundle) =
  let
    val prt_thm = Pretty.cartouche o Thm.pretty_thm ctxt;
    fun prt_thm_attribs atts th =
      Pretty.block (Pretty.breaks (prt_thm th :: Attrib.pretty_attribs ctxt atts));
    fun prt_thms (ths, []) = map prt_thm ths
      | prt_thms (ths, atts) = map (prt_thm_attribs atts) ths;
  in
    Pretty.block ([Pretty.keyword1 "bundle", Pretty.str " ", Pretty.mark_str markup_name] @
      (if null bundle then [] else Pretty.fbreaks (Pretty.str " =" :: maps prt_thms bundle)))
  end;

fun print_bundles verbose ctxt =
  Pretty.writeln_chunks
    (map (pretty_bundle ctxt) (Name_Space.markup_table verbose ctxt (get_bundles ctxt)));



(** define bundle **)

fun transform_bundle phi =
  map (fn (fact, atts) => (Morphism.fact phi fact, (map o map) (Token.transform phi) atts));


(* command *)

local

fun gen_bundle prep_fact prep_att add_fixes (binding, raw_bundle) raw_fixes lthy =
  let
    val (_, ctxt') = add_fixes raw_fixes lthy;
    val bundle0 = raw_bundle
      |> map (fn (fact, atts) => (prep_fact ctxt' fact, map (prep_att ctxt') atts));
    val bundle =
      Attrib.partial_evaluation ctxt' [(Binding.empty_atts, bundle0)] |> map snd |> flat
      |> transform_bundle (Proof_Context.export_morphism ctxt' lthy);
  in
    lthy |> Local_Theory.declaration {syntax = false, pervasive = true}
      (fn phi => #2 o define_bundle (Morphism.binding phi binding, transform_bundle phi bundle))
  end;

in

val bundle = gen_bundle (K I) (K I) Proof_Context.add_fixes;
val bundle_cmd = gen_bundle Proof_Context.get_fact Attrib.check_src Proof_Context.add_fixes_cmd;

end;


(* target *)

local

fun bad_operation _ = error "Not possible in bundle target";

fun conclude invisible binding =
  Local_Theory.background_theory_result (fn thy =>
    thy
    |> invisible ? Context_Position.set_visible_global false
    |> Context.Theory
    |> define_bundle (binding, the_target thy)
    ||> (Context.the_theory
      #> invisible ? Context_Position.restore_visible_global thy
      #> reset_target));

fun pretty binding lthy =
  let
    val bundle = the_target (Proof_Context.theory_of lthy);
    val (name, lthy') = lthy
      |> Local_Theory.raw_theory (Context_Position.set_visible_global false)
      |> conclude true binding;
    val thy_ctxt' = Proof_Context.init_global (Proof_Context.theory_of lthy');
    val markup_name =
      Name_Space.markup_extern thy_ctxt' (Name_Space.space_of_table (get_bundles thy_ctxt')) name;
  in [pretty_bundle lthy' (markup_name, bundle)] end;

fun bundle_notes kind facts lthy =
  let
    val bundle = facts
      |> maps (fn ((_, more_atts), thms) => map (fn (ths, atts) => (ths, atts @ more_atts)) thms);
  in
    lthy
    |> augment_target (transform_bundle (Local_Theory.standard_morphism_theory lthy) bundle)
    |> Generic_Target.standard_notes (op <>) kind facts
    |> Attrib.local_notes kind facts
  end;

fun bundle_declaration decl lthy =
  lthy
  |> (augment_target o Attrib.internal_declaration)
    (Morphism.transform (Local_Theory.standard_morphism_theory lthy) decl)
  |> Generic_Target.standard_declaration (K true) decl;

in

fun init binding thy =
  thy
  |> Sign.change_begin
  |> set_target []
  |> Proof_Context.init_global
  |> Local_Theory.init (Sign.naming_of thy)
     {define = bad_operation,
      notes = bundle_notes,
      abbrev = bad_operation,
      declaration = K bundle_declaration,
      theory_registration = bad_operation,
      locale_dependency = bad_operation,
      pretty = pretty binding,
      exit = conclude false binding #> #2 #> Local_Theory.target_of #> Sign.change_end_local}

end;



(** activate bundles **)

local

fun gen_activate notes get args ctxt =
  let val decls = maps (get ctxt) args in
    ctxt
    |> Context_Position.set_visible false
    |> notes [(Binding.empty_atts, decls)] |> #2
    |> Context_Position.restore_visible ctxt
  end;

fun gen_includes get = gen_activate (Attrib.local_notes "") get;

fun gen_context get prep_decl raw_incls raw_elems gthy =
  let
    val (after_close, lthy) =
      gthy |> Context.cases (pair Local_Theory.exit o Named_Target.theory_init)
        (pair I o Local_Theory.assert);
    val ((_, _, _, lthy'), _) = lthy
      |> gen_includes get raw_incls
      |> prep_decl ([], []) I raw_elems;
  in
    lthy' |> Local_Theory.init_target
      (Local_Theory.background_naming_of lthy) (Local_Theory.operations_of lthy) after_close
  end;

in

val unbundle = gen_activate Local_Theory.notes get_bundle;
val unbundle_cmd = gen_activate Local_Theory.notes get_bundle_cmd;

val includes = gen_includes get_bundle;
val includes_cmd = gen_includes get_bundle_cmd;

fun include_ bs = Proof.assert_forward #> Proof.map_context (includes bs) #> Proof.reset_facts;
fun include_cmd bs =
  Proof.assert_forward #> Proof.map_context (includes_cmd bs) #> Proof.reset_facts;

fun including bs = Proof.assert_backward #> Proof.map_context (includes bs);
fun including_cmd bs = Proof.assert_backward #> Proof.map_context (includes_cmd bs);

val context = gen_context get_bundle Expression.cert_declaration;
val context_cmd = gen_context get_bundle_cmd Expression.read_declaration;

end;

end;
