(*  Title:      Pure/Isar/bundle.ML
    Author:     Makarius

Bundled declarations (notes etc.).
*)

signature BUNDLE =
sig
  type bundle = (thm list * Token.src list) list
  val check: Proof.context -> xstring * Position.T -> string
  val get_bundle: Proof.context -> string -> bundle
  val get_bundle_cmd: Proof.context -> xstring * Position.T -> bundle
  val bundle: binding * (thm list * Token.src list) list ->
    (binding * typ option * mixfix) list -> local_theory -> local_theory
  val bundle_cmd: binding * (Facts.ref * Token.src list) list ->
    (binding * string option * mixfix) list -> local_theory -> local_theory
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
  val print_bundles: bool -> Proof.context -> unit
end;

structure Bundle: BUNDLE =
struct

(* maintain bundles *)

type bundle = (thm list * Token.src list) list;

fun transform_bundle phi : bundle -> bundle =
  map (fn (fact, atts) => (Morphism.fact phi fact, map (Token.transform_src phi) atts));

structure Data = Generic_Data
(
  type T = bundle Name_Space.table;
  val empty : T = Name_Space.empty_table "bundle";
  val extend = I;
  val merge = Name_Space.merge_tables;
);

val get_bundles = Data.get o Context.Proof;

fun check ctxt = #1 o Name_Space.check (Context.Proof ctxt) (get_bundles ctxt);

val get_bundle = Name_Space.get o get_bundles;
fun get_bundle_cmd ctxt = get_bundle ctxt o check ctxt;


(* define bundle *)

local

fun gen_bundle prep_fact prep_att prep_vars (binding, raw_bundle) fixes lthy =
  let
    val (_, ctxt') = lthy |> prep_vars fixes |-> Proof_Context.add_fixes;
    val bundle0 = raw_bundle
      |> map (fn (fact, atts) => (prep_fact ctxt' fact, map (prep_att ctxt') atts));
    val bundle =
      Attrib.partial_evaluation ctxt' [(Attrib.empty_binding, bundle0)] |> map snd |> flat
      |> transform_bundle (Proof_Context.export_morphism ctxt' lthy);
  in
    lthy |> Local_Theory.declaration {syntax = false, pervasive = true}
      (fn phi => fn context =>
        context |> Data.map
          (#2 o Name_Space.define context true
            (Morphism.binding phi binding, transform_bundle phi bundle)))
  end;

in

val bundle = gen_bundle (K I) (K I) Proof_Context.cert_vars;
val bundle_cmd = gen_bundle Proof_Context.get_fact Attrib.check_src Proof_Context.read_vars;

end;


(* include bundles *)

local

fun gen_includes get args ctxt =
  let val decls = maps (get ctxt) args
  in #2 (Attrib.local_notes "" [((Binding.empty, []), decls)] ctxt) end;

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


(* print_bundles *)

fun print_bundles verbose ctxt =
  let
    val prt_thm = Pretty.backquote o Display.pretty_thm ctxt;

    fun prt_fact (ths, []) = map prt_thm ths
      | prt_fact (ths, atts) = Pretty.enclose "(" ")"
          (Pretty.breaks (map prt_thm ths)) :: Attrib.pretty_attribs ctxt atts;

    fun prt_bundle (name, bundle) =
      Pretty.block (Pretty.keyword1 "bundle" :: Pretty.str " " :: Pretty.mark_str name ::
        Pretty.breaks (Pretty.str " =" :: maps prt_fact bundle));
  in
    map prt_bundle (Name_Space.markup_table verbose ctxt (get_bundles ctxt))
  end |> Pretty.writeln_chunks;

end;
