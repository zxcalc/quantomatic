(*  Title:      Pure/ML/ml_thms.ML
    Author:     Makarius

Attribute source and theorem values within ML.
*)

signature ML_THMS =
sig
  val the_attributes: Proof.context -> int -> Args.src list
  val the_thmss: Proof.context -> thm list list
end;

structure ML_Thms: ML_THMS =
struct

(* auxiliary data *)

type thms = (string * bool) * thm list;  (*name, single, value*)

structure Data = Proof_Data
(
  type T = Args.src list Inttab.table * thms list;
  fun init _ = (Inttab.empty, []);
);

val put_attributes = Data.map o apfst o Inttab.update;
fun the_attributes ctxt name = the (Inttab.lookup (fst (Data.get ctxt)) name);

val get_thmss = snd o Data.get;
val the_thmss = map snd o get_thmss;
val cons_thms = Data.map o apsnd o cons;



(* attribute source *)

val _ =
  Context.>> (Context.map_theory
    (ML_Context.add_antiq (Binding.name "attributes")
      (fn _ => Scan.lift Parse_Spec.attribs >> (fn raw_srcs => fn background =>
        let
          val thy = Proof_Context.theory_of background;

          val i = serial ();
          val srcs = map (Attrib.intern_src thy) raw_srcs;
          val _ = map (Attrib.attribute background) srcs;
          val (a, background') = background
            |> ML_Antiquote.variant "attributes" ||> put_attributes (i, srcs);
          val ml =
            ("val " ^ a ^ " = ML_Thms.the_attributes ML_context " ^
              string_of_int i ^ ";\n", "Isabelle." ^ a);
        in (K ml, background') end))));


(* fact references *)

fun thm_binding kind is_single thms background =
  let
    val initial = null (get_thmss background);
    val (name, background') = ML_Antiquote.variant kind background;
    val background'' = cons_thms ((name, is_single), thms) background';

    val ml_body = "Isabelle." ^ name;
    fun decl ctxt =
      if initial then
        let
          val binds = get_thmss ctxt |> map (fn ((a, b), _) => (b ? enclose "[" "]") a);
          val ml_env = "val [" ^ commas binds ^ "] = ML_Thms.the_thmss ML_context;\n";
        in (ml_env, ml_body) end
      else ("", ml_body);
  in (decl, background'') end;

val _ =
  Context.>> (Context.map_theory
   (ML_Context.add_antiq (Binding.name "thm") (K (Attrib.thm >> (thm_binding "thm" true o single))) #>
    ML_Context.add_antiq (Binding.name "thms") (K (Attrib.thms >> thm_binding "thms" false))));


(* ad-hoc goals *)

val and_ = Args.$$$ "and";
val by = Args.$$$ "by";
val goal = Scan.unless (by || and_) Args.name_source;

val _ =
  Context.>> (Context.map_theory
   (ML_Context.add_antiq (Binding.name "lemma")
    (fn _ => Args.context -- Args.mode "open" --
        Scan.lift (Parse.and_list1 (Scan.repeat1 goal) --
          (by |-- Method.parse -- Scan.option Method.parse)) >>
      (fn ((ctxt, is_open), (raw_propss, methods)) =>
        let
          val propss = burrow (map (rpair []) o Syntax.read_props ctxt) raw_propss;
          val prep_result = Goal.norm_result #> not is_open ? Thm.close_derivation;
          fun after_qed res goal_ctxt =
            Proof_Context.put_thms false (Auto_Bind.thisN,
              SOME (map prep_result (Proof_Context.export goal_ctxt ctxt (flat res)))) goal_ctxt;

          val ctxt' = ctxt
            |> Proof.theorem NONE after_qed propss
            |> Proof.global_terminal_proof methods;
          val thms =
            Proof_Context.get_fact ctxt'
              (Facts.named (Proof_Context.full_name ctxt' (Binding.name Auto_Bind.thisN)));
        in thm_binding "lemma" (length (flat propss) = 1) thms end))));

end;

