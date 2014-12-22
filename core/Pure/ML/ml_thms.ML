(*  Title:      Pure/ML/ml_thms.ML
    Author:     Makarius

Attribute source and theorem values within ML.
*)

signature ML_THMS =
sig
  val the_attributes: Proof.context -> int -> Args.src list
  val the_thmss: Proof.context -> thm list list
  val get_stored_thms: unit -> thm list
  val get_stored_thm: unit -> thm
  val store_thms: string * thm list -> unit
  val store_thm: string * thm -> unit
  val bind_thm: string * thm -> unit
  val bind_thms: string * thm list -> unit
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

val _ = Theory.setup
  (ML_Antiquotation.declaration @{binding attributes} (Scan.lift Parse_Spec.attribs)
    (fn _ => fn raw_srcs => fn ctxt =>
      let
        val i = serial ();
        val srcs = map (Attrib.check_src ctxt) raw_srcs;
        val _ = map (Attrib.attribute ctxt) srcs;
        val (a, ctxt') = ctxt
          |> ML_Antiquotation.variant "attributes" ||> put_attributes (i, srcs);
        val ml =
          ("val " ^ a ^ " = ML_Thms.the_attributes ML_context " ^
            string_of_int i ^ ";\n", "Isabelle." ^ a);
      in (K ml, ctxt') end));


(* fact references *)

fun thm_binding kind is_single thms ctxt =
  let
    val initial = null (get_thmss ctxt);
    val (name, ctxt') = ML_Antiquotation.variant kind ctxt;
    val ctxt'' = cons_thms ((name, is_single), thms) ctxt';

    val ml_body = "Isabelle." ^ name;
    fun decl final_ctxt =
      if initial then
        let
          val binds = get_thmss final_ctxt |> map (fn ((a, b), _) => (b ? enclose "[" "]") a);
          val ml_env = "val [" ^ commas binds ^ "] = ML_Thms.the_thmss ML_context;\n";
        in (ml_env, ml_body) end
      else ("", ml_body);
  in (decl, ctxt'') end;

val _ = Theory.setup
  (ML_Antiquotation.declaration @{binding thm} (Attrib.thm >> single) (K (thm_binding "thm" true)) #>
   ML_Antiquotation.declaration @{binding thms} Attrib.thms (K (thm_binding "thms" false)));


(* ad-hoc goals *)

val and_ = Args.$$$ "and";
val by = Args.$$$ "by";
val goal = Scan.unless (by || and_) Args.name_inner_syntax;

val _ = Theory.setup
  (ML_Antiquotation.declaration @{binding lemma}
    (Scan.lift (Args.mode "open" -- Parse.enum1 "and" (Scan.repeat1 goal) --
      (by |-- Method.parse -- Scan.option Method.parse)))
    (fn _ => fn ((is_open, raw_propss), (m1, m2)) => fn ctxt =>
      let
        val _ = Context_Position.reports ctxt (maps Method.reports_of (m1 :: the_list m2));

        val propss = burrow (map (rpair []) o Syntax.read_props ctxt) raw_propss;
        val prep_result = Goal.norm_result ctxt #> not is_open ? Thm.close_derivation;
        fun after_qed res goal_ctxt =
          Proof_Context.put_thms false (Auto_Bind.thisN,
            SOME (map prep_result (Proof_Context.export goal_ctxt ctxt (flat res)))) goal_ctxt;

        val ctxt' = ctxt
          |> Proof.theorem NONE after_qed propss
          |> Proof.global_terminal_proof (m1, m2);
        val thms =
          Proof_Context.get_fact ctxt'
            (Facts.named (Proof_Context.full_name ctxt' (Binding.name Auto_Bind.thisN)));
      in thm_binding "lemma" (length (flat propss) = 1) thms ctxt end));


(* old-style theorem bindings *)

structure Stored_Thms = Theory_Data
(
  type T = thm list;
  val empty = [];
  fun extend _ = [];
  fun merge _ = [];
);

fun get_stored_thms () = Stored_Thms.get (ML_Context.the_global_context ());
val get_stored_thm = hd o get_stored_thms;

fun ml_store get (name, ths) =
  let
    val ths' = Context.>>> (Context.map_theory_result
      (Global_Theory.store_thms (Binding.name name, ths)));
    val _ = Theory.setup (Stored_Thms.put ths');
    val _ =
      if name = "" then ()
      else if not (ML_Syntax.is_identifier name) then
        error ("Cannot bind theorem(s) " ^ quote name ^ " as ML value")
      else
        ML_Compiler.eval (ML_Compiler.verbose true ML_Compiler.flags) Position.none
          (ML_Lex.tokenize ("val " ^ name ^ " = " ^ get ^ " ();"));
    val _ = Theory.setup (Stored_Thms.put []);
  in () end;

val store_thms = ml_store "ML_Thms.get_stored_thms";
fun store_thm (name, th) = ml_store "ML_Thms.get_stored_thm" (name, [th]);

fun bind_thm (name, thm) = store_thm (name, Drule.export_without_context thm);
fun bind_thms (name, thms) = store_thms (name, map Drule.export_without_context thms);

end;

