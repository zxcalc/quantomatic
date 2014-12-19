(*  Title:      Pure/Isar/calculation.ML
    Author:     Markus Wenzel, TU Muenchen

Generic calculational proofs.
*)

signature CALCULATION =
sig
  val print_rules: Proof.context -> unit
  val get_calculation: Proof.state -> thm list option
  val trans_add: attribute
  val trans_del: attribute
  val sym_add: attribute
  val sym_del: attribute
  val symmetric: attribute
  val also: thm list option -> bool -> Proof.state -> Proof.state Seq.result Seq.seq
  val also_cmd: (Facts.ref * Attrib.src list) list option ->
    bool -> Proof.state -> Proof.state Seq.result Seq.seq
  val finally: thm list option -> bool -> Proof.state -> Proof.state Seq.result Seq.seq
  val finally_cmd: (Facts.ref * Attrib.src list) list option -> bool ->
    Proof.state -> Proof.state Seq.result Seq.seq
  val moreover: bool -> Proof.state -> Proof.state
  val ultimately: bool -> Proof.state -> Proof.state
end;

structure Calculation: CALCULATION =
struct

(** calculation data **)

structure Data = Generic_Data
(
  type T = (thm Item_Net.T * thm list) * (thm list * int) option;
  val empty = ((Thm.elim_rules, []), NONE);
  val extend = I;
  fun merge (((trans1, sym1), _), ((trans2, sym2), _)) =
    ((Item_Net.merge (trans1, trans2), Thm.merge_thms (sym1, sym2)), NONE);
);

val get_rules = #1 o Data.get o Context.Proof;

fun print_rules ctxt =
  let
    val pretty_thm = Display.pretty_thm_item ctxt;
    val (trans, sym) = get_rules ctxt;
  in
   [Pretty.big_list "transitivity rules:" (map pretty_thm (Item_Net.content trans)),
    Pretty.big_list "symmetry rules:" (map pretty_thm sym)]
  end |> Pretty.writeln_chunks;


(* access calculation *)

fun get_calculation state =
  (case #2 (Data.get (Context.Proof (Proof.context_of state))) of
    NONE => NONE
  | SOME (thms, lev) => if lev = Proof.level state then SOME thms else NONE);

val calculationN = "calculation";

fun put_calculation calc =
  `Proof.level #-> (fn lev => Proof.map_context (Context.proof_map
     (Data.map (apsnd (K (Option.map (rpair lev) calc))))))
  #> Proof.put_thms false (calculationN, calc);



(** attributes **)

(* add/del rules *)

val trans_add = Thm.declaration_attribute (Data.map o apfst o apfst o Item_Net.update);
val trans_del = Thm.declaration_attribute (Data.map o apfst o apfst o Item_Net.remove);

val sym_add =
  Thm.declaration_attribute (fn th =>
    (Data.map o apfst o apsnd) (Thm.add_thm th) #>
    Thm.attribute_declaration (Context_Rules.elim_query NONE) th);

val sym_del =
  Thm.declaration_attribute (fn th =>
    (Data.map o apfst o apsnd) (Thm.del_thm th) #>
    Thm.attribute_declaration Context_Rules.rule_del th);


(* symmetric *)

val symmetric = Thm.rule_attribute (fn x => fn th =>
  (case Seq.chop 2 (Drule.multi_resolves [th] (#2 (#1 (Data.get x)))) of
    ([th'], _) => Drule.zero_var_indexes th'
  | ([], _) => raise THM ("symmetric: no unifiers", 1, [th])
  | _ => raise THM ("symmetric: multiple unifiers", 1, [th])));


(* concrete syntax *)

val _ = Theory.setup
 (Attrib.setup @{binding trans} (Attrib.add_del trans_add trans_del)
    "declaration of transitivity rule" #>
  Attrib.setup @{binding sym} (Attrib.add_del sym_add sym_del)
    "declaration of symmetry rule" #>
  Attrib.setup @{binding symmetric} (Scan.succeed symmetric)
    "resolution with symmetry rule" #>
  Global_Theory.add_thms
   [((Binding.empty, transitive_thm), [trans_add]),
    ((Binding.empty, symmetric_thm), [sym_add])] #> snd);



(** proof commands **)

fun assert_sane final =
  if final then Proof.assert_forward else Proof.assert_forward_or_chain;

fun maintain_calculation int final calc state =
  let
    val state' = put_calculation (SOME calc) state;
    val ctxt' = Proof.context_of state';
    val _ =
      if int then
        Proof_Context.pretty_fact ctxt'
          (Proof_Context.full_name ctxt' (Binding.name calculationN), calc)
        |> Pretty.string_of |> Output.urgent_message
      else ();
  in state' |> final ? (put_calculation NONE #> Proof.chain_facts calc) end;


(* also and finally *)

fun calculate prep_rules final raw_rules int state =
  let
    val ctxt = Proof.context_of state;
    val pretty_thm = Display.pretty_thm ctxt;
    val pretty_thm_item = Display.pretty_thm_item ctxt;

    val strip_assums_concl = Logic.strip_assums_concl o Thm.prop_of;
    val eq_prop = op aconv o pairself (Envir.beta_eta_contract o strip_assums_concl);
    fun check_projection ths th =
      (case find_index (curry eq_prop th) ths of
        ~1 => Seq.Result [th]
      | i =>
          Seq.Error (fn () =>
            (Pretty.string_of o Pretty.chunks)
             [Pretty.block [Pretty.str "Vacuous calculation result:", Pretty.brk 1, pretty_thm th],
              (Pretty.block o Pretty.fbreaks)
                (Pretty.str ("derived as projection (" ^ string_of_int (i + 1) ^ ") from:") ::
                  map pretty_thm_item ths)]));

    val opt_rules = Option.map (prep_rules ctxt) raw_rules;
    fun combine ths =
      Seq.append
        ((case opt_rules of
          SOME rules => rules
        | NONE =>
            (case ths of
              [] => Item_Net.content (#1 (get_rules ctxt))
            | th :: _ => Item_Net.retrieve (#1 (get_rules ctxt)) (strip_assums_concl th)))
        |> Seq.of_list |> Seq.maps (Drule.multi_resolve ths)
        |> Seq.map (check_projection ths))
        (Seq.single (Seq.Error (fn () =>
          (Pretty.string_of o Pretty.block o Pretty.fbreaks)
            (Pretty.str "No matching trans rules for calculation:" ::
              map pretty_thm_item ths))));

    val facts = Proof.the_facts (assert_sane final state);
    val (initial, calculations) =
      (case get_calculation state of
        NONE => (true, Seq.single (Seq.Result facts))
      | SOME calc => (false, combine (calc @ facts)));

    val _ = initial andalso final andalso error "No calculation yet";
    val _ = initial andalso is_some opt_rules andalso
      error "Initial calculation -- no rules to be given";
  in
    calculations |> Seq.map_result (fn calc => maintain_calculation int final calc state)
  end;

val also = calculate (K I) false;
val also_cmd = calculate Attrib.eval_thms false;
val finally = calculate (K I) true;
val finally_cmd = calculate Attrib.eval_thms true;


(* moreover and ultimately *)

fun collect final int state =
  let
    val facts = Proof.the_facts (assert_sane final state);
    val (initial, thms) =
      (case get_calculation state of
        NONE => (true, [])
      | SOME thms => (false, thms));
    val calc = thms @ facts;
    val _ = initial andalso final andalso error "No calculation yet";
  in maintain_calculation int final calc state end;

val moreover = collect false;
val ultimately = collect true;


(* outer syntax *)

val calc_args =
  Scan.option (@{keyword "("} |-- Parse.!!! ((Parse_Spec.xthms1 --| @{keyword ")"})));

val _ =
  Outer_Syntax.command @{command_spec "also"} "combine calculation and current facts"
    (calc_args >> (Toplevel.proofs' o also_cmd));

val _ =
  Outer_Syntax.command @{command_spec "finally"}
    "combine calculation and current facts, exhibit result"
    (calc_args >> (Toplevel.proofs' o finally_cmd));

val _ =
  Outer_Syntax.command @{command_spec "moreover"} "augment calculation by current facts"
    (Scan.succeed (Toplevel.proof' moreover));

val _ =
  Outer_Syntax.command @{command_spec "ultimately"}
    "augment calculation by current facts, exhibit result"
    (Scan.succeed (Toplevel.proof' ultimately));

val _ =
  Outer_Syntax.improper_command @{command_spec "print_trans_rules"} "print transitivity rules"
    (Scan.succeed (Toplevel.unknown_context o Toplevel.keep (print_rules o Toplevel.context_of)));

end;
