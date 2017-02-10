(*  Title:      Pure/Isar/calculation.ML
    Author:     Markus Wenzel, TU Muenchen

Generic calculational proofs.
*)

signature CALCULATION =
sig
  val print_rules: Proof.context -> unit
  val check: Proof.state -> thm list option
  val trans_add: attribute
  val trans_del: attribute
  val sym_add: attribute
  val sym_del: attribute
  val symmetric: attribute
  val also: thm list option -> bool -> Proof.state -> Proof.state Seq.result Seq.seq
  val also_cmd: (Facts.ref * Token.src list) list option ->
    bool -> Proof.state -> Proof.state Seq.result Seq.seq
  val finally: thm list option -> bool -> Proof.state -> Proof.state Seq.result Seq.seq
  val finally_cmd: (Facts.ref * Token.src list) list option -> bool ->
    Proof.state -> Proof.state Seq.result Seq.seq
  val moreover: bool -> Proof.state -> Proof.state
  val ultimately: bool -> Proof.state -> Proof.state
end;

structure Calculation: CALCULATION =
struct

(** calculation data **)

type calculation = {result: thm list, level: int, serial: serial, pos: Position.T};

structure Data = Generic_Data
(
  type T = (thm Item_Net.T * thm list) * calculation option;
  val empty = ((Thm.elim_rules, []), NONE);
  val extend = I;
  fun merge (((trans1, sym1), _), ((trans2, sym2), _)) =
    ((Item_Net.merge (trans1, trans2), Thm.merge_thms (sym1, sym2)), NONE);
);

val get_rules = #1 o Data.get o Context.Proof;
val get_calculation = #2 o Data.get o Context.Proof;

fun print_rules ctxt =
  let
    val pretty_thm = Thm.pretty_thm_item ctxt;
    val (trans, sym) = get_rules ctxt;
  in
   [Pretty.big_list "transitivity rules:" (map pretty_thm (Item_Net.content trans)),
    Pretty.big_list "symmetry rules:" (map pretty_thm sym)]
  end |> Pretty.writeln_chunks;


(* access calculation *)

fun check_calculation state =
  (case get_calculation (Proof.context_of state) of
    NONE => NONE
  | SOME calculation =>
      if #level calculation = Proof.level state then SOME calculation else NONE);

val check = Option.map #result o check_calculation;

val calculationN = "calculation";

fun update_calculation calc state =
  let
    fun report def serial pos =
      Context_Position.report (Proof.context_of state)
        (Position.thread_data ())
          (Markup.entity calculationN ""
            |> Markup.properties (Position.entity_properties_of def serial pos));
    val calculation =
      (case calc of
        NONE => NONE
      | SOME result =>
          (case check_calculation state of
            NONE =>
              let
                val level = Proof.level state;
                val serial = serial ();
                val pos = Position.thread_data ();
                val _ = report true serial pos;
              in SOME {result = result, level = level, serial = serial, pos = pos} end
          | SOME {level, serial, pos, ...} =>
              (report false serial pos;
                SOME {result = result, level = level, serial = serial, pos = pos})));
  in
    state
    |> (Proof.map_context o Context.proof_map o Data.map o apsnd) (K calculation)
    |> Proof.map_context (Proof_Context.put_thms false (calculationN, calc))
  end;



(** attributes **)

(* add/del rules *)

val trans_add = Thm.declaration_attribute (Data.map o apfst o apfst o Item_Net.update);
val trans_del = Thm.declaration_attribute (Data.map o apfst o apfst o Item_Net.remove);

val sym_add =
  Thm.declaration_attribute (fn th =>
    (Data.map o apfst o apsnd) (Thm.add_thm (Thm.trim_context th)) #>
    Thm.attribute_declaration (Context_Rules.elim_query NONE) th);

val sym_del =
  Thm.declaration_attribute (fn th =>
    (Data.map o apfst o apsnd) (Thm.del_thm (Thm.trim_context th)) #>
    Thm.attribute_declaration Context_Rules.rule_del th);


(* symmetric *)

val symmetric =
  Thm.rule_attribute [] (fn context => fn th =>
    (case Seq.chop 2
        (Drule.multi_resolves (SOME (Context.proof_of context)) [th] (#2 (#1 (Data.get context)))) of
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
  if final then Proof.assert_forward
  else
    Proof.assert_forward_or_chain #>
    tap (fn state =>
      if can Proof.assert_chain state then
        Context_Position.report (Proof.context_of state) (Position.thread_data ()) Markup.improper
      else ());

fun maintain_calculation int final calc state =
  let
    val state' = state
      |> update_calculation (SOME calc)
      |> Proof.improper_reset_facts;
    val ctxt' = Proof.context_of state';
    val _ =
      if int then
        Proof_Context.pretty_fact ctxt'
          (Proof_Context.full_name ctxt' (Binding.name calculationN), calc)
        |> Pretty.string_of |> writeln
      else ();
  in state' |> final ? (update_calculation NONE #> Proof.chain_facts calc) end;


(* also and finally *)

fun calculate prep_rules final raw_rules int state =
  let
    val ctxt = Proof.context_of state;
    val pretty_thm = Thm.pretty_thm ctxt;
    val pretty_thm_item = Thm.pretty_thm_item ctxt;

    val strip_assums_concl = Logic.strip_assums_concl o Thm.prop_of;
    val eq_prop = op aconv o apply2 (Envir.beta_eta_contract o strip_assums_concl);
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
        |> Seq.of_list |> Seq.maps (Drule.multi_resolve (SOME ctxt) ths)
        |> Seq.map (check_projection ths))
        (Seq.single (Seq.Error (fn () =>
          (Pretty.string_of o Pretty.block o Pretty.fbreaks)
            (Pretty.str "No matching trans rules for calculation:" ::
              map pretty_thm_item ths))));

    val facts = Proof.the_facts (assert_sane final state);
    val (initial, calculations) =
      (case check state of
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
      (case check state of
        NONE => (true, [])
      | SOME thms => (false, thms));
    val calc = thms @ facts;
    val _ = initial andalso final andalso error "No calculation yet";
  in maintain_calculation int final calc state end;

val moreover = collect false;
val ultimately = collect true;

end;
