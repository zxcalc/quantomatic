(*  Title:      Pure/theory.ML
    Author:     Lawrence C Paulson and Markus Wenzel

Logical theory content: axioms, definitions, and begin/end wrappers.
*)

signature THEORY =
sig
  val parents_of: theory -> theory list
  val ancestors_of: theory -> theory list
  val nodes_of: theory -> theory list
  val setup: (theory -> theory) -> unit
  val local_setup: (Proof.context -> Proof.context) -> unit
  val get_markup: theory -> Markup.T
  val check: Proof.context -> string * Position.T -> theory
  val axiom_table: theory -> term Name_Space.table
  val axiom_space: theory -> Name_Space.T
  val axioms_of: theory -> (string * term) list
  val all_axioms_of: theory -> (string * term) list
  val defs_of: theory -> Defs.T
  val at_begin: (theory -> theory option) -> theory -> theory
  val at_end: (theory -> theory option) -> theory -> theory
  val begin_theory: string * Position.T -> theory list -> theory
  val end_theory: theory -> theory
  val add_axiom: Proof.context -> binding * term -> theory -> theory
  val const_dep: theory -> string * typ -> Defs.entry
  val type_dep: string * typ list -> Defs.entry
  val add_deps: Defs.context -> string -> Defs.entry -> Defs.entry list -> theory -> theory
  val add_deps_global: string -> Defs.entry -> Defs.entry list -> theory -> theory
  val add_def: Defs.context -> bool -> bool -> binding * term -> theory -> theory
  val specify_const: (binding * typ) * mixfix -> theory -> term * theory
  val check_overloading: Proof.context -> bool -> string * typ -> unit
end

structure Theory: THEORY =
struct


(** theory context operations **)

val parents_of = Context.parents_of;
val ancestors_of = Context.ancestors_of;
fun nodes_of thy = thy :: ancestors_of thy;

fun setup f = Context.>> (Context.map_theory f);
fun local_setup f = Context.>> (Context.map_proof f);



(** datatype thy **)

type wrapper = (theory -> theory option) * stamp;

fun apply_wrappers (wrappers: wrapper list) =
  perhaps (perhaps_loop (perhaps_apply (map fst wrappers)));

datatype thy = Thy of
 {pos: Position.T,
  id: serial,
  axioms: term Name_Space.table,
  defs: Defs.T,
  wrappers: wrapper list * wrapper list};

fun make_thy (pos, id, axioms, defs, wrappers) =
  Thy {pos = pos, id = id, axioms = axioms, defs = defs, wrappers = wrappers};

structure Thy = Theory_Data'
(
  type T = thy;
  val empty_axioms = Name_Space.empty_table "axiom" : term Name_Space.table;
  val empty = make_thy (Position.none, 0, empty_axioms, Defs.empty, ([], []));

  fun extend (Thy {pos = _, id = _, axioms = _, defs, wrappers}) =
    make_thy (Position.none, 0, empty_axioms, defs, wrappers);

  fun merge old_thys (thy1, thy2) =
    let
      val Thy {pos = _, id = _, axioms = _, defs = defs1, wrappers = (bgs1, ens1)} = thy1;
      val Thy {pos = _, id = _, axioms = _, defs = defs2, wrappers = (bgs2, ens2)} = thy2;

      val axioms' = empty_axioms;
      val defs' = Defs.merge (Defs.global_context (fst old_thys)) (defs1, defs2);
      val bgs' = Library.merge (eq_snd op =) (bgs1, bgs2);
      val ens' = Library.merge (eq_snd op =) (ens1, ens2);
    in make_thy (Position.none, 0, axioms', defs', (bgs', ens')) end;
);

fun rep_theory thy = Thy.get thy |> (fn Thy args => args);

fun map_thy f = Thy.map (fn (Thy {pos, id, axioms, defs, wrappers}) =>
  make_thy (f (pos, id, axioms, defs, wrappers)));

fun map_axioms f =
  map_thy (fn (pos, id, axioms, defs, wrappers) => (pos, id, f axioms, defs, wrappers));

fun map_defs f =
  map_thy (fn (pos, id, axioms, defs, wrappers) => (pos, id, axioms, f defs, wrappers));

fun map_wrappers f =
  map_thy (fn (pos, id, axioms, defs, wrappers) => (pos, id, axioms, defs, f wrappers));


(* entity markup *)

fun theory_markup def name id pos =
  if id = 0 then Markup.empty
  else
    Markup.properties (Position.entity_properties_of def id pos)
      (Markup.entity Markup.theoryN name);

fun init_markup (name, pos) thy =
  let
    val id = serial ();
    val _ = Position.report pos (theory_markup true name id pos);
  in map_thy (fn (_, _, axioms, defs, wrappers) => (pos, id, axioms, defs, wrappers)) thy end;

fun get_markup thy =
  let val {pos, id, ...} = rep_theory thy
  in theory_markup false (Context.theory_name thy) id pos end;

fun check ctxt (name, pos) =
  let
    val thy = Proof_Context.theory_of ctxt;
    val thy' =
      Context.get_theory thy name
        handle ERROR msg =>
          let
            val completion =
              Completion.make (name, pos)
                (fn completed =>
                  map Context.theory_name (ancestors_of thy)
                  |> filter completed
                  |> sort_strings
                  |> map (fn a => (a, (Markup.theoryN, a))));
            val report = Markup.markup_report (Completion.reported_text completion);
          in error (msg ^ Position.here pos ^ report) end;
    val _ = Context_Position.report ctxt pos (get_markup thy');
  in thy' end;


(* basic operations *)

val axiom_table = #axioms o rep_theory;
val axiom_space = Name_Space.space_of_table o axiom_table;

fun axioms_of thy = rev (Name_Space.fold_table cons (axiom_table thy) []);
fun all_axioms_of thy = maps axioms_of (nodes_of thy);

val defs_of = #defs o rep_theory;


(* begin/end theory *)

val begin_wrappers = rev o #1 o #wrappers o rep_theory;
val end_wrappers = rev o #2 o #wrappers o rep_theory;

fun at_begin f = map_wrappers (apfst (cons (f, stamp ())));
fun at_end f = map_wrappers (apsnd (cons (f, stamp ())));

fun begin_theory (name, pos) imports =
  if name = Context.PureN then
    (case imports of
      [thy] => init_markup (name, pos) thy
    | _ => error "Bad bootstrapping of theory Pure")
  else
    let
      val thy = Context.begin_thy name imports;
      val wrappers = begin_wrappers thy;
    in
      thy
      |> init_markup (name, pos)
      |> Sign.local_path
      |> Sign.map_naming (Name_Space.set_theory_name name)
      |> apply_wrappers wrappers
      |> tap (Syntax.force_syntax o Sign.syn_of)
    end;

fun end_theory thy =
  thy
  |> apply_wrappers (end_wrappers thy)
  |> Sign.change_check
  |> Context.finish_thy;



(** primitive specifications **)

(* raw axioms *)

fun cert_axm ctxt (b, raw_tm) =
  let
    val thy = Proof_Context.theory_of ctxt;
    val t = Sign.cert_prop thy raw_tm
      handle TYPE (msg, _, _) => error msg
        | TERM (msg, _) => error msg;
    val _ = Term.no_dummy_patterns t handle TERM (msg, _) => error msg;

    val bad_sorts =
      rev ((fold_types o fold_atyps_sorts) (fn (_, []) => I | (T, _) => insert (op =) T) t []);
    val _ = null bad_sorts orelse
      error ("Illegal sort constraints in primitive specification: " ^
        commas (map (Syntax.string_of_typ (Config.put show_sorts true ctxt)) bad_sorts));
  in (b, Sign.no_vars ctxt t) end
  handle ERROR msg => cat_error msg ("The error(s) above occurred in axiom " ^ Binding.print b);

fun add_axiom ctxt raw_axm thy = thy |> map_axioms (fn axioms =>
  let
    val axm = apsnd Logic.varify_global (cert_axm ctxt raw_axm);
    val context = ctxt
      |> Sign.inherit_naming thy
      |> Context_Position.set_visible_generic false;
    val (_, axioms') = Name_Space.define context true axm axioms;
  in axioms' end);


(* dependencies *)

fun const_dep thy (c, T) = ((Defs.Const, c), Sign.const_typargs thy (c, T));
fun type_dep (c, args) = ((Defs.Type, c), args);

fun dependencies (context as (ctxt, _)) unchecked def description lhs rhs =
  let
    fun prep (item, args) =
      (case fold Term.add_tvarsT args [] of
        [] => (item, map Logic.varifyT_global args)
      | vs => raise TYPE ("Illegal schematic type variable(s)", map TVar vs, []));

    val lhs_vars = fold Term.add_tfreesT (snd lhs) [];
    val rhs_extras =
      fold (fn (_, args) => args |> (fold o Term.fold_atyps) (fn TFree v =>
        if member (op =) lhs_vars v then I else insert (op =) v)) rhs [];
    val _ =
      if null rhs_extras then ()
      else error ("Specification depends on extra type variables: " ^
        commas_quote (map (Syntax.string_of_typ ctxt o TFree) rhs_extras) ^
        "\nThe error(s) above occurred in " ^ quote description);
  in Defs.define context unchecked def description (prep lhs) (map prep rhs) end;

fun cert_entry thy ((Defs.Const, c), args) =
      Sign.cert_term thy (Const (c, Sign.const_instance thy (c, args)))
      |> dest_Const |> const_dep thy
  | cert_entry thy ((Defs.Type, c), args) =
      Sign.certify_typ thy (Type (c, args)) |> dest_Type |> type_dep;

fun add_deps context a raw_lhs raw_rhs thy =
  let
    val (lhs as ((_, lhs_name), _)) :: rhs = map (cert_entry thy) (raw_lhs :: raw_rhs);
    val description = if a = "" then lhs_name ^ " axiom" else a;
  in thy |> map_defs (dependencies context false NONE description lhs rhs) end;

fun add_deps_global a x y thy =
  add_deps (Defs.global_context thy) a x y thy;

fun specify_const decl thy =
  let val (t, thy') = Sign.declare_const_global decl thy;
  in (t, add_deps_global "" (const_dep thy' (dest_Const t)) [] thy') end;


(* overloading *)

fun check_overloading ctxt overloaded (c, T) =
  let
    val thy = Proof_Context.theory_of ctxt;

    val declT = Sign.the_const_constraint thy c
      handle TYPE (msg, _, _) => error msg;
    val T' = Logic.varifyT_global T;

    fun message sorts txt =
      [Pretty.block [Pretty.str "Specification of constant ",
        Pretty.str c, Pretty.str " ::", Pretty.brk 1,
        Pretty.quote (Syntax.pretty_typ (Config.put show_sorts sorts ctxt) T)],
        Pretty.str txt] |> Pretty.chunks |> Pretty.string_of;
  in
    if Sign.typ_instance thy (declT, T') then ()
    else if Type.raw_instance (declT, T') then
      error (message true "imposes additional sort constraints on the constant declaration")
    else if overloaded then ()
    else
      error (message false "is strictly less general than the declared type (overloading required)")
  end;


(* definitional axioms *)

local

fun check_def (context as (ctxt, _)) thy unchecked overloaded (b, tm) defs =
  let
    val name = Sign.full_name thy b;
    val ((lhs, rhs), _, _) =
      Primitive_Defs.dest_def ctxt
        {check_head = Term.is_Const,
         check_free_lhs = K true,
         check_free_rhs = K false,
         check_tfree = K false} tm
      handle TERM (msg, _) => error msg;
    val lhs_const = Term.dest_Const (Term.head_of lhs);

    val rhs_consts =
      fold_aterms (fn Const const => insert (op =) (const_dep thy const) | _ => I) rhs [];
    val rhs_types =
      (fold_types o fold_subtypes) (fn Type t => insert (op =) (type_dep t) | _ => I) rhs [];
    val rhs_deps = rhs_consts @ rhs_types;

    val _ = check_overloading ctxt overloaded lhs_const;
  in defs |> dependencies context unchecked (SOME name) name (const_dep thy lhs_const) rhs_deps end
  handle ERROR msg => cat_error msg (Pretty.string_of (Pretty.block
   [Pretty.str ("The error(s) above occurred in definition " ^ Binding.print b ^ ":"),
    Pretty.fbrk, Pretty.quote (Syntax.pretty_term ctxt tm)]));

in

fun add_def (context as (ctxt, _)) unchecked overloaded raw_axm thy =
  let val axm = cert_axm ctxt raw_axm in
    thy
    |> map_defs (check_def context thy unchecked overloaded axm)
    |> add_axiom ctxt axm
  end;

end;

end;
