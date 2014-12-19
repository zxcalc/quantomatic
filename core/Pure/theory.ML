(*  Title:      Pure/theory.ML
    Author:     Lawrence C Paulson and Markus Wenzel

Logical theory content: axioms, definitions, and begin/end wrappers.
*)

signature THEORY =
sig
  val eq_thy: theory * theory -> bool
  val subthy: theory * theory -> bool
  val assert_super: theory -> theory -> theory
  val parents_of: theory -> theory list
  val ancestors_of: theory -> theory list
  val nodes_of: theory -> theory list
  val merge: theory * theory -> theory
  val merge_list: theory list -> theory
  val requires: theory -> string -> string -> unit
  val setup: (theory -> theory) -> unit
  val get_markup: theory -> Markup.T
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
  val add_deps: Proof.context -> string -> string * typ -> (string * typ) list -> theory -> theory
  val add_deps_global: string -> string * typ -> (string * typ) list -> theory -> theory
  val add_def: Proof.context -> bool -> bool -> binding * term -> theory -> theory
  val specify_const: (binding * typ) * mixfix -> theory -> term * theory
  val check_overloading: Proof.context -> bool -> string * typ -> unit
end

structure Theory: THEORY =
struct


(** theory context operations **)

val eq_thy = Context.eq_thy;
val subthy = Context.subthy;

fun assert_super thy1 thy2 =
  if subthy (thy1, thy2) then thy2
  else raise THEORY ("Not a super theory", [thy1, thy2]);

val parents_of = Context.parents_of;
val ancestors_of = Context.ancestors_of;
fun nodes_of thy = thy :: ancestors_of thy;

val merge = Context.merge;

fun merge_list [] = raise THEORY ("Empty merge of theories", [])
  | merge_list (thy :: thys) = Library.foldl merge (thy, thys);

fun requires thy name what =
  if exists (fn thy' => Context.theory_name thy' = name) (nodes_of thy) then ()
  else error ("Require theory " ^ quote name ^ " as an ancestor for " ^ what);

fun setup f = Context.>> (Context.map_theory f);



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

structure Thy = Theory_Data_PP
(
  type T = thy;
  val empty_axioms = Name_Space.empty_table "axiom" : term Name_Space.table;
  val empty = make_thy (Position.none, 0, empty_axioms, Defs.empty, ([], []));

  fun extend (Thy {pos = _, id = _, axioms = _, defs, wrappers}) =
    make_thy (Position.none, 0, empty_axioms, defs, wrappers);

  fun merge pp (thy1, thy2) =
    let
      val ctxt = Syntax.init_pretty pp;
      val Thy {pos = _, id = _, axioms = _, defs = defs1, wrappers = (bgs1, ens1)} = thy1;
      val Thy {pos = _, id = _, axioms = _, defs = defs2, wrappers = (bgs2, ens2)} = thy2;

      val axioms' = empty_axioms;
      val defs' = Defs.merge ctxt (defs1, defs2);
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
      val thy = Context.begin_thy Context.pretty_global name imports;
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
    val (_, axioms') = Name_Space.define (Sign.inherit_naming thy ctxt) true axm axioms;
  in axioms' end);


(* dependencies *)

fun dependencies ctxt unchecked def description lhs rhs =
  let
    val thy = Proof_Context.theory_of ctxt;
    val consts = Sign.consts_of thy;
    fun prep const =
      let val Const (c, T) = Sign.no_vars ctxt (Const const)
      in (c, Consts.typargs consts (c, Logic.varifyT_global T)) end;

    val lhs_vars = Term.add_tfreesT (#2 lhs) [];
    val rhs_extras = fold (#2 #> Term.fold_atyps (fn TFree v =>
      if member (op =) lhs_vars v then I else insert (op =) v | _ => I)) rhs [];
    val _ =
      if null rhs_extras then ()
      else error ("Specification depends on extra type variables: " ^
        commas_quote (map (Syntax.string_of_typ ctxt o TFree) rhs_extras) ^
        "\nThe error(s) above occurred in " ^ quote description);
  in Defs.define ctxt unchecked def description (prep lhs) (map prep rhs) end;

fun add_deps ctxt a raw_lhs raw_rhs thy =
  let
    val lhs :: rhs = map (dest_Const o Sign.cert_term thy o Const) (raw_lhs :: raw_rhs);
    val description = if a = "" then #1 lhs ^ " axiom" else a;
  in thy |> map_defs (dependencies ctxt false NONE description lhs rhs) end;

fun add_deps_global a x y thy = add_deps (Syntax.init_pretty_global thy) a x y thy;

fun specify_const decl thy =
  let val (t as Const const, thy') = Sign.declare_const_global decl thy;
  in (t, add_deps_global "" const [] thy') end;


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

fun check_def ctxt thy unchecked overloaded (b, tm) defs =
  let
    val name = Sign.full_name thy b;
    val ((lhs, rhs), _) = Primitive_Defs.dest_def ctxt Term.is_Const (K false) (K false) tm
      handle TERM (msg, _) => error msg;
    val lhs_const = Term.dest_Const (Term.head_of lhs);
    val rhs_consts = fold_aterms (fn Const const => insert (op =) const | _ => I) rhs [];
    val _ = check_overloading ctxt overloaded lhs_const;
  in defs |> dependencies ctxt unchecked (SOME name) name lhs_const rhs_consts end
  handle ERROR msg => cat_error msg (Pretty.string_of (Pretty.block
   [Pretty.str ("The error(s) above occurred in definition " ^ Binding.print b ^ ":"),
    Pretty.fbrk, Pretty.quote (Syntax.pretty_term ctxt tm)]));

in

fun add_def ctxt unchecked overloaded raw_axm thy =
  let val axm = cert_axm ctxt raw_axm in
    thy
    |> map_defs (check_def ctxt thy unchecked overloaded axm)
    |> add_axiom ctxt axm
  end;

end;

end;
