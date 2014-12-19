(*  Title:      Pure/global_theory.ML
    Author:     Makarius

Global theory content: stored facts.
*)

signature GLOBAL_THEORY =
sig
  val facts_of: theory -> Facts.T
  val check_fact: theory -> xstring * Position.T -> string
  val intern_fact: theory -> xstring -> string
  val defined_fact: theory -> string -> bool
  val hide_fact: bool -> string -> theory -> theory
  val get_thms: theory -> xstring -> thm list
  val get_thm: theory -> xstring -> thm
  val all_thms_of: theory -> bool -> (string * thm) list
  val map_facts: ('a -> 'b) -> ('c * ('a list * 'd) list) list -> ('c * ('b list * 'd) list) list
  val burrow_fact: ('a list -> 'b list) -> ('a list * 'c) list -> ('b list * 'c) list
  val burrow_facts: ('a list -> 'b list) ->
    ('c * ('a list * 'd) list) list -> ('c * ('b list * 'd) list) list
  val name_multi: string -> 'a list -> (string * 'a) list
  val name_thm: bool -> bool -> string -> thm -> thm
  val name_thms: bool -> bool -> string -> thm list -> thm list
  val name_thmss: bool -> string -> (thm list * 'a) list -> (thm list * 'a) list
  val store_thms: binding * thm list -> theory -> thm list * theory
  val store_thm: binding * thm -> theory -> thm * theory
  val store_thm_open: binding * thm -> theory -> thm * theory
  val add_thms: ((binding * thm) * attribute list) list -> theory -> thm list * theory
  val add_thm: (binding * thm) * attribute list -> theory -> thm * theory
  val add_thmss: ((binding * thm list) * attribute list) list -> theory -> thm list list * theory
  val add_thms_dynamic: binding * (Context.generic -> thm list) -> theory -> theory
  val note_thmss: string -> (Thm.binding * (thm list * attribute list) list) list
    -> theory -> (string * thm list) list * theory
  val add_defs: bool -> ((binding * term) * attribute list) list ->
    theory -> thm list * theory
  val add_defs_unchecked: bool -> ((binding * term) * attribute list) list ->
    theory -> thm list * theory
  val add_defs_cmd: bool -> ((binding * string) * attribute list) list ->
    theory -> thm list * theory
  val add_defs_unchecked_cmd: bool -> ((binding * string) * attribute list) list ->
    theory -> thm list * theory
end;

structure Global_Theory: GLOBAL_THEORY =
struct

(** theory data **)

structure Data = Theory_Data
(
  type T = Facts.T;
  val empty = Facts.empty;
  val extend = I;
  val merge = Facts.merge;
);

val facts_of = Data.get;

fun check_fact thy = Facts.check (Context.Theory thy) (facts_of thy);
val intern_fact = Facts.intern o facts_of;
val defined_fact = Facts.defined o facts_of;

fun hide_fact fully name = Data.map (Facts.hide fully name);


(* retrieve theorems *)

fun get_thms thy xname =
  #2 (Facts.retrieve (Context.Theory thy) (facts_of thy) (xname, Position.none));

fun get_thm thy xname =
  Facts.the_single (xname, Position.none) (get_thms thy xname);

fun all_thms_of thy verbose =
  let
    val facts = facts_of thy;
    fun add (name, ths) =
      if not verbose andalso Facts.is_concealed facts name then I
      else append (map (`(Thm.get_name_hint)) ths);
  in Facts.fold_static add facts [] end;



(** store theorems **)

(* fact specifications *)

fun map_facts f = map (apsnd (map (apfst (map f))));
fun burrow_fact f = split_list #>> burrow f #> op ~~;
fun burrow_facts f = split_list ##> burrow (burrow_fact f) #> op ~~;


(* naming *)

fun name_multi name [x] = [(name, x)]
  | name_multi "" xs = map (pair "") xs
  | name_multi name xs = map_index (fn (i, x) => (name ^ "_" ^ string_of_int (i + 1), x)) xs;

fun name_thm pre official name thm = thm
  |> (if not official orelse pre andalso Thm.derivation_name thm <> "" then I
      else Thm.name_derivation name)
  |> (if name = "" orelse pre andalso Thm.has_name_hint thm then I
      else Thm.put_name_hint name);

fun name_thms pre official name xs =
  map (uncurry (name_thm pre official)) (name_multi name xs);

fun name_thmss official name fact =
  burrow_fact (name_thms true official name) fact;


(* enter_thms *)

fun register_proofs thms thy = (thms, Thm.register_proofs thms thy);

fun enter_thms pre_name post_name app_att (b, thms) thy =
  if Binding.is_empty b
  then app_att thms thy |-> register_proofs
  else
    let
      val name = Sign.full_name thy b;
      val (thms', thy') = app_att (pre_name name thms) thy |>> post_name name |-> register_proofs;
      val thms'' = map (Thm.transfer thy') thms';
      val thy'' = thy' |> Data.map
        (Facts.add_static (Context.Theory thy') {strict = true, index = false} (b, thms'') #> snd);
    in (thms'', thy'') end;


(* store_thm(s) *)

fun store_thms (b, thms) =
  enter_thms (name_thms true true) (name_thms false true) pair (b, thms);

fun store_thm (b, th) = store_thms (b, [th]) #>> the_single;

fun store_thm_open (b, th) =
  enter_thms (name_thms true false) (name_thms false false) pair (b, [th]) #>> the_single;


(* add_thms(s) *)

fun add_thms_atts pre_name ((b, thms), atts) =
  enter_thms pre_name (name_thms false true) (fold_map (Thm.theory_attributes atts)) (b, thms);

fun gen_add_thmss pre_name =
  fold_map (add_thms_atts pre_name);

fun gen_add_thms pre_name args =
  apfst (map hd) o gen_add_thmss pre_name (map (apfst (apsnd single)) args);

val add_thmss = gen_add_thmss (name_thms true true);
val add_thms = gen_add_thms (name_thms true true);
val add_thm = yield_singleton add_thms;


(* add_thms_dynamic *)

fun add_thms_dynamic (b, f) thy = thy
  |> Data.map (Facts.add_dynamic (Context.Theory thy) (b, f) #> snd);


(* note_thmss *)

fun note_thmss kind = fold_map (fn ((b, more_atts), facts) => fn thy =>
  let
    val name = Sign.full_name thy b;
    fun app (ths, atts) =
      fold_map (Thm.theory_attributes (surround (Thm.kind kind) (atts @ more_atts))) ths;
    val (thms, thy') =
      enter_thms (name_thmss true) (name_thms false true) (apfst flat oo fold_map app)
        (b, facts) thy;
  in ((name, thms), thy') end);


(* store axioms as theorems *)

local

fun no_read _ (_, t) = t;

fun read ctxt (b, str) =
  Syntax.read_prop ctxt str handle ERROR msg =>
    cat_error msg ("The error(s) above occurred in definition " ^ Binding.print b);

fun add prep unchecked overloaded = fold_map (fn ((b, raw_prop), atts) => fn thy =>
  let
    val ctxt = Syntax.init_pretty_global thy;
    val prop = prep ctxt (b, raw_prop);
    val ((_, def), thy') = Thm.add_def ctxt unchecked overloaded (b, prop) thy;
    val thm = def
      |> Thm.forall_intr_frees
      |> Thm.forall_elim_vars 0
      |> Thm.varifyT_global;
  in yield_singleton (gen_add_thms (K I)) ((b, thm), atts) thy' end);

in

val add_defs = add no_read false;
val add_defs_unchecked = add no_read true;
val add_defs_cmd = add read false;
val add_defs_unchecked_cmd = add read true;

end;

end;
