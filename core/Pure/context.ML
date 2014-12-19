(*  Title:      Pure/context.ML
    Author:     Markus Wenzel, TU Muenchen

Generic theory contexts with unique identity, arbitrarily typed data,
monotonic development graph and history support.  Generic proof
contexts with arbitrarily typed data.

Firm naming conventions:
   thy, thy', thy1, thy2: theory
   ctxt, ctxt', ctxt1, ctxt2: Proof.context
   context: Context.generic
*)

signature BASIC_CONTEXT =
sig
  type theory
  exception THEORY of string * theory list
  structure Proof: sig type context end
  structure Proof_Context:
  sig
    val theory_of: Proof.context -> theory
    val init_global: theory -> Proof.context
    val get_global: theory -> string -> Proof.context
  end
end;

signature CONTEXT =
sig
  include BASIC_CONTEXT
  (*theory context*)
  val timing: bool Unsynchronized.ref
  type pretty
  val parents_of: theory -> theory list
  val ancestors_of: theory -> theory list
  val theory_name: theory -> string
  val PureN: string
  val display_names: theory -> string list
  val pretty_thy: theory -> Pretty.T
  val string_of_thy: theory -> string
  val pretty_abbrev_thy: theory -> Pretty.T
  val str_of_thy: theory -> string
  val get_theory: theory -> string -> theory
  val this_theory: theory -> string -> theory
  val eq_thy: theory * theory -> bool
  val subthy: theory * theory -> bool
  val merge: theory * theory -> theory
  val finish_thy: theory -> theory
  val begin_thy: (theory -> pretty) -> string -> theory list -> theory
  (*proof context*)
  val raw_transfer: theory -> Proof.context -> Proof.context
  (*generic context*)
  datatype generic = Theory of theory | Proof of Proof.context
  val cases: (theory -> 'a) -> (Proof.context -> 'a) -> generic -> 'a
  val mapping: (theory -> theory) -> (Proof.context -> Proof.context) -> generic -> generic
  val mapping_result: (theory -> 'a * theory) -> (Proof.context -> 'a * Proof.context) ->
    generic -> 'a * generic
  val the_theory: generic -> theory
  val the_proof: generic -> Proof.context
  val map_theory: (theory -> theory) -> generic -> generic
  val map_proof: (Proof.context -> Proof.context) -> generic -> generic
  val map_theory_result: (theory -> 'a * theory) -> generic -> 'a * generic
  val map_proof_result: (Proof.context -> 'a * Proof.context) -> generic -> 'a * generic
  val theory_map: (generic -> generic) -> theory -> theory
  val proof_map: (generic -> generic) -> Proof.context -> Proof.context
  val theory_of: generic -> theory  (*total*)
  val proof_of: generic -> Proof.context  (*total*)
  (*pretty printing context*)
  val pretty: Proof.context -> pretty
  val pretty_global: theory -> pretty
  val pretty_generic: generic -> pretty
  val pretty_context: (theory -> Proof.context) -> pretty -> Proof.context
  (*thread data*)
  val thread_data: unit -> generic option
  val the_thread_data: unit -> generic
  val set_thread_data: generic option -> unit
  val setmp_thread_data: generic option -> ('a -> 'b) -> 'a -> 'b
  val >> : (generic -> generic) -> unit
  val >>> : (generic -> 'a * generic) -> 'a
end;

signature PRIVATE_CONTEXT =
sig
  include CONTEXT
  structure Theory_Data:
  sig
    val declare: Position.T -> Any.T -> (Any.T -> Any.T) ->
      (pretty -> Any.T * Any.T -> Any.T) -> serial
    val get: serial -> (Any.T -> 'a) -> theory -> 'a
    val put: serial -> ('a -> Any.T) -> 'a -> theory -> theory
  end
  structure Proof_Data:
  sig
    val declare: (theory -> Any.T) -> serial
    val get: serial -> (Any.T -> 'a) -> Proof.context -> 'a
    val put: serial -> ('a -> Any.T) -> 'a -> Proof.context -> Proof.context
  end
end;

structure Context: PRIVATE_CONTEXT =
struct

(*** theory context ***)

(** theory data **)

(* data kinds and access methods *)

val timing = Unsynchronized.ref false;

(*private copy avoids potential conflict of table exceptions*)
structure Datatab = Table(type key = int val ord = int_ord);

datatype pretty = Pretty of Any.T;

local

type kind =
 {pos: Position.T,
  empty: Any.T,
  extend: Any.T -> Any.T,
  merge: pretty -> Any.T * Any.T -> Any.T};

val kinds = Synchronized.var "Theory_Data" (Datatab.empty: kind Datatab.table);

fun invoke name f k x =
  (case Datatab.lookup (Synchronized.value kinds) k of
    SOME kind =>
      if ! timing andalso name <> "" then
        Timing.cond_timeit true ("Theory_Data." ^ name ^ Position.here (#pos kind))
          (fn () => f kind x)
      else f kind x
  | NONE => raise Fail "Invalid theory data identifier");

in

fun invoke_empty k = invoke "" (K o #empty) k ();
val invoke_extend = invoke "extend" #extend;
fun invoke_merge pp = invoke "merge" (fn kind => #merge kind pp);

fun declare_theory_data pos empty extend merge =
  let
    val k = serial ();
    val kind = {pos = pos, empty = empty, extend = extend, merge = merge};
    val _ = Synchronized.change kinds (Datatab.update (k, kind));
  in k end;

val extend_data = Datatab.map invoke_extend;
fun merge_data pp = Datatab.join (invoke_merge pp) o pairself extend_data;

end;



(** datatype theory **)

datatype theory =
  Theory of
   (*identity*)
   {id: serial,                   (*identifier*)
    ids: Inttab.set} *            (*cumulative identifiers -- symbolic body content*)
   (*data*)
   Any.T Datatab.table *          (*body content*)
   (*ancestry*)
   {parents: theory list,         (*immediate predecessors*)
    ancestors: theory list} *     (*all predecessors -- canonical reverse order*)
   (*history*)
   {name: string,                 (*official theory name*)
    stage: int};                  (*counter for anonymous updates*)

exception THEORY of string * theory list;

fun rep_theory (Theory args) = args;

val identity_of = #1 o rep_theory;
val data_of = #2 o rep_theory;
val ancestry_of = #3 o rep_theory;
val history_of = #4 o rep_theory;

fun make_identity id ids = {id = id, ids = ids};
fun make_ancestry parents ancestors = {parents = parents, ancestors = ancestors};
fun make_history name stage = {name = name, stage = stage};

val parents_of = #parents o ancestry_of;
val ancestors_of = #ancestors o ancestry_of;
val theory_name = #name o history_of;


(* names *)

val PureN = "Pure";
val finished = ~1;

fun display_names thy =
  let
    val {name, stage} = history_of thy;
    val name' =
      if stage = finished then name
      else name ^ ":" ^ string_of_int stage;
    val ancestor_names = map theory_name (ancestors_of thy);
  in rev (name' :: ancestor_names) end;

val pretty_thy = Pretty.str_list "{" "}" o display_names;
val string_of_thy = Pretty.string_of o pretty_thy;

fun pretty_abbrev_thy thy =
  let
    val names = display_names thy;
    val n = length names;
    val abbrev = if n > 5 then "..." :: List.drop (names, n - 5) else names;
  in Pretty.str_list "{" "}" abbrev end;

val str_of_thy = Pretty.str_of o pretty_abbrev_thy;

fun get_theory thy name =
  if theory_name thy <> name then
    (case find_first (fn thy' => theory_name thy' = name) (ancestors_of thy) of
      SOME thy' => thy'
    | NONE => error ("Unknown ancestor theory " ^ quote name))
  else if #stage (history_of thy) = finished then thy
  else error ("Unfinished theory " ^ quote name);

fun this_theory thy name =
  if theory_name thy = name then thy
  else get_theory thy name;


(* build ids *)

fun insert_id id ids = Inttab.update (id, ()) ids;

fun merge_ids
    (Theory ({id = id1, ids = ids1, ...}, _, _, _))
    (Theory ({id = id2, ids = ids2, ...}, _, _, _)) =
  Inttab.merge (K true) (ids1, ids2)
  |> insert_id id1
  |> insert_id id2;


(* equality and inclusion *)

val eq_thy = op = o pairself (#id o identity_of);

fun proper_subthy (Theory ({id, ...}, _, _, _), Theory ({ids, ...}, _, _, _)) =
  Inttab.defined ids id;

fun subthy thys = eq_thy thys orelse proper_subthy thys;


(* consistent ancestors *)

fun eq_thy_consistent (thy1, thy2) =
  eq_thy (thy1, thy2) orelse
    (theory_name thy1 = theory_name thy2 andalso
      raise THEORY ("Duplicate theory name", [thy1, thy2]));

fun extend_ancestors thy thys =
  if member eq_thy_consistent thys thy then
    raise THEORY ("Duplicate theory node", thy :: thys)
  else thy :: thys;

val merge_ancestors = merge eq_thy_consistent;


(* trivial merge *)

fun merge (thy1, thy2) =
  if eq_thy (thy1, thy2) then thy1
  else if proper_subthy (thy2, thy1) then thy1
  else if proper_subthy (thy1, thy2) then thy2
  else error (cat_lines ["Attempt to perform non-trivial merge of theories:",
    str_of_thy thy1, str_of_thy thy2]);



(** build theories **)

(* primitives *)

fun create_thy ids data ancestry history =
  Theory (make_identity (serial ()) ids, data, ancestry, history);

val pre_pure_thy =
  create_thy Inttab.empty Datatab.empty (make_ancestry [] []) (make_history PureN 0);

local

fun change_thy finish f thy =
  let
    val Theory ({id, ids}, data, ancestry, {name, stage}) = thy;
    val (data', ancestry') =
      if stage = finished then
        (extend_data data, make_ancestry [thy] (extend_ancestors thy (ancestors_of thy)))
      else (data, ancestry);
    val history' = {name = name, stage = if finish then finished else stage + 1};
    val ids' = insert_id id ids;
    val data'' = f data';
  in create_thy ids' data'' ancestry' history' end;

in

val update_thy = change_thy false;
val extend_thy = update_thy I;
val finish_thy = change_thy true I;

end;


(* named theory nodes *)

fun merge_thys pp (thy1, thy2) =
  let
    val ids = merge_ids thy1 thy2;
    val data = merge_data (pp thy1) (data_of thy1, data_of thy2);
    val ancestry = make_ancestry [] [];
    val history = make_history "" 0;
  in create_thy ids data ancestry history end;

fun maximal_thys thys =
  thys |> filter_out (fn thy => exists (fn thy' => proper_subthy (thy, thy')) thys);

fun begin_thy pp name imports =
  if name = "" then error ("Bad theory name: " ^ quote name)
  else
    let
      val parents = maximal_thys (distinct eq_thy imports);
      val ancestors =
        Library.foldl merge_ancestors ([], map ancestors_of parents)
        |> fold extend_ancestors parents;

      val Theory ({ids, ...}, data, _, _) =
        (case parents of
          [] => error "Missing theory imports"
        | [thy] => extend_thy thy
        | thy :: thys => Library.foldl (merge_thys pp) (thy, thys));

      val ancestry = make_ancestry parents ancestors;
      val history = make_history name 0;
    in create_thy ids data ancestry history end;


(* theory data *)

structure Theory_Data =
struct

val declare = declare_theory_data;

fun get k dest thy =
  (case Datatab.lookup (data_of thy) k of
    SOME x => x
  | NONE => invoke_empty k) |> dest;

fun put k mk x = update_thy (Datatab.update (k, mk x));

end;



(*** proof context ***)

(* datatype Proof.context *)

structure Proof =
struct
  datatype context = Context of Any.T Datatab.table * theory;
end;

fun theory_of_proof (Proof.Context (_, thy)) = thy;
fun data_of_proof (Proof.Context (data, _)) = data;
fun map_prf f (Proof.Context (data, thy)) = Proof.Context (f data, thy);


(* proof data kinds *)

local

val kinds = Synchronized.var "Proof_Data" (Datatab.empty: (theory -> Any.T) Datatab.table);

fun invoke_init k =
  (case Datatab.lookup (Synchronized.value kinds) k of
    SOME init => init
  | NONE => raise Fail "Invalid proof data identifier");

fun init_data thy =
  Datatab.map (fn k => fn _ => invoke_init k thy) (Synchronized.value kinds);

fun init_new_data data thy =
  Datatab.merge (K true) (data, init_data thy);

in

fun raw_transfer thy' (Proof.Context (data, thy)) =
  let
    val _ = subthy (thy, thy') orelse error "Cannot transfer proof context: not a super theory";
    val data' = init_new_data data thy';
  in Proof.Context (data', thy') end;

structure Proof_Context =
struct
  val theory_of = theory_of_proof;
  fun init_global thy = Proof.Context (init_data thy, thy);
  fun get_global thy name = init_global (get_theory thy name);
end;

structure Proof_Data =
struct

fun declare init =
  let
    val k = serial ();
    val _ = Synchronized.change kinds (Datatab.update (k, init));
  in k end;

fun get k dest prf =
  (case Datatab.lookup (data_of_proof prf) k of
    SOME x => x
  | NONE => invoke_init k (Proof_Context.theory_of prf)) |> dest;  (*adhoc value for old theories*)

fun put k mk x = map_prf (Datatab.update (k, mk x));

end;

end;



(*** generic context ***)

datatype generic = Theory of theory | Proof of Proof.context;

fun cases f _ (Theory thy) = f thy
  | cases _ g (Proof prf) = g prf;

fun mapping f g = cases (Theory o f) (Proof o g);
fun mapping_result f g = cases (apsnd Theory o f) (apsnd Proof o g);

val the_theory = cases I (fn _ => error "Ill-typed context: theory expected");
val the_proof = cases (fn _ => error "Ill-typed context: proof expected") I;

fun map_theory f = Theory o f o the_theory;
fun map_proof f = Proof o f o the_proof;

fun map_theory_result f = apsnd Theory o f o the_theory;
fun map_proof_result f = apsnd Proof o f o the_proof;

fun theory_map f = the_theory o f o Theory;
fun proof_map f = the_proof o f o Proof;

val theory_of = cases I Proof_Context.theory_of;
val proof_of = cases Proof_Context.init_global I;


(* pretty printing context *)

exception PRETTY of generic;

val pretty_generic = Pretty o PRETTY;
val pretty = pretty_generic o Proof;
val pretty_global = pretty_generic o Theory;

fun pretty_context init (Pretty (PRETTY context)) = cases init I context;



(** thread data **)

local val tag = Universal.tag () : generic option Universal.tag in

fun thread_data () =
  (case Thread.getLocal tag of
    SOME (SOME context) => SOME context
  | _ => NONE);

fun the_thread_data () =
  (case thread_data () of
    SOME context => context
  | _ => error "Unknown context");

fun set_thread_data context = Thread.setLocal (tag, context);
fun setmp_thread_data context = Library.setmp_thread_data tag (thread_data ()) context;

end;

fun >>> f =
  let
    val (res, context') = f (the_thread_data ());
    val _ = set_thread_data (SOME context');
  in res end;

nonfix >>;
fun >> f = >>> (fn context => ((), f context));

val _ = set_thread_data (SOME (Theory pre_pure_thy));

end;

structure Basic_Context: BASIC_CONTEXT = Context;
open Basic_Context;



(*** type-safe interfaces for data declarations ***)

(** theory data **)

signature THEORY_DATA_PP_ARGS =
sig
  type T
  val empty: T
  val extend: T -> T
  val merge: Context.pretty -> T * T -> T
end;

signature THEORY_DATA_ARGS =
sig
  type T
  val empty: T
  val extend: T -> T
  val merge: T * T -> T
end;

signature THEORY_DATA =
sig
  type T
  val get: theory -> T
  val put: T -> theory -> theory
  val map: (T -> T) -> theory -> theory
end;

functor Theory_Data_PP(Data: THEORY_DATA_PP_ARGS): THEORY_DATA =
struct

type T = Data.T;
exception Data of T;

val kind =
  Context.Theory_Data.declare
    (Position.thread_data ())
    (Data Data.empty)
    (fn Data x => Data (Data.extend x))
    (fn pp => fn (Data x1, Data x2) => Data (Data.merge pp (x1, x2)));

val get = Context.Theory_Data.get kind (fn Data x => x);
val put = Context.Theory_Data.put kind Data;
fun map f thy = put (f (get thy)) thy;

end;

functor Theory_Data(Data: THEORY_DATA_ARGS): THEORY_DATA =
  Theory_Data_PP
  (
    type T = Data.T;
    val empty = Data.empty;
    val extend = Data.extend;
    fun merge _ = Data.merge;
  );



(** proof data **)

signature PROOF_DATA_ARGS =
sig
  type T
  val init: theory -> T
end;

signature PROOF_DATA =
sig
  type T
  val get: Proof.context -> T
  val put: T -> Proof.context -> Proof.context
  val map: (T -> T) -> Proof.context -> Proof.context
end;

functor Proof_Data(Data: PROOF_DATA_ARGS): PROOF_DATA =
struct

type T = Data.T;
exception Data of T;

val kind = Context.Proof_Data.declare (Data o Data.init);

val get = Context.Proof_Data.get kind (fn Data x => x);
val put = Context.Proof_Data.put kind Data;
fun map f prf = put (f (get prf)) prf;

end;



(** generic data **)

signature GENERIC_DATA_ARGS =
sig
  type T
  val empty: T
  val extend: T -> T
  val merge: T * T -> T
end;

signature GENERIC_DATA =
sig
  type T
  val get: Context.generic -> T
  val put: T -> Context.generic -> Context.generic
  val map: (T -> T) -> Context.generic -> Context.generic
end;

functor Generic_Data(Data: GENERIC_DATA_ARGS): GENERIC_DATA =
struct

structure Thy_Data = Theory_Data(Data);
structure Prf_Data = Proof_Data(type T = Data.T val init = Thy_Data.get);

type T = Data.T;

fun get (Context.Theory thy) = Thy_Data.get thy
  | get (Context.Proof prf) = Prf_Data.get prf;

fun put x (Context.Theory thy) = Context.Theory (Thy_Data.put x thy)
  | put x (Context.Proof prf) = Context.Proof (Prf_Data.put x prf);

fun map f ctxt = put (f (get ctxt)) ctxt;

end;

(*hide private interface*)
structure Context: CONTEXT = Context;

