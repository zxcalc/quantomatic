(*  Title:      Pure/Isar/local_theory.ML
    Author:     Makarius

Local theory operations, with abstract target context.
*)

type local_theory = Proof.context;
type generic_theory = Context.generic;

structure Attrib =
struct
  type binding = binding * Token.src list;
end;

signature LOCAL_THEORY =
sig
  type operations
  val assert: local_theory -> local_theory
  val restore: local_theory -> local_theory
  val level: Proof.context -> int
  val assert_bottom: bool -> local_theory -> local_theory
  val mark_brittle: local_theory -> local_theory
  val assert_nonbrittle: local_theory -> local_theory
  val map_contexts: (int -> Proof.context -> Proof.context) -> local_theory -> local_theory
  val background_naming_of: local_theory -> Name_Space.naming
  val map_background_naming: (Name_Space.naming -> Name_Space.naming) ->
    local_theory -> local_theory
  val restore_background_naming: local_theory -> local_theory -> local_theory
  val full_name: local_theory -> binding -> string
  val new_group: local_theory -> local_theory
  val reset_group: local_theory -> local_theory
  val standard_morphism: local_theory -> Proof.context -> morphism
  val standard_form: local_theory -> Proof.context -> (morphism -> 'a) -> 'a
  val raw_theory_result: (theory -> 'a * theory) -> local_theory -> 'a * local_theory
  val raw_theory: (theory -> theory) -> local_theory -> local_theory
  val background_theory_result: (theory -> 'a * theory) -> local_theory -> 'a * local_theory
  val background_theory: (theory -> theory) -> local_theory -> local_theory
  val target_of: local_theory -> Proof.context
  val target: (Proof.context -> Proof.context) -> local_theory -> local_theory
  val target_morphism: local_theory -> morphism
  val propagate_ml_env: generic_theory -> generic_theory
  val operations_of: local_theory -> operations
  val define: (binding * mixfix) * (Attrib.binding * term) -> local_theory ->
    (term * (string * thm)) * local_theory
  val define_internal: (binding * mixfix) * (Attrib.binding * term) -> local_theory ->
    (term * (string * thm)) * local_theory
  val note: Attrib.binding * thm list -> local_theory -> (string * thm list) * local_theory
  val notes: (Attrib.binding * (thm list * Token.src list) list) list ->
    local_theory -> (string * thm list) list * local_theory
  val notes_kind: string -> (Attrib.binding * (thm list * Token.src list) list) list ->
    local_theory -> (string * thm list) list * local_theory
  val abbrev: Syntax.mode -> (binding * mixfix) * term -> local_theory ->
    (term * term) * local_theory
  val declaration: {syntax: bool, pervasive: bool} -> declaration -> local_theory -> local_theory
  val subscription: string * morphism -> (morphism * bool) option -> morphism ->
    local_theory -> local_theory
  val pretty: local_theory -> Pretty.T list
  val add_thms_dynamic: binding * (Context.generic -> thm list) -> local_theory -> local_theory
  val set_defsort: sort -> local_theory -> local_theory
  val type_notation: bool -> Syntax.mode -> (typ * mixfix) list -> local_theory -> local_theory
  val notation: bool -> Syntax.mode -> (term * mixfix) list -> local_theory -> local_theory
  val generic_alias:
    (('a Name_Space.table -> 'a Name_Space.table) -> Context.generic -> Context.generic) ->
    binding -> string -> local_theory -> local_theory
  val class_alias: binding -> class -> local_theory -> local_theory
  val type_alias: binding -> string -> local_theory -> local_theory
  val const_alias: binding -> string -> local_theory -> local_theory
  val init: Name_Space.naming -> operations -> Proof.context -> local_theory
  val exit: local_theory -> Proof.context
  val exit_global: local_theory -> theory
  val exit_result: (morphism -> 'a -> 'b) -> 'a * local_theory -> 'b * Proof.context
  val exit_result_global: (morphism -> 'a -> 'b) -> 'a * local_theory -> 'b * theory
  val init_target: Name_Space.naming -> operations -> (local_theory -> local_theory) ->
    local_theory -> Binding.scope * local_theory
  val open_target: local_theory -> Binding.scope * local_theory
  val close_target: local_theory -> local_theory
end;

structure Local_Theory: LOCAL_THEORY =
struct

(** local theory data **)

(* type lthy *)

type operations =
 {define: bool -> (binding * mixfix) * (Attrib.binding * term) -> local_theory ->
    (term * (string * thm)) * local_theory,
  notes: string ->
    (Attrib.binding * (thm list * Token.src list) list) list ->
    local_theory -> (string * thm list) list * local_theory,
  abbrev: Syntax.mode -> (binding * mixfix) * term -> local_theory ->
    (term * term) * local_theory,
  declaration: {syntax: bool, pervasive: bool} -> declaration -> local_theory -> local_theory,
  subscription: string * morphism -> (morphism * bool) option -> morphism ->
     local_theory -> local_theory,
  pretty: local_theory -> Pretty.T list,
  exit: local_theory -> Proof.context};

type lthy =
 {background_naming: Name_Space.naming,
  operations: operations,
  after_close: local_theory -> local_theory,
  brittle: bool,
  target: Proof.context};

fun make_lthy (background_naming, operations, after_close, brittle, target) : lthy =
  {background_naming = background_naming, operations = operations,
    after_close = after_close, brittle = brittle, target = target};


(* context data *)

structure Data = Proof_Data
(
  type T = lthy list;
  fun init _ = [];
);

fun assert lthy =
  if null (Data.get lthy) then error "Missing local theory context" else lthy;

val bottom_of = List.last o Data.get o assert;
val top_of = hd o Data.get o assert;

fun map_top f =
  assert #>
  Data.map (fn {background_naming, operations, after_close, brittle, target} :: parents =>
    make_lthy (f (background_naming, operations, after_close, brittle, target)) :: parents);

fun restore lthy = #target (top_of lthy) |> Data.put (Data.get lthy);


(* nested structure *)

val level = length o Data.get;  (*1: main target at bottom, >= 2: nested context*)

fun assert_bottom b lthy =
  let
    val _ = assert lthy;
    val b' = level lthy <= 1;
  in
    if b andalso not b' then error "Not at bottom of local theory nesting"
    else if not b andalso b' then error "Already at bottom of local theory nesting"
    else lthy
  end;

fun map_contexts f lthy =
  let val n = level lthy in
    lthy |> (Data.map o map_index)
      (fn (i, {background_naming, operations, after_close, brittle, target}) =>
        make_lthy (background_naming, operations, after_close, brittle,
          target
          |> Context_Position.set_visible false
          |> f (n - i - 1)
          |> Context_Position.restore_visible target))
      |> f n
  end;


(* brittle context -- implicit for nested structures *)

fun mark_brittle lthy =
  if level lthy = 1 then
    map_top (fn (background_naming, operations, after_close, _, target) =>
      (background_naming, operations, after_close, true, target)) lthy
  else lthy;

fun assert_nonbrittle lthy =
  if #brittle (top_of lthy) then error "Brittle local theory context"
  else lthy;


(* naming for background theory *)

val background_naming_of = #background_naming o top_of;

fun map_background_naming f =
  map_top (fn (background_naming, operations, after_close, brittle, target) =>
    (f background_naming, operations, after_close, brittle, target));

val restore_background_naming = map_background_naming o K o background_naming_of;

val full_name = Name_Space.full_name o background_naming_of;

val new_group = map_background_naming Name_Space.new_group;
val reset_group = map_background_naming Name_Space.reset_group;


(* standard morphisms *)

fun standard_morphism lthy ctxt =
  Proof_Context.norm_export_morphism lthy ctxt $>
  Morphism.binding_morphism "Local_Theory.standard_binding"
    (Name_Space.transform_binding (Proof_Context.naming_of lthy));

fun standard_form lthy ctxt x =
  Morphism.form (Morphism.transform (standard_morphism lthy ctxt) x);


(* background theory *)

fun raw_theory_result f lthy =
  let
    val (res, thy') = f (Proof_Context.theory_of lthy);
    val lthy' = map_contexts (K (Proof_Context.transfer thy')) lthy;
  in (res, lthy') end;

fun raw_theory f = #2 o raw_theory_result (f #> pair ());

fun background_theory_result f lthy =
  let
    val naming =
      background_naming_of lthy
      |> Name_Space.transform_naming (Proof_Context.naming_of lthy);
  in
    lthy |> raw_theory_result (fn thy =>
      thy
      |> Sign.map_naming (K naming)
      |> f
      ||> Sign.restore_naming thy)
  end;

fun background_theory f = #2 o background_theory_result (f #> pair ());


(* target contexts *)

val target_of = #target o bottom_of;

fun target f lthy =
  let
    val ctxt = target_of lthy;
    val ctxt' = ctxt
      |> Context_Position.set_visible false
      |> f
      |> Context_Position.restore_visible ctxt;
    val thy' = Proof_Context.theory_of ctxt';
  in map_contexts (fn 0 => K ctxt' | _ => Proof_Context.transfer thy') lthy end;

fun target_morphism lthy = standard_morphism lthy (target_of lthy);

fun propagate_ml_env (context as Context.Proof lthy) =
      let val inherit = ML_Env.inherit context in
        lthy
        |> background_theory (Context.theory_map inherit)
        |> map_contexts (K (Context.proof_map inherit))
        |> Context.Proof
      end
  | propagate_ml_env context = context;



(** operations **)

val operations_of = #operations o top_of;

fun operation f lthy = f (operations_of lthy) lthy;
fun operation2 f x y = operation (fn ops => f ops x y);


(* primitives *)

val pretty = operation #pretty;
val abbrev = operation2 #abbrev;
val define = operation2 #define false;
val define_internal = operation2 #define true;
val notes_kind = operation2 #notes;
val declaration = operation2 #declaration;
fun subscription dep_morph mixin export =
  assert_bottom true #> operation (fn ops => #subscription ops dep_morph mixin export);


(* theorems *)

val notes = notes_kind "";
fun note (a, ths) = notes [(a, [(ths, [])])] #>> the_single;

fun add_thms_dynamic (binding, f) lthy =
  lthy
  |> background_theory_result (fn thy => thy
      |> Global_Theory.add_thms_dynamic' (Sign.inherit_naming thy lthy) (binding, f))
  |-> (fn name =>
    map_contexts (fn _ => fn ctxt =>
      Proof_Context.transfer_facts (Proof_Context.theory_of ctxt) ctxt) #>
    declaration {syntax = false, pervasive = false} (fn phi =>
      let val binding' = Morphism.binding phi binding in
        Context.mapping
          (Global_Theory.alias_fact binding' name)
          (Proof_Context.fact_alias binding' name)
      end));


(* default sort *)

fun set_defsort S =
  declaration {syntax = true, pervasive = false}
    (K (Context.mapping (Sign.set_defsort S) (Proof_Context.set_defsort S)));


(* notation *)

fun type_notation add mode raw_args lthy =
  let
    val args = map (apfst (Logic.type_map (Assumption.export_term lthy (target_of lthy)))) raw_args;
  in
    declaration {syntax = true, pervasive = false}
      (Proof_Context.generic_type_notation add mode args) lthy
  end;

fun notation add mode raw_args lthy =
  let
    val args = map (apfst (Assumption.export_term lthy (target_of lthy))) raw_args
  in
    declaration {syntax = true, pervasive = false}
      (Proof_Context.generic_notation add mode args) lthy
  end;


(* name space aliases *)

fun generic_alias app b name =
  declaration {syntax = false, pervasive = false} (fn phi => fn context =>
    let
      val naming = Name_Space.naming_of context;
      val b' = Morphism.binding phi b;
    in app (Name_Space.alias_table naming b' name) context end);

fun syntax_alias global_alias local_alias b name =
  declaration {syntax = true, pervasive = false} (fn phi =>
    let val b' = Morphism.binding phi b
    in Context.mapping (global_alias b' name) (local_alias b' name) end);

val class_alias = syntax_alias Sign.class_alias Proof_Context.class_alias;
val type_alias = syntax_alias Sign.type_alias Proof_Context.type_alias;
val const_alias = syntax_alias Sign.const_alias Proof_Context.const_alias;



(** manage targets **)

(* outermost target *)

fun init background_naming operations target =
  target |> Data.map
    (fn [] => [make_lthy (background_naming, operations, I, false, target)]
      | _ => error "Local theory already initialized");

val exit = operation #exit;
val exit_global = Proof_Context.theory_of o exit;

fun exit_result f (x, lthy) =
  let
    val ctxt = exit lthy;
    val phi = standard_morphism lthy ctxt;
  in (f phi x, ctxt) end;

fun exit_result_global f (x, lthy) =
  let
    val thy = exit_global lthy;
    val thy_ctxt = Proof_Context.init_global thy;
    val phi = standard_morphism lthy thy_ctxt;
  in (f phi x, thy) end;


(* nested targets *)

fun init_target background_naming operations after_close lthy =
  let
    val _ = assert lthy;
    val (scope, target) = Proof_Context.new_scope lthy;
    val lthy' =
      target
      |> Data.map (cons (make_lthy (background_naming, operations, after_close, true, target)));
  in (scope, lthy') end;

fun open_target lthy =
  init_target (background_naming_of lthy) (operations_of lthy) I lthy;

fun close_target lthy =
  let
    val _ = assert_bottom false lthy;
    val ({after_close, ...} :: rest) = Data.get lthy;
  in lthy |> Data.put rest |> restore |> after_close end;

end;
