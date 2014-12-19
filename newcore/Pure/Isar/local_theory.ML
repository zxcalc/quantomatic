(*  Title:      Pure/Isar/local_theory.ML
    Author:     Makarius

Local theory operations, with abstract target context.
*)

type local_theory = Proof.context;
type generic_theory = Context.generic;

signature LOCAL_THEORY =
sig
  type operations
  val assert: local_theory -> local_theory
  val restore: local_theory -> local_theory
  val level: Proof.context -> int
  val assert_bottom: bool -> local_theory -> local_theory
  val assert_nonbrittle: local_theory -> local_theory
  val open_target: Name_Space.naming -> operations -> (local_theory -> local_theory) ->
    local_theory -> local_theory
  val close_target: local_theory -> local_theory
  val map_contexts: (int -> Proof.context -> Proof.context) -> local_theory -> local_theory
  val naming_of: local_theory -> Name_Space.naming
  val full_name: local_theory -> binding -> string
  val map_naming: (Name_Space.naming -> Name_Space.naming) -> local_theory -> local_theory
  val conceal: local_theory -> local_theory
  val new_group: local_theory -> local_theory
  val reset_group: local_theory -> local_theory
  val restore_naming: local_theory -> local_theory -> local_theory
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
  val notes: (Attrib.binding * (thm list * Attrib.src list) list) list ->
    local_theory -> (string * thm list) list * local_theory
  val notes_kind: string -> (Attrib.binding * (thm list * Attrib.src list) list) list ->
    local_theory -> (string * thm list) list * local_theory
  val abbrev: Syntax.mode -> (binding * mixfix) * term -> local_theory ->
    (term * term) * local_theory
  val declaration: {syntax: bool, pervasive: bool} -> declaration -> local_theory -> local_theory
  val subscription: string * morphism -> (morphism * bool) option -> morphism ->
    local_theory -> local_theory
  val pretty: local_theory -> Pretty.T list
  val set_defsort: sort -> local_theory -> local_theory
  val type_notation: bool -> Syntax.mode -> (typ * mixfix) list -> local_theory -> local_theory
  val notation: bool -> Syntax.mode -> (term * mixfix) list -> local_theory -> local_theory
  val class_alias: binding -> class -> local_theory -> local_theory
  val type_alias: binding -> string -> local_theory -> local_theory
  val const_alias: binding -> string -> local_theory -> local_theory
  val activate: string * morphism -> (morphism * bool) option -> morphism ->
    local_theory -> local_theory
  val activate_nonbrittle: string * morphism -> (morphism * bool) option -> morphism ->
    local_theory -> local_theory
  val init: Name_Space.naming -> operations -> Proof.context -> local_theory
  val exit: local_theory -> Proof.context
  val exit_global: local_theory -> theory
  val exit_result: (morphism -> 'a -> 'b) -> 'a * local_theory -> 'b * Proof.context
  val exit_result_global: (morphism -> 'a -> 'b) -> 'a * local_theory -> 'b * theory
end;

structure Local_Theory: LOCAL_THEORY =
struct

(** local theory data **)

(* type lthy *)

type operations =
 {define: bool -> (binding * mixfix) * (Attrib.binding * term) -> local_theory ->
    (term * (string * thm)) * local_theory,
  notes: string ->
    (Attrib.binding * (thm list * Attrib.src list) list) list ->
    local_theory -> (string * thm list) list * local_theory,
  abbrev: Syntax.mode -> (binding * mixfix) * term -> local_theory ->
    (term * term) * local_theory,
  declaration: {syntax: bool, pervasive: bool} -> declaration -> local_theory -> local_theory,
  subscription: string * morphism -> (morphism * bool) option -> morphism ->
     local_theory -> local_theory,
  pretty: local_theory -> Pretty.T list,
  exit: local_theory -> Proof.context};

type lthy =
 {naming: Name_Space.naming,
  operations: operations,
  after_close: local_theory -> local_theory,
  brittle: bool,
  target: Proof.context};

fun make_lthy (naming, operations, after_close, brittle, target) : lthy =
  {naming = naming, operations = operations, after_close = after_close, brittle = brittle, target = target};


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

fun map_bottom f =
  assert #>
  Data.map (fn {naming, operations, after_close, brittle, target} :: parents =>
    make_lthy (f (naming, operations, after_close, brittle, target)) :: parents);

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

fun open_target naming operations after_close target =
  assert target
  |> Data.map (cons (make_lthy (naming, operations, after_close, true, target)));

fun close_target lthy =
  let
    val _ = assert_bottom false lthy;
    val ({after_close, ...} :: rest) = Data.get lthy;
  in lthy |> Data.put rest |> restore |> after_close end;

fun map_contexts f lthy =
  let val n = level lthy in
    lthy |> (Data.map o map_index) (fn (i, {naming, operations, after_close, brittle, target}) =>
      make_lthy (naming, operations, after_close, brittle,
        target
        |> Context_Position.set_visible false
        |> f (n - i - 1)
        |> Context_Position.restore_visible target))
    |> f n
  end;


(* brittle context -- implicit for nested structures *)

fun mark_brittle lthy =
  if level lthy = 1
  then map_bottom (fn (naming, operations, after_close, brittle, target) =>
    (naming, operations, after_close, true, target)) lthy
  else lthy;

fun assert_nonbrittle lthy =
  if #brittle (top_of lthy)
  then error "Brittle local theory context"
  else lthy;


(* naming *)

val naming_of = #naming o top_of;
val full_name = Name_Space.full_name o naming_of;

fun map_naming f =
  map_bottom (fn (naming, operations, after_close, brittle, target) =>
    (f naming, operations, after_close, brittle, target));

val conceal = map_naming Name_Space.conceal;
val new_group = map_naming Name_Space.new_group;
val reset_group = map_naming Name_Space.reset_group;

val restore_naming = map_naming o K o naming_of;


(* standard morphisms *)

fun standard_morphism lthy ctxt =
  Proof_Context.norm_export_morphism lthy ctxt $>
  Morphism.binding_morphism "Local_Theory.standard_binding"
    (Name_Space.transform_binding (naming_of lthy));

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
  lthy |> raw_theory_result (fn thy =>
    thy
    |> Sign.map_naming (K (naming_of lthy))
    |> f
    ||> Sign.restore_naming thy);

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


(* primitives *)

fun operation f lthy = f (operations_of lthy) lthy;
fun operation2 f x y = operation (fn ops => f ops x y);

val pretty = operation #pretty;
val abbrev = operation2 #abbrev;
val define = operation2 #define false;
val define_internal = operation2 #define true;
val notes_kind = operation2 #notes;
val declaration = operation2 #declaration;
fun subscription dep_morph mixin export =
  assert_bottom true #> operation (fn ops => #subscription ops dep_morph mixin export);


(* basic derived operations *)

val notes = notes_kind "";
fun note (a, ths) = notes [(a, [(ths, [])])] #>> the_single;

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

fun alias global_alias local_alias b name =
  declaration {syntax = true, pervasive = false} (fn phi =>
    let val b' = Morphism.binding phi b
    in Context.mapping (global_alias b' name) (local_alias b' name) end);

val class_alias = alias Sign.class_alias Proof_Context.class_alias;
val type_alias = alias Sign.type_alias Proof_Context.type_alias;
val const_alias = alias Sign.const_alias Proof_Context.const_alias;


(* activation of locale fragments *)

fun activate_nonbrittle dep_morph mixin export =
  map_bottom (fn (naming, operations, after_close, brittle, target) =>
    (naming, operations, after_close, brittle,
      (Context.proof_map ooo Locale.add_registration) dep_morph mixin export target));

fun activate dep_morph mixin export =
  mark_brittle #> activate_nonbrittle dep_morph mixin export;



(** init and exit **)

(* init *)

fun init naming operations target =
  target |> Data.map
    (fn [] => [make_lthy (naming, operations, I, false, target)]
      | _ => error "Local theory already initialized");


(* exit *)

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

end;
