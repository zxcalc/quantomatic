(*  Title:      Pure/context_position.ML
    Author:     Makarius

Context position visibility flag.
*)

signature CONTEXT_POSITION =
sig
  val is_visible_generic: Context.generic -> bool
  val is_visible: Proof.context -> bool
  val is_visible_global: theory -> bool
  val set_visible: bool -> Proof.context -> Proof.context
  val set_visible_global: bool -> theory -> theory
  val is_really_visible: Proof.context -> bool
  val not_really: Proof.context -> Proof.context
  val restore_visible: Proof.context -> Proof.context -> Proof.context
  val restore_visible_global: theory -> theory -> theory
  val is_reported_generic: Context.generic -> Position.T -> bool
  val is_reported: Proof.context -> Position.T -> bool
  val report_generic: Context.generic -> Position.T -> Markup.T -> unit
  val reported_text: Proof.context -> Position.T -> Markup.T -> string -> string
  val report_text: Proof.context -> Position.T -> Markup.T -> string -> unit
  val report: Proof.context -> Position.T -> Markup.T -> unit
  val reports_text: Proof.context -> Position.report_text list -> unit
  val reports: Proof.context -> Position.report list -> unit
end;

structure Context_Position: CONTEXT_POSITION =
struct

structure Data = Generic_Data
(
  type T = bool option * bool option;  (*really, visible*)
  val empty: T = (NONE, NONE);
  val extend = I;
  fun merge ((a, b), (a', b')) : T = (merge_options (a, a'), merge_options (b, b'));
);

val is_visible_generic = the_default true o snd o Data.get;
val is_visible = is_visible_generic o Context.Proof;
val is_visible_global = is_visible_generic o Context.Theory;

val set_visible = Context.proof_map o Data.map o apsnd o K o SOME;
val set_visible_global = Context.theory_map o Data.map o apsnd o K o SOME;

val is_really = the_default true o fst o Data.get o Context.Proof;
fun is_really_visible ctxt = is_really ctxt andalso is_visible ctxt;
val not_really = Context.proof_map (Data.map (apfst (K (SOME false))));

val restore_visible = Context.proof_map o Data.put o Data.get o Context.Proof;
val restore_visible_global = Context.theory_map o Data.put o Data.get o Context.Theory;

fun is_reported_generic context pos = is_visible_generic context andalso Position.is_reported pos;
fun is_reported ctxt pos = is_visible ctxt andalso Position.is_reported pos;

fun report_generic context pos markup =
  if is_reported_generic context pos then
    Output.report [Position.reported_text pos markup ""]
  else ();

fun reported_text ctxt pos markup txt =
  if is_reported ctxt pos then Position.reported_text pos markup txt else "";

fun report_text ctxt pos markup txt = Output.report [reported_text ctxt pos markup txt];
fun report ctxt pos markup = report_text ctxt pos markup "";

fun reports_text ctxt reps = if is_visible ctxt then Position.reports_text reps else ();
fun reports ctxt reps = if is_visible ctxt then Position.reports reps else ();

end;
