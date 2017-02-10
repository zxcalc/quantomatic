(*  Title:      Pure/Isar/named_target.ML
    Author:     Makarius
    Author:     Florian Haftmann, TU Muenchen

Targets for theory, locale, class -- at the bottom the nested structure.
*)

signature NAMED_TARGET =
sig
  val is_theory: local_theory -> bool
  val locale_of: local_theory -> string option
  val bottom_locale_of: local_theory -> string option
  val class_of: local_theory -> string option
  val init: (local_theory -> local_theory) option -> string -> theory -> local_theory
  val theory_init: theory -> local_theory
  val theory_map: (local_theory -> local_theory) -> theory -> theory
  val begin: xstring * Position.T -> theory -> local_theory
  val exit: local_theory -> theory
  val switch: (xstring * Position.T) option -> Context.generic ->
    (local_theory -> Context.generic) * local_theory
end;

structure Named_Target: NAMED_TARGET =
struct

(* context data *)

structure Data = Proof_Data
(
  type T = (string * bool) option;
  fun init _ = NONE;
);

val get_bottom_data = Data.get;

fun get_data lthy =
  if Local_Theory.level lthy = 1
  then get_bottom_data lthy
  else NONE;

fun is_theory lthy =
  (case get_data lthy of
    SOME ("", _) => true
  | _ => false);

fun target_of lthy =
  (case get_data lthy of
    NONE => error "Not in a named target"
  | SOME (target, _) => target);

fun locale_name_of NONE = NONE
  | locale_name_of (SOME ("", _)) = NONE
  | locale_name_of (SOME (locale, _)) = SOME locale;

val locale_of = locale_name_of o get_data;

val bottom_locale_of = locale_name_of o get_bottom_data;

fun class_of lthy =
  (case get_data lthy of
    SOME (class, true) => SOME class
  | _ => NONE);


(* operations *)

fun locale_foundation locale (((b, U), mx), (b_def, rhs)) params =
  Generic_Target.background_foundation (((b, U), NoSyn), (b_def, rhs)) params
  #-> (fn (lhs, def) => Generic_Target.locale_const locale Syntax.mode_default ((b, mx), lhs)
    #> pair (lhs, def));

fun class_foundation class (((b, U), mx), (b_def, rhs)) params =
  Generic_Target.background_foundation (((b, U), NoSyn), (b_def, rhs)) params
  #-> (fn (lhs, def) => Class.const class ((b, mx), lhs) params
    #> pair (lhs, def));

fun foundation ("", _) = Generic_Target.theory_target_foundation
  | foundation (locale, false) = locale_foundation locale
  | foundation (class, true) = class_foundation class;

fun notes ("", _) = Generic_Target.theory_target_notes
  | notes (locale, _) = Generic_Target.locale_target_notes locale;

fun abbrev ("", _) = Generic_Target.theory_abbrev
  | abbrev (locale, false) = Generic_Target.locale_abbrev locale
  | abbrev (class, true) = Class.abbrev class;

fun declaration ("", _) _ decl = Generic_Target.theory_declaration decl
  | declaration (locale, _) flags decl = Generic_Target.locale_declaration locale flags decl;

fun theory_registration ("", _) = Generic_Target.theory_registration
  | theory_registration _ = (fn _ => error "Not possible in theory target");

fun locale_dependency ("", false) = (fn _ => error "Not possible in locale target")
  | locale_dependency ("", true) = (fn _ => error "Not possible in class target")
  | locale_dependency (locale, _) = Generic_Target.locale_dependency locale;

fun pretty (target, is_class) ctxt =
  if target = "" then
    [Pretty.block [Pretty.keyword1 "theory", Pretty.brk 1,
      Pretty.str (Context.theory_name (Proof_Context.theory_of ctxt))]]
  else if is_class then Class.pretty_specification (Proof_Context.theory_of ctxt) target
  else
    (* FIXME pretty locale content *)
    let
      val target_name = [Pretty.keyword1 "locale", Pretty.brk 1, Locale.pretty_name ctxt target];
      val fixes =
        map (fn (x, T) => (Binding.name x, SOME T, NoSyn))
          (#1 (Proof_Context.inferred_fixes ctxt));
      val assumes =
        map (fn A => (Binding.empty_atts, [(Thm.term_of A, [])]))
          (Assumption.all_assms_of ctxt);
      val elems =
        (if null fixes then [] else [Element.Fixes fixes]) @
        (if null assumes then [] else [Element.Assumes assumes]);
    in
      if null elems then [Pretty.block target_name]
      else [Pretty.block (Pretty.fbreaks (Pretty.block (target_name @ [Pretty.str " ="]) ::
        map (Pretty.chunks o Element.pretty_ctxt ctxt) elems))]
    end;


(* init *)

fun make_name_data _ "" = ("", false)
  | make_name_data thy target =
      if Locale.defined thy target
      then (target, Class.is_class thy target)
      else error ("No such locale: " ^ quote target);

fun init_context ("", _) = Proof_Context.init_global
  | init_context (locale, false) = Locale.init locale
  | init_context (class, true) = Class.init class;

fun init before_exit target thy =
  let
    val name_data = make_name_data thy target;
    val background_naming =
      Sign.naming_of thy |> Name_Space.mandatory_path (Long_Name.base_name target);
  in
    thy
    |> Sign.change_begin
    |> init_context name_data
    |> is_none before_exit ? Data.put (SOME name_data)
    |> Local_Theory.init background_naming
       {define = Generic_Target.define (foundation name_data),
        notes = Generic_Target.notes (notes name_data),
        abbrev = abbrev name_data,
        declaration = declaration name_data,
        theory_registration = theory_registration name_data,
        locale_dependency = locale_dependency name_data,
        pretty = pretty name_data,
        exit = the_default I before_exit
          #> Local_Theory.target_of #> Sign.change_end_local}
  end;

val theory_init = init NONE "";
fun theory_map f = theory_init #> f #> Local_Theory.exit_global;


(* toplevel interaction *)

fun begin ("-", _) thy = theory_init thy
  | begin target thy = init NONE (Locale.check thy target) thy;

val exit = Local_Theory.assert_bottom true #> Local_Theory.exit_global;

fun switch NONE (Context.Theory thy) =
      (Context.Theory o exit, theory_init thy)
  | switch (SOME name) (Context.Theory thy) =
      (Context.Theory o exit, begin name thy)
  | switch NONE (Context.Proof lthy) =
      (Context.Proof o Local_Theory.reset, lthy)
  | switch (SOME name) (Context.Proof lthy) =
      (Context.Proof o init NONE (target_of lthy) o exit,
        (begin name o exit o Local_Theory.assert_nonbrittle) lthy);

end;
