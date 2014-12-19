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
  val init: string -> theory -> local_theory
  val theory_init: theory -> local_theory
  val theory_like_init: (local_theory -> local_theory) -> theory -> local_theory
  val begin: xstring * Position.T -> theory -> local_theory
  val exit: local_theory -> theory
  val switch: (xstring * Position.T) option -> Context.generic
    -> (local_theory -> Context.generic) * local_theory
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
  case get_data lthy of
    SOME ("", _) => true
  | _ => false;

fun target_of lthy =
  case get_data lthy of
    NONE => error "Not in a named target"
  | SOME (target, _) => target;

fun locale_name_of NONE = NONE
  | locale_name_of (SOME ("", _)) = NONE
  | locale_name_of (SOME (locale, _)) = SOME locale;

val locale_of = locale_name_of o get_data;

val bottom_locale_of = locale_name_of o get_bottom_data;

fun class_of lthy =
  case get_data lthy of
    SOME (class, true) => SOME class
  | _ => NONE;


(* define *)

fun locale_foundation locale (((b, U), mx), (b_def, rhs)) params =
  Generic_Target.background_foundation (((b, U), NoSyn), (b_def, rhs)) params
  #-> (fn (lhs, def) => Generic_Target.locale_const locale Syntax.mode_default ((b, mx), lhs)
    #> pair (lhs, def));

fun class_foundation class (((b, U), mx), (b_def, rhs)) params =
  Generic_Target.background_foundation (((b, U), NoSyn), (b_def, rhs)) params
  #-> (fn (lhs, def) => Class.const class ((b, mx), lhs) params
    #> pair (lhs, def));

fun foundation ("", _) = Generic_Target.theory_foundation
  | foundation (locale, false) = locale_foundation locale
  | foundation (class, true) = class_foundation class;


(* notes *)

fun notes ("", _) = Generic_Target.theory_notes
  | notes (locale, _) = Generic_Target.locale_notes locale;


(* abbrev *)

fun locale_abbrev locale prmode (b, mx) global_rhs params =
  Generic_Target.background_abbrev (b, global_rhs) (snd params)
  #-> (fn (lhs, _) => Generic_Target.locale_const locale prmode ((b, mx), lhs));

fun class_abbrev class prmode (b, mx) global_rhs params =
  Generic_Target.background_abbrev (b, global_rhs) (snd params)
  #-> (fn (lhs, rhs) => Class.abbrev class prmode ((b, mx), lhs) rhs params);

fun abbrev ("", _) = Generic_Target.theory_abbrev
  | abbrev (locale, false) = locale_abbrev locale
  | abbrev (class, true) = class_abbrev class;


(* declaration *)

fun declaration ("", _) flags decl = Generic_Target.theory_declaration decl
  | declaration (locale, _) flags decl = Generic_Target.locale_declaration locale flags decl;


(* subscription *)

fun subscription ("", _) = Generic_Target.theory_registration
  | subscription (locale, _) = Generic_Target.locale_dependency locale;


(* pretty *)

fun pretty (target, is_class) ctxt =
  let
    val target_name =
      [Pretty.keyword1 (if is_class then "class" else "locale"), Pretty.brk 1,
        Locale.pretty_name ctxt target];
    val fixes =
      map (fn (x, T) => (Binding.name x, SOME T, NoSyn))
        (#1 (Proof_Context.inferred_fixes ctxt));
    val assumes =
      map (fn A => (Attrib.empty_binding, [(Thm.term_of A, [])]))
        (Assumption.all_assms_of ctxt);
    val elems =
      (if null fixes then [] else [Element.Fixes fixes]) @
      (if null assumes then [] else [Element.Assumes assumes]);
    val body_elems =
      if target = "" then []
      else if null elems then [Pretty.block target_name]
      else [Pretty.block (Pretty.fbreaks (Pretty.block (target_name @ [Pretty.str " ="]) ::
        map (Pretty.chunks o Element.pretty_ctxt ctxt) elems))];
  in
    Pretty.block [Pretty.keyword1 "theory", Pretty.brk 1,
      Pretty.str (Context.theory_name (Proof_Context.theory_of ctxt))] :: body_elems
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

fun gen_init before_exit target thy =
  let
    val name_data = make_name_data thy target;
    val naming = Sign.naming_of thy
      |> Name_Space.mandatory_path (Long_Name.base_name target);
  in
    thy
    |> Sign.change_begin
    |> init_context name_data
    |> is_none before_exit ? Data.put (SOME name_data)
    |> Local_Theory.init naming
       {define = Generic_Target.define (foundation name_data),
        notes = Generic_Target.notes (notes name_data),
        abbrev = Generic_Target.abbrev (abbrev name_data),
        declaration = declaration name_data,
        subscription = subscription name_data,
        pretty = pretty name_data,
        exit = the_default I before_exit
          #> Local_Theory.target_of #> Sign.change_end_local}
  end;

val init = gen_init NONE

val theory_init = init "";

fun theory_like_init before_exit = gen_init (SOME before_exit) "";


(* toplevel interaction *)

fun begin ("-", _) thy = theory_init thy
  | begin target thy = init (Locale.check thy target) thy;

val exit = Local_Theory.assert_bottom true #> Local_Theory.exit_global;

fun switch NONE (Context.Theory thy) =
      (Context.Theory o exit, theory_init thy)
  | switch (SOME name) (Context.Theory thy) =
      (Context.Theory o exit, begin name thy)
  | switch NONE (Context.Proof lthy) =
      (Context.Proof o Local_Theory.restore, lthy)
  | switch (SOME name) (Context.Proof lthy) =
      (Context.Proof o init (target_of lthy) o exit,
        (begin name o exit o Local_Theory.assert_nonbrittle) lthy);

end;
