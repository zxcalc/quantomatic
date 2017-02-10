(*  Title:      Pure/ML/ml_env.ML
    Author:     Makarius

Toplevel environment for Standard ML and Isabelle/ML within the
implicit context.
*)

signature ML_ENV =
sig
  val inherit: Context.generic -> Context.generic -> Context.generic
  val forget_structure: string -> Context.generic -> Context.generic
  val get_breakpoint: Context.generic -> serial -> (bool Unsynchronized.ref * Position.T) option
  val add_breakpoints: (serial * (bool Unsynchronized.ref * Thread_Position.T)) list -> unit
  val init_bootstrap: Context.generic -> Context.generic
  val SML_environment: bool Config.T
  val add_name_space: {SML: bool} -> ML_Name_Space.T -> Context.generic -> Context.generic
  val make_name_space: {SML: bool, exchange: bool} -> ML_Name_Space.T
  val context: ML_Compiler0.context
  val name_space: ML_Name_Space.T
  val check_functor: string -> unit
end

structure ML_Env: ML_ENV =
struct

(* context data *)

type tables =
  PolyML.NameSpace.Values.value Symtab.table *
  PolyML.NameSpace.TypeConstrs.typeConstr Symtab.table *
  PolyML.NameSpace.Infixes.fixity Symtab.table *
  PolyML.NameSpace.Structures.structureVal Symtab.table *
  PolyML.NameSpace.Signatures.signatureVal Symtab.table *
  PolyML.NameSpace.Functors.functorVal Symtab.table;

fun merge_tables
  ((val1, type1, fixity1, structure1, signature1, functor1),
   (val2, type2, fixity2, structure2, signature2, functor2)) : tables =
  (Symtab.merge (K true) (val1, val2),
   Symtab.merge (K true) (type1, type2),
   Symtab.merge (K true) (fixity1, fixity2),
   Symtab.merge (K true) (structure1, structure2),
   Symtab.merge (K true) (signature1, signature2),
   Symtab.merge (K true) (functor1, functor2));

type data =
 {bootstrap: bool,
  tables: tables,
  sml_tables: tables,
  breakpoints: (bool Unsynchronized.ref * Position.T) Inttab.table};

fun make_data (bootstrap, tables, sml_tables, breakpoints) : data =
  {bootstrap = bootstrap, tables = tables, sml_tables = sml_tables, breakpoints = breakpoints};

structure Env = Generic_Data
(
  type T = data
  val empty =
    make_data (true,
      (Symtab.empty, Symtab.empty, Symtab.empty, Symtab.empty, Symtab.empty, Symtab.empty),
      (Symtab.make ML_Name_Space.sml_val,
       Symtab.make ML_Name_Space.sml_type,
       Symtab.make ML_Name_Space.sml_fixity,
       Symtab.make ML_Name_Space.sml_structure,
       Symtab.make ML_Name_Space.sml_signature,
       Symtab.make ML_Name_Space.sml_functor),
      Inttab.empty);
  fun extend (data : T) = make_data (false, #tables data, #sml_tables data, #breakpoints data);
  fun merge (data : T * T) =
    make_data (false,
      merge_tables (apply2 #tables data),
      merge_tables (apply2 #sml_tables data),
      Inttab.merge (K true) (apply2 #breakpoints data));
);

val inherit = Env.put o Env.get;

fun forget_structure name =
  Env.map (fn {bootstrap, tables, sml_tables, breakpoints} =>
    let
      val _ = if bootstrap then ML_Name_Space.forget_structure name else ();
      val tables' =
        (#1 tables, #2 tables, #3 tables, Symtab.delete_safe name (#4 tables), #5 tables, #6 tables);
    in make_data (bootstrap, tables', sml_tables, breakpoints) end);

val get_breakpoint = Inttab.lookup o #breakpoints o Env.get;

fun add_breakpoints more_breakpoints =
  if is_some (Context.get_generic_context ()) then
    Context.>>
      (Env.map (fn {bootstrap, tables, sml_tables, breakpoints} =>
        let val breakpoints' =
          fold (Inttab.update_new o (apsnd o apsnd) Position.make) more_breakpoints breakpoints;
        in make_data (bootstrap, tables, sml_tables, breakpoints') end))
  else ();


(* SML environment for Isabelle/ML *)

val SML_environment =
  Config.bool (Config.declare ("SML_environment", @{here}) (fn _ => Config.Bool false));

fun sml_env SML =
  SML orelse
    (case Context.get_generic_context () of
      NONE => false
    | SOME context => Config.get_generic context SML_environment);


(* name space *)

val init_bootstrap =
  Env.map (fn {bootstrap, tables, sml_tables, breakpoints} =>
    let
      val sml_tables' =
        sml_tables |> (fn (val1, type1, fixity1, structure1, signature1, functor1) =>
            let
              val val2 =
                fold (fn (x, y) =>
                  member (op =) ML_Name_Space.bootstrap_values x ? Symtab.update (x, y))
                (#allVal ML_Name_Space.global ()) val1;
              val structure2 =
                fold (fn (x, y) =>
                  member (op =) ML_Name_Space.bootstrap_structures x ? Symtab.update (x, y))
                (#allStruct ML_Name_Space.global ()) structure1;
              val signature2 =
                fold (fn (x, y) =>
                  member (op =) ML_Name_Space.bootstrap_signatures x ? Symtab.update (x, y))
                (#allSig ML_Name_Space.global ()) signature1;
            in (val2, type1, fixity1, structure2, signature2, functor1) end);
    in make_data (bootstrap, tables, sml_tables', breakpoints) end);

fun add_name_space {SML} (space: ML_Name_Space.T) =
  Env.map (fn {bootstrap, tables, sml_tables, breakpoints} =>
    let
      val (tables', sml_tables') =
        (tables, sml_tables) |> (if sml_env SML then apsnd else apfst)
          (fn (val1, type1, fixity1, structure1, signature1, functor1) =>
            let
              val val2 = fold Symtab.update (#allVal space ()) val1;
              val type2 = fold Symtab.update (#allType space ()) type1;
              val fixity2 = fold Symtab.update (#allFix space ()) fixity1;
              val structure2 = fold Symtab.update (#allStruct space ()) structure1;
              val signature2 = fold Symtab.update (#allSig space ()) signature1;
              val functor2 = fold Symtab.update (#allFunct space ()) functor1;
            in (val2, type2, fixity2, structure2, signature2, functor2) end);
    in make_data (bootstrap, tables', sml_tables', breakpoints) end);

fun make_name_space {SML, exchange} : ML_Name_Space.T =
  let
    fun lookup sel1 sel2 name =
      if sml_env SML then
        Context.the_generic_context ()
        |> (fn context => Symtab.lookup (sel1 (#sml_tables (Env.get context))) name)
      else
        Context.get_generic_context ()
        |> (fn NONE => NONE | SOME context => Symtab.lookup (sel1 (#tables (Env.get context))) name)
        |> (fn NONE => sel2 ML_Name_Space.global name | some => some);

    fun all sel1 sel2 () =
      (if sml_env SML then
        Context.the_generic_context ()
        |> (fn context => Symtab.dest (sel1 (#sml_tables (Env.get context))))
      else
        Context.get_generic_context ()
        |> (fn NONE => [] | SOME context => Symtab.dest (sel1 (#tables (Env.get context))))
        |> append (sel2 ML_Name_Space.global ()))
      |> sort_distinct (string_ord o apply2 #1);

    fun enter ap1 sel2 entry =
      if sml_env SML <> exchange then
        Context.>> (Env.map (fn {bootstrap, tables, sml_tables, breakpoints} =>
          let val sml_tables' = ap1 (Symtab.update entry) sml_tables
          in make_data (bootstrap, tables, sml_tables', breakpoints) end))
      else if is_some (Context.get_generic_context ()) then
        Context.>> (Env.map (fn {bootstrap, tables, sml_tables, breakpoints} =>
          let
            val _ = if bootstrap then sel2 ML_Name_Space.global entry else ();
            val tables' = ap1 (Symtab.update entry) tables;
          in make_data (bootstrap, tables', sml_tables, breakpoints) end))
      else sel2 ML_Name_Space.global entry;
  in
   {lookupVal    = lookup #1 #lookupVal,
    lookupType   = lookup #2 #lookupType,
    lookupFix    = lookup #3 #lookupFix,
    lookupStruct = lookup #4 #lookupStruct,
    lookupSig    = lookup #5 #lookupSig,
    lookupFunct  = lookup #6 #lookupFunct,
    enterVal     = enter (fn h => fn (a, b, c, d, e, f) => (h a, b, c, d, e, f)) #enterVal,
    enterType    = enter (fn h => fn (a, b, c, d, e, f) => (a, h b, c, d, e, f)) #enterType,
    enterFix     = enter (fn h => fn (a, b, c, d, e, f) => (a, b, h c, d, e, f)) #enterFix,
    enterStruct  = enter (fn h => fn (a, b, c, d, e, f) => (a, b, c, h d, e, f)) #enterStruct,
    enterSig     = enter (fn h => fn (a, b, c, d, e, f) => (a, b, c, d, h e, f)) #enterSig,
    enterFunct   = enter (fn h => fn (a, b, c, d, e, f) => (a, b, c, d, e, h f)) #enterFunct,
    allVal       = all #1 #allVal,
    allType      = all #2 #allType,
    allFix       = all #3 #allFix,
    allStruct    = all #4 #allStruct,
    allSig       = all #5 #allSig,
    allFunct     = all #6 #allFunct}
  end;

val context: ML_Compiler0.context =
 {name_space = make_name_space {SML = false, exchange = false},
  print_depth = NONE,
  here = Position.here oo Position.line_file,
  print = writeln,
  error = error};

val name_space = #name_space context;

val is_functor = is_some o #lookupFunct name_space;

fun check_functor name =
  if not (is_functor "Table") (*mask dummy version of name_space*) orelse is_functor name then ()
  else error ("Unknown ML functor: " ^ quote name);

end;
