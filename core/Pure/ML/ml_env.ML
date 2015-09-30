(*  Title:      Pure/ML/ml_env.ML
    Author:     Makarius

Toplevel environment for Standard ML and Isabelle/ML within the
implicit context.
*)

signature ML_ENV =
sig
  val inherit: Context.generic -> Context.generic -> Context.generic
  val forget_structure: string -> Context.generic -> Context.generic
  val name_space: {SML: bool, exchange: bool} -> ML_Name_Space.T
  val local_context: use_context
  val local_name_space: ML_Name_Space.T
  val check_functor: string -> unit
end

structure ML_Env: ML_ENV =
struct

(* context data *)

type tables =
  ML_Name_Space.valueVal Symtab.table *
  ML_Name_Space.typeVal Symtab.table *
  ML_Name_Space.fixityVal Symtab.table *
  ML_Name_Space.structureVal Symtab.table *
  ML_Name_Space.signatureVal Symtab.table *
  ML_Name_Space.functorVal Symtab.table;

fun merge_tables
  ((val1, type1, fixity1, structure1, signature1, functor1),
   (val2, type2, fixity2, structure2, signature2, functor2)) : tables =
  (Symtab.merge (K true) (val1, val2),
   Symtab.merge (K true) (type1, type2),
   Symtab.merge (K true) (fixity1, fixity2),
   Symtab.merge (K true) (structure1, structure2),
   Symtab.merge (K true) (signature1, signature2),
   Symtab.merge (K true) (functor1, functor2));

type data = {bootstrap: bool, tables: tables, sml_tables: tables};

fun make_data (bootstrap, tables, sml_tables) : data =
  {bootstrap = bootstrap, tables = tables, sml_tables = sml_tables};

structure Env = Generic_Data
(
  type T = data
  val empty =
    make_data (true,
      (Symtab.empty, Symtab.empty, Symtab.empty, Symtab.empty, Symtab.empty, Symtab.empty),
      (Symtab.make ML_Name_Space.initial_val,
       Symtab.make ML_Name_Space.initial_type,
       Symtab.make ML_Name_Space.initial_fixity,
       Symtab.make ML_Name_Space.initial_structure,
       Symtab.make ML_Name_Space.initial_signature,
       Symtab.make ML_Name_Space.initial_functor));
  fun extend (data : T) = make_data (false, #tables data, #sml_tables data);
  fun merge (data : T * T) =
    make_data (false,
      merge_tables (apply2 #tables data),
      merge_tables (apply2 #sml_tables data));
);

val inherit = Env.put o Env.get;

fun forget_structure name =
  Env.map (fn {bootstrap, tables, sml_tables} =>
    let
      val _ = if bootstrap then ML_Name_Space.forget_global_structure name else ();
      val tables' =
        (#1 tables, #2 tables, #3 tables, Symtab.delete_safe name (#4 tables), #5 tables, #6 tables);
    in make_data (bootstrap, tables', sml_tables) end);


(* name space *)

fun name_space {SML, exchange} : ML_Name_Space.T =
  let
    fun lookup sel1 sel2 name =
      if SML then
        Context.the_thread_data ()
        |> (fn context => Symtab.lookup (sel1 (#sml_tables (Env.get context))) name)
      else
        Context.thread_data ()
        |> (fn NONE => NONE | SOME context => Symtab.lookup (sel1 (#tables (Env.get context))) name)
        |> (fn NONE => sel2 ML_Name_Space.global name | some => some);

    fun all sel1 sel2 () =
      (if SML then
        Context.the_thread_data ()
        |> (fn context => Symtab.dest (sel1 (#sml_tables (Env.get context))))
      else
        Context.thread_data ()
        |> (fn NONE => [] | SOME context => Symtab.dest (sel1 (#tables (Env.get context))))
        |> append (sel2 ML_Name_Space.global ()))
      |> sort_distinct (string_ord o apply2 #1);

    fun enter ap1 sel2 entry =
      if SML <> exchange then
        Context.>> (Env.map (fn {bootstrap, tables, sml_tables} =>
          let val sml_tables' = ap1 (Symtab.update entry) sml_tables
          in make_data (bootstrap, tables, sml_tables') end))
      else if is_some (Context.thread_data ()) then
        Context.>> (Env.map (fn {bootstrap, tables, sml_tables} =>
          let
            val _ = if bootstrap then sel2 ML_Name_Space.global entry else ();
            val tables' = ap1 (Symtab.update entry) tables;
          in make_data (bootstrap, tables', sml_tables) end))
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

val local_context: use_context =
 {tune_source = ML_Parse.fix_ints,
  name_space = name_space {SML = false, exchange = false},
  str_of_pos = Position.here oo Position.line_file,
  print = writeln,
  error = error};

val local_name_space = #name_space local_context;

val is_functor = is_some o #lookupFunct local_name_space;

fun check_functor name =
  if not (is_functor "Table") (*mask dummy version of name_space*) orelse is_functor name then ()
  else error ("Unknown ML functor: " ^ quote name);

end;

