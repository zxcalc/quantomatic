(*  Title:      Pure/ML/ml_env.ML
    Author:     Makarius

Local environment of ML results.
*)

signature ML_ENV =
sig
  val inherit: Context.generic -> Context.generic -> Context.generic
  val name_space: ML_Name_Space.T
  val local_context: use_context
  val check_functor: string -> unit
end

structure ML_Env: ML_ENV =
struct

(* context data *)

structure Env = Generic_Data
(
  type T =
    ML_Name_Space.valueVal Symtab.table *
    ML_Name_Space.typeVal Symtab.table *
    ML_Name_Space.fixityVal Symtab.table *
    ML_Name_Space.structureVal Symtab.table *
    ML_Name_Space.signatureVal Symtab.table *
    ML_Name_Space.functorVal Symtab.table;
  val empty = (Symtab.empty, Symtab.empty, Symtab.empty, Symtab.empty, Symtab.empty, Symtab.empty);
  val extend = I;
  fun merge
   ((val1, type1, fixity1, structure1, signature1, functor1),
    (val2, type2, fixity2, structure2, signature2, functor2)) : T =
    (Symtab.merge (K true) (val1, val2),
     Symtab.merge (K true) (type1, type2),
     Symtab.merge (K true) (fixity1, fixity2),
     Symtab.merge (K true) (structure1, structure2),
     Symtab.merge (K true) (signature1, signature2),
     Symtab.merge (K true) (functor1, functor2));
);

val inherit = Env.put o Env.get;


(* results *)

val name_space: ML_Name_Space.T =
  let
    fun lookup sel1 sel2 name =
      Context.thread_data ()
      |> (fn NONE => NONE | SOME context => Symtab.lookup (sel1 (Env.get context)) name)
      |> (fn NONE => sel2 ML_Name_Space.global name | some => some);

    fun all sel1 sel2 () =
      Context.thread_data ()
      |> (fn NONE => [] | SOME context => Symtab.dest (sel1 (Env.get context)))
      |> append (sel2 ML_Name_Space.global ())
      |> sort_distinct (string_ord o pairself #1);

    fun enter ap1 sel2 entry =
      if is_some (Context.thread_data ()) then
        Context.>> (Env.map (ap1 (Symtab.update entry)))
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
  name_space = name_space,
  str_of_pos = Position.here oo Position.line_file,
  print = writeln,
  error = error};

val is_functor = is_some o #lookupFunct name_space;

fun check_functor name =
  if not (is_functor "Table") (*mask dummy version of name_space*) orelse is_functor name then ()
  else error ("Unknown ML functor: " ^ quote name);

end;

