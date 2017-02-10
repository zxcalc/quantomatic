(*  Title:      Pure/Tools/plugin.ML
    Author:     Makarius
    Author:     Jasmin Blanchette

Named plugins for definitional packages.
*)

(** plugin name **)

signature PLUGIN_NAME =
sig
  val check: Proof.context -> xstring * Position.T -> string
  val define: binding -> string list -> theory -> string * theory
  val define_setup: binding -> string list -> string
  val declare: binding -> theory -> string * theory
  val declare_setup: binding -> string
  type filter = string -> bool
  val default_filter: filter
  val make_filter: Proof.context -> (Proof.context -> filter) -> filter
  val parse_filter: (Proof.context -> filter) parser
end;

structure Plugin_Name: PLUGIN_NAME =
struct

(* theory data *)

structure Data = Theory_Data
(
  type T = string list Name_Space.table;
  val empty: T = Name_Space.empty_table "plugin";
  val extend = I;
  val merge = Name_Space.merge_tables;
);


(* check *)

fun check ctxt =
  #1 o Name_Space.check (Context.Proof ctxt) (Data.get (Proof_Context.theory_of ctxt));

val _ = Theory.setup
  (ML_Antiquotation.inline @{binding plugin}
    (Args.context -- Scan.lift (Parse.position Args.name)
      >> (ML_Syntax.print_string o uncurry check)));


(* indirections *)

fun resolve thy = maps (fn name =>
  (case Name_Space.get (Data.get thy) name of
    [] => [name]
  | names => resolve thy names));

fun define binding rhs thy =
  let
    val context = Context.Theory thy |> Name_Space.map_naming (K Name_Space.global_naming);
    val (name, data') = Name_Space.define context true (binding, resolve thy rhs) (Data.get thy);
    val thy' = Data.put data' thy;
  in (name, thy') end;

fun define_setup binding rhs = Context.>>> (Context.map_theory_result (define binding rhs));


(* immediate entries *)

fun declare binding thy = define binding [] thy;
fun declare_setup binding = Context.>>> (Context.map_theory_result (declare binding));


(* filter *)

type filter = string -> bool;

val default_filter: filter = K true;

fun make_filter (ctxt: Proof.context) f : filter = f ctxt;

val parse_filter =
  Parse.position (Parse.reserved "plugins") --
    Parse.position (Parse.reserved "only" >> K I || Parse.reserved "del" >> K not) --
    (Parse.$$$ ":" |-- Scan.repeat (Parse.position Parse.name)) >>
      (fn (((_, pos1), (modif, pos2)), args) => fn ctxt =>
        let
          val thy = Proof_Context.theory_of ctxt;
          val _ = Context_Position.reports ctxt (map (rpair Markup.quasi_keyword) [pos1, pos2]);
          val basic_names = resolve thy (map (check ctxt) args);
        in modif o member (op =) basic_names end);

end;



(** plugin content **)

signature PLUGIN =
sig
  type T
  val data: Plugin_Name.filter -> T -> local_theory -> local_theory
  val data_default: T -> local_theory -> local_theory
  val interpretation: string -> (T -> local_theory -> local_theory) -> theory -> theory
end;

functor Plugin(type T): PLUGIN =
struct

type T = T;


(* data with interpretation *)

type data = {filter: Plugin_Name.filter, naming: Name_Space.naming, value: T, id: serial};
type interp = {name: string, apply: T -> local_theory -> local_theory, id: serial};

val eq_data: data * data -> bool = op = o apply2 #id;
val eq_interp: interp * interp -> bool = op = o apply2 #id;

fun mk_data filter naming x : data = {filter = filter, naming = naming, value = x, id = serial ()};
fun mk_interp name f : interp = {name = name, apply = f, id = serial ()};


(* theory data *)

structure Plugin_Data = Theory_Data
(
  type T = data list * (interp * data list) list;
  val empty : T = ([], []);
  val extend = I;
  val merge_data = merge eq_data;
  fun merge ((data1, interps1), (data2, interps2)) : T =
    (merge_data (data1, data2), AList.join eq_interp (K merge_data) (interps1, interps2));
);


(* consolidate data wrt. interpretations *)

local

fun apply change_naming (interp: interp) (data: data) lthy =
  lthy
  |> change_naming ? Local_Theory.map_background_naming (K (#naming data))
  |> #apply interp (#value data)
  |> Local_Theory.restore_background_naming lthy;

fun unfinished data (interp: interp, data') =
  (interp,
    if eq_list eq_data (data, data') then []
    else data |> filter (fn d => #filter d (#name interp) andalso not (member eq_data data' d)));

fun unfinished_data thy =
  let
    val (data, interps) = Plugin_Data.get thy;
    val finished = map (apsnd (K data)) interps;
    val thy' = Plugin_Data.put (data, finished) thy;
  in (map (unfinished data) interps, thy') end;

in

val consolidate =
  Local_Theory.raw_theory_result unfinished_data
  #-> fold_rev (fn (interp, data) => fold_rev (apply false interp) data);

val consolidate' =
  unfinished_data #> (fn (unfinished, thy) =>
    if forall (null o #2) unfinished then NONE
    else
      SOME (Named_Target.theory_map
        (fold_rev (fn (interp, data) => fold_rev (apply true interp) data) unfinished) thy));

end;

val _ = Theory.setup (Theory.at_begin consolidate');


(* add content *)

fun data filter x =
  Local_Theory.background_theory (fn thy =>
    Plugin_Data.map (apfst (cons (mk_data filter (Sign.naming_of thy) x))) thy)
  #> consolidate;

val data_default = data Plugin_Name.default_filter;

fun interpretation name f =
  Plugin_Data.map (apsnd (cons (mk_interp name f, [])))
  #> perhaps consolidate';

end;
