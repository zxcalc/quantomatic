(*  Title:      Pure/Tools/named_theorems.ML
    Author:     Makarius

Named collections of theorems in canonical order.
*)

signature NAMED_THEOREMS =
sig
  val member: Proof.context -> string -> thm -> bool
  val get: Proof.context -> string -> thm list
  val add_thm: string -> thm -> Context.generic -> Context.generic
  val del_thm: string -> thm -> Context.generic -> Context.generic
  val add: string -> attribute
  val del: string -> attribute
  val check: Proof.context -> string * Position.T -> string
  val declare: binding -> string -> local_theory -> string * local_theory
end;

structure Named_Theorems: NAMED_THEOREMS =
struct

(* context data *)

structure Data = Generic_Data
(
  type T = thm Item_Net.T Symtab.table;
  val empty: T = Symtab.empty;
  val extend = I;
  val merge : T * T -> T = Symtab.join (K Item_Net.merge);
);

fun new_entry name =
  Data.map (fn data =>
    if Symtab.defined data name
    then error ("Duplicate declaration of named theorems: " ^ quote name)
    else Symtab.update (name, Thm.full_rules) data);

fun undeclared name = "Undeclared named theorems " ^ quote name;

fun the_entry context name =
  (case Symtab.lookup (Data.get context) name of
    NONE => error (undeclared name)
  | SOME entry => entry);

fun map_entry name f context =
  (the_entry context name; Data.map (Symtab.map_entry name f) context);


(* maintain content *)

fun member ctxt = Item_Net.member o the_entry (Context.Proof ctxt);

fun content context = rev o Item_Net.content o the_entry context;
val get = content o Context.Proof;

fun add_thm name = map_entry name o Item_Net.update;
fun del_thm name = map_entry name o Item_Net.remove;

val add = Thm.declaration_attribute o add_thm;
val del = Thm.declaration_attribute o del_thm;


(* check *)

fun check ctxt (xname, pos) =
  let
    val context = Context.Proof ctxt;
    val fact_ref = Facts.Named ((xname, Position.none), NONE);
    fun err () = error (undeclared xname ^ Position.here pos);
  in
    (case try (Proof_Context.get_fact_generic context) fact_ref of
      SOME (SOME name, _) => if can (the_entry context) name then name else err ()
    | _ => err ())
  end;


(* declaration *)

fun declare binding descr lthy =
  let
    val name = Local_Theory.full_name lthy binding;
    val description =
      "declaration of " ^ (if descr = "" then Binding.name_of binding ^ " rules" else descr);
    val lthy' = lthy
      |> Local_Theory.background_theory (Context.theory_map (new_entry name))
      |> Local_Theory.map_contexts (K (Context.proof_map (new_entry name)))
      |> Local_Theory.add_thms_dynamic (binding, fn context => content context name)
      |> Attrib.local_setup binding (Attrib.add_del (add name) (del name)) description
  in (name, lthy') end;

val _ =
  Outer_Syntax.local_theory @{command_keyword named_theorems}
    "declare named collection of theorems"
    (Parse.and_list1 (Parse.binding -- Scan.optional Parse.text "") >>
      fold (fn (b, descr) => snd o declare b descr));


(* ML antiquotation *)

val _ = Theory.setup
  (ML_Antiquotation.inline @{binding named_theorems}
    (Args.context -- Scan.lift (Parse.position Args.name) >>
      (fn (ctxt, name) => ML_Syntax.print_string (check ctxt name))));

end;
