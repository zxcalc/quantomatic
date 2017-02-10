(*  Title:      Pure/Isar/entity.ML
    Author:     Makarius

Entity definitions within a global or local theory context.
*)

signature ENTITY =
sig
  type 'a data_ops =
   {get_data: Context.generic -> 'a Name_Space.table,
    put_data: 'a Name_Space.table -> Context.generic -> Context.generic}
  val define_global: 'a data_ops -> binding -> 'a -> theory -> string * theory
  val define: 'a data_ops -> binding -> 'a -> local_theory -> string * local_theory
end;

structure Entity: ENTITY =
struct

(* context data *)

type 'a data_ops =
 {get_data: Context.generic -> 'a Name_Space.table,
  put_data: 'a Name_Space.table -> Context.generic -> Context.generic};


(* global definition (foundation) *)

fun define_global {get_data, put_data} b x thy =
  let
    val context = Context.Theory thy;
    val (name, data') = Name_Space.define context true (b, x) (get_data context);
  in (name, Context.the_theory (put_data data' context)) end;


(* local definition *)

fun alias {get_data, put_data} binding name =
  Local_Theory.declaration {syntax = false, pervasive = false} (fn phi => fn context =>
    let
      val naming = Name_Space.naming_of context;
      val binding' = Morphism.binding phi binding;
      val data' = Name_Space.alias_table naming binding' name (get_data context);
    in put_data data' context end);

fun transfer {get_data, put_data} ctxt =
  let
    val data0 = get_data (Context.Theory (Proof_Context.theory_of ctxt));
    val data' = Name_Space.merge_tables (data0, get_data (Context.Proof ctxt));
  in Context.proof_map (put_data data') ctxt end;

fun define ops binding x =
  Local_Theory.background_theory_result (define_global ops binding x)
  #-> (fn name =>
    Local_Theory.map_contexts (K (transfer ops))
    #> alias ops binding name
    #> pair name);

end;
