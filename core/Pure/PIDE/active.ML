(*  Title:      Pure/PIDE/active.ML
    Author:     Makarius

Active areas within the document (see also Tools/jEdit/src/active.scala).
*)

signature ACTIVE =
sig
  val make_markup: string -> {implicit: bool, properties: Properties.T} -> Markup.T
  val markup_implicit: string -> string -> string
  val markup: string -> string -> string
  val sendback_markup_properties: Properties.T -> string -> string
  val sendback_markup_command: string -> string
  val dialog: unit -> (string -> Markup.T) * string future
  val dialog_text: unit -> (string -> string) * string future
  val dialog_result: serial -> string -> unit
end;

structure Active: ACTIVE =
struct

(* active markup *)

fun explicit_id () =
  (case Position.get_id (Position.thread_data ()) of
    SOME id => [(Markup.idN, id)]
  | NONE => []);

fun make_markup name {implicit, properties} =
  (name, [])
  |> not implicit ? (fn markup => Markup.properties (explicit_id ()) markup)
  |> Markup.properties properties;

fun markup_implicit name s = Markup.markup (make_markup name {implicit = true, properties = []}) s;
fun markup name s = Markup.markup (make_markup name {implicit = false, properties = []}) s;

fun sendback_markup_properties props s =
  Markup.markup (make_markup Markup.sendbackN {implicit = false, properties = props}) s;

fun sendback_markup_command s =
  sendback_markup_properties [Markup.padding_command] s;


(* dialog via document content *)

val dialogs = Synchronized.var "Active.dialogs" (Inttab.empty: string future Inttab.table);

fun dialog () =
  let
    val i = serial ();
    fun abort () = Synchronized.change dialogs (Inttab.delete_safe i);
    val promise = Future.promise abort : string future;
    val _ = Synchronized.change dialogs (Inttab.update_new (i, promise));
    fun result_markup result = Markup.properties (explicit_id ()) (Markup.dialog i result);
  in (result_markup, promise) end;

fun dialog_text () =
  let val (markup, promise) = dialog ()
  in (fn s => Markup.markup (markup s) s, promise) end;

fun dialog_result i result =
  Synchronized.change_result dialogs
    (fn tab => (Inttab.lookup tab i, Inttab.delete_safe i tab))
  |> (fn NONE => () | SOME promise => Future.fulfill promise result);

end;

