(*  Title:      Pure/System/invoke_scala.ML
    Author:     Makarius

JVM method invocation service via Isabelle/Scala.
*)

signature INVOKE_SCALA =
sig
  val method: string -> string -> string
  val promise_method: string -> string -> string future
  exception Null
end;

structure Invoke_Scala: INVOKE_SCALA =
struct

val _ = Session.protocol_handler "isabelle.Invoke_Scala";


(* pending promises *)

val new_id = string_of_int o Counter.make ();

val promises =
  Synchronized.var "Invoke_Scala.promises" (Symtab.empty: string future Symtab.table);


(* method invocation *)

fun promise_method name arg =
  let
    val id = new_id ();
    fun abort () = Output.protocol_message (Markup.cancel_scala id) [];
    val promise = Future.promise abort : string future;
    val _ = Synchronized.change promises (Symtab.update (id, promise));
    val _ = Output.protocol_message (Markup.invoke_scala name id) [arg];
  in promise end;

fun method name arg = Future.join (promise_method name arg);


(* fulfill *)

exception Null;

fun fulfill id tag res =
  let
    val result =
      (case tag of
        "0" => Exn.Exn Null
      | "1" => Exn.Res res
      | "2" => Exn.Exn (ERROR res)
      | "3" => Exn.Exn (Fail res)
      | "4" => Exn.Exn Exn.Interrupt
      | _ => raise Fail "Bad tag");
    val promise =
      Synchronized.change_result promises
        (fn tab => (the (Symtab.lookup tab id), Symtab.delete id tab));
    val _ = Future.fulfill_result promise result;
  in () end;

val _ =
  Isabelle_Process.protocol_command "Invoke_Scala.fulfill"
    (fn [id, tag, res] =>
      fulfill id tag res
        handle exn => if Exn.is_interrupt exn then () else reraise exn);

end;

