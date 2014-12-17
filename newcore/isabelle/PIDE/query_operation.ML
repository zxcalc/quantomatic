(*  Title:      Pure/PIDE/query_operation.ML
    Author:     Makarius

One-shot query operations via asynchronous print functions and temporary
document overlays.
*)

signature QUERY_OPERATION =
sig
  val register: string ->
    ({state: Toplevel.state, args: string list, output_result: string -> unit} -> unit) -> unit
end;

structure Query_Operation: QUERY_OPERATION =
struct

fun register name f =
  Command.print_function name
    (fn {args = instance :: args, ...} =>
        SOME {delay = NONE, pri = 0, persistent = false, strict = false,
          print_fn = fn _ => uninterruptible (fn restore_attributes => fn state =>
            let
              fun result s = Output.result [(Markup.instanceN, instance)] [s];
              fun status m = result (Markup.markup_only m);
              fun output_result s = result (Markup.markup (Markup.writelnN, []) s);
              fun toplevel_error exn =
                result (Markup.markup (Markup.errorN, []) (Runtime.exn_message exn));

              val _ = status Markup.running;
              fun run () = f {state = state, args = args, output_result = output_result};
              val _ =
                (case Exn.capture (*sic!*) (restore_attributes run) () of
                  Exn.Res () => ()
                | Exn.Exn exn => toplevel_error exn);
              val _ = status Markup.finished;
            in () end)}
      | _ => NONE);

end;

