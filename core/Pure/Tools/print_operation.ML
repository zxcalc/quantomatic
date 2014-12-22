(*  Title:      Pure/Tools/print_operation.ML
    Author:     Makarius

Print operations as asynchronous query.
*)


signature PRINT_OPERATION =
sig
  val register: string -> string -> (Toplevel.state -> Pretty.T list) -> unit
end;

structure Print_Operation: PRINT_OPERATION =
struct

(* maintain print operations *)

local

val print_operations =
  Synchronized.var "print_operations"
    ([]: (string * (string * (Toplevel.state -> Pretty.T list))) list);

fun report () =
  Output.try_protocol_message Markup.print_operations
    let
      val yxml =
        Synchronized.value print_operations
        |> map (fn (x, (y, _)) => (x, y)) |> rev
        |> let open XML.Encode in list (pair string string) end
        |> YXML.string_of_body;
    in [yxml] end;

val _ = Isabelle_Process.protocol_command "print_operations" (fn [] => report ());

val _ = Session.protocol_handler "isabelle.Print_Operation$Handler";

in

fun register name description pr =
 (Synchronized.change print_operations (fn tab =>
   (if not (AList.defined (op =) tab name) then ()
    else warning ("Redefining print operation: " ^ quote name);
    AList.update (op =) (name, (description, pr)) tab));
  report ());

val _ =
  Query_Operation.register "print_operation" (fn {state, args, output_result} =>
    let
      val _ = Toplevel.context_of state handle Toplevel.UNDEF => error "Unknown context";
      fun err s = Pretty.mark_str (Markup.bad, s);
      fun print name =
        (case AList.lookup (op =) (Synchronized.value print_operations) name of
          SOME (_, pr) => (pr state handle Toplevel.UNDEF => [err "Unknown context"])
        | NONE => [err ("Unknown print operation: " ^ quote name)]);
    in output_result (Pretty.string_of (Pretty.chunks (maps print args))) end);

end;


(* common print operations *)

val _ =
  register "context" "context of local theory target" Toplevel.pretty_context;

val _ =
  register "cases" "cases of proof context"
    (Proof_Context.pretty_cases o Toplevel.context_of);

val _ =
  register "terms" "term bindings of proof context"
    (Proof_Context.pretty_term_bindings o Toplevel.context_of);

val _ =
  register "theorems" "theorems of local theory or proof context"
    (Isar_Cmd.pretty_theorems false);

val _ =
  register "state" "proof state" Toplevel.pretty_state;

end;

