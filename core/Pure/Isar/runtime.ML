(*  Title:      Pure/Isar/runtime.ML
    Author:     Makarius

Isar toplevel runtime support.
*)

signature RUNTIME =
sig
  exception UNDEF
  exception EXCURSION_FAIL of exn * string
  exception TOPLEVEL_ERROR
  val exn_context: Proof.context option -> exn -> exn
  type error = ((serial * string) * string option)
  val exn_messages_ids: exn -> error list
  val exn_messages: exn -> (serial * string) list
  val exn_message: exn -> string
  val exn_error_message: exn -> unit
  val exn_system_message: exn -> unit
  val exn_trace: (unit -> 'a) -> 'a
  val exn_trace_system: (unit -> 'a) -> 'a
  val debugging: Context.generic option -> ('a -> 'b) -> 'a -> 'b
  val controlled_execution: Context.generic option -> ('a -> 'b) -> 'a -> 'b
  val toplevel_error: (exn -> unit) -> ('a -> 'b) -> 'a -> 'b
  val toplevel_program: (unit -> 'a) -> 'a
end;

structure Runtime: RUNTIME =
struct

(** exceptions **)

exception UNDEF;
exception EXCURSION_FAIL of exn * string;
exception TOPLEVEL_ERROR;


(* exn_context *)

exception CONTEXT of Proof.context * exn;

fun exn_context NONE exn = exn
  | exn_context (SOME ctxt) exn = if Exn.is_interrupt exn then exn else CONTEXT (ctxt, exn);


(* exn_message *)

type error = ((serial * string) * string option);

local

fun robust f x =
  (case try f x of
    SOME s => s
  | NONE => Markup.markup Markup.intensify "<malformed>");

fun robust2 f x y = robust (fn () => f x y) ();

fun robust_context NONE _ _ = []
  | robust_context (SOME ctxt) f xs = map (robust2 f ctxt) xs;

fun identify exn =
  let
    val exn' = Par_Exn.identify [] exn;
    val exec_id = Properties.get (Exn_Properties.get exn') Markup.exec_idN;
    val i = Par_Exn.the_serial exn' handle Option.Option => serial ();
  in ((i, exn'), exec_id) end;

fun flatten _ (CONTEXT (ctxt, exn)) = flatten (SOME ctxt) exn
  | flatten context (Exn.EXCEPTIONS exns) = maps (flatten context) exns
  | flatten context exn =
      (case Par_Exn.dest exn of
        SOME exns => maps (flatten context) exns
      | NONE => [(context, identify exn)]);

in

fun exn_messages_ids e =
  let
    fun raised exn name msgs =
      let val pos = Position.here (Exn_Output.position exn) in
        (case msgs of
          [] => "exception " ^ name ^ " raised" ^ pos
        | [msg] => "exception " ^ name ^ " raised" ^ pos ^ ": " ^ msg
        | _ =>
            cat_lines (("exception " ^ name ^ " raised" ^ pos ^ ":") ::
              map (Markup.markup Markup.item) msgs))
      end;

    fun exn_msgs (context, ((i, exn), id)) =
      (case exn of
        EXCURSION_FAIL (exn, loc) =>
          map (fn ((i, msg), id) => ((i, msg ^ Markup.markup Markup.no_report ("\n" ^ loc)), id))
            (sorted_msgs context exn)
      | _ =>
        let
          val msg =
            (case exn of
              TimeLimit.TimeOut => "Timeout"
            | TOPLEVEL_ERROR => "Error"
            | ERROR "" => "Error"
            | ERROR msg => msg
            | Fail msg => raised exn "Fail" [msg]
            | THEORY (msg, thys) =>
                raised exn "THEORY" (msg :: map (robust Context.str_of_thy) thys)
            | Ast.AST (msg, asts) =>
                raised exn "AST" (msg :: map (robust (Pretty.string_of o Ast.pretty_ast)) asts)
            | TYPE (msg, Ts, ts) =>
                raised exn "TYPE" (msg ::
                  (robust_context context Syntax.string_of_typ Ts @
                    robust_context context Syntax.string_of_term ts))
            | TERM (msg, ts) =>
                raised exn "TERM" (msg :: robust_context context Syntax.string_of_term ts)
            | CTERM (msg, cts) =>
                raised exn "CTERM"
                  (msg :: robust_context context Syntax.string_of_term (map Thm.term_of cts))
            | THM (msg, i, thms) =>
                raised exn ("THM " ^ string_of_int i)
                  (msg :: robust_context context Display.string_of_thm thms)
            | _ => raised exn (robust (Pretty.string_of o Exn_Output.pretty) exn) []);
        in [((i, msg), id)] end)
      and sorted_msgs context exn =
        sort_distinct (int_ord o apply2 (fst o fst)) (maps exn_msgs (flatten context exn));

  in sorted_msgs NONE e end;

end;

fun exn_messages exn = map #1 (exn_messages_ids exn);

fun exn_message exn =
  (case exn_messages exn of
    [] => "Interrupt"
  | msgs => cat_lines (map snd msgs));

val exn_error_message = Output.error_message o exn_message;
val exn_system_message = Output.system_message o exn_message;
fun exn_trace e = print_exception_trace exn_message tracing e;
fun exn_trace_system e = print_exception_trace exn_message Output.system_message e;



(** controlled execution **)

fun debugging opt_context f x =
  if ML_Options.exception_trace_enabled opt_context
  then exn_trace (fn () => f x) else f x;

fun controlled_execution opt_context f x =
  (f |> debugging opt_context |> Future.interruptible_task) x;

fun toplevel_error output_exn f x = f x
  handle exn =>
    if Exn.is_interrupt exn then reraise exn
    else
      let
        val opt_ctxt =
          (case Context.thread_data () of
            NONE => NONE
          | SOME context => try Context.proof_of context);
        val _ = output_exn (exn_context opt_ctxt exn);
      in raise TOPLEVEL_ERROR end;

fun toplevel_program body =
  (body |> controlled_execution NONE |> toplevel_error exn_error_message) ();

end;

