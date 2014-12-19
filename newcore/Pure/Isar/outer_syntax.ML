(*  Title:      Pure/Isar/outer_syntax.ML
    Author:     Markus Wenzel, TU Muenchen

The global Isabelle/Isar outer syntax.

Note: the syntax for files is statically determined at the very
beginning; for interactive processing it may change dynamically.
*)

signature OUTER_SYNTAX =
sig
  type outer_syntax
  val batch_mode: bool Unsynchronized.ref
  val is_markup: outer_syntax -> Thy_Output.markup -> string -> bool
  val get_syntax: unit -> (Scan.lexicon * Scan.lexicon) * outer_syntax
  val check_syntax: unit -> unit
  type command_spec = (string * Keyword.T) * Position.T
  val command: command_spec -> string ->
    (Toplevel.transition -> Toplevel.transition) parser -> unit
  val markup_command: Thy_Output.markup -> command_spec -> string ->
    (Toplevel.transition -> Toplevel.transition) parser -> unit
  val improper_command: command_spec -> string ->
    (Toplevel.transition -> Toplevel.transition) parser -> unit
  val local_theory': command_spec -> string ->
    (bool -> local_theory -> local_theory) parser -> unit
  val local_theory: command_spec -> string ->
    (local_theory -> local_theory) parser -> unit
  val local_theory_to_proof': command_spec -> string ->
    (bool -> local_theory -> Proof.state) parser -> unit
  val local_theory_to_proof: command_spec -> string ->
    (local_theory -> Proof.state) parser -> unit
  val help_outer_syntax: string list -> unit
  val print_outer_syntax: unit -> unit
  val scan: Position.T -> string -> Token.T list
  val parse: Position.T -> string -> Toplevel.transition list
  type isar
  val isar: TextIO.instream -> bool -> isar
  val side_comments: Token.T list -> Token.T list
  val command_reports: outer_syntax -> Token.T -> Position.report_text list
  val read_spans: outer_syntax -> Token.T list -> Toplevel.transition list
end;

structure Outer_Syntax: OUTER_SYNTAX =
struct

(** outer syntax **)

(* command parsers *)

datatype command = Command of
 {comment: string,
  markup: Thy_Output.markup option,
  int_only: bool,
  parse: (Toplevel.transition -> Toplevel.transition) parser,
  pos: Position.T,
  id: serial};

fun new_command comment markup int_only parse pos =
  Command {comment = comment, markup = markup, int_only = int_only, parse = parse,
    pos = pos, id = serial ()};

fun command_markup def (name, Command {pos, id, ...}) =
  Markup.properties (Position.entity_properties_of def id pos)
    (Markup.entity Markup.commandN name);

fun pretty_command (cmd as (name, Command {comment, ...})) =
  Pretty.block
    (Pretty.marks_str
      ([Active.make_markup Markup.sendbackN {implicit = true, properties = [Markup.padding_line]},
        command_markup false cmd], name) :: Pretty.str ":" :: Pretty.brk 2 :: Pretty.text comment);


(* parse command *)

local

fun terminate false = Scan.succeed ()
  | terminate true =
      Parse.group (fn () => "end of input")
        (Scan.option Parse.sync -- Parse.semicolon >> K ());

fun body cmd (name, _) =
  (case cmd name of
    SOME (Command {int_only, parse, ...}) =>
      Parse.!!! (Scan.prompt (name ^ "# ") (Parse.tags |-- parse >> pair int_only))
  | NONE =>
      Scan.succeed (false, Toplevel.imperative (fn () =>
        error ("Bad parser for outer syntax command " ^ quote name))));

in

fun parse_command do_terminate cmd =
  Parse.semicolon >> K NONE ||
  Parse.sync >> K NONE ||
  (Parse.position Parse.command :-- body cmd) --| terminate do_terminate
    >> (fn ((name, pos), (int_only, f)) =>
      SOME (Toplevel.empty |> Toplevel.name name |> Toplevel.position pos |>
        Toplevel.interactive int_only |> f));

end;


(* type outer_syntax *)

datatype outer_syntax = Outer_Syntax of
 {commands: command Symtab.table,
  markups: (string * Thy_Output.markup) list};

fun make_outer_syntax commands markups =
  Outer_Syntax {commands = commands, markups = markups};

val empty_outer_syntax = make_outer_syntax Symtab.empty [];


fun map_commands f (Outer_Syntax {commands, ...}) =
  let
    val commands' = f commands;
    val markups' =
      Symtab.fold (fn (name, Command {markup = SOME m, ...}) => cons (name, m) | _ => I)
        commands' [];
  in make_outer_syntax commands' markups' end;

fun dest_commands (Outer_Syntax {commands, ...}) =
  commands |> Symtab.dest |> sort_wrt #1;

fun lookup_commands (Outer_Syntax {commands, ...}) = Symtab.lookup commands;

fun is_markup (Outer_Syntax {markups, ...}) kind name =
  AList.lookup (op =) markups name = SOME kind;



(** global outer syntax **)

type command_spec = (string * Keyword.T) * Position.T;

val batch_mode = Unsynchronized.ref false;

local

(*synchronized wrt. Keywords*)
val global_outer_syntax = Unsynchronized.ref empty_outer_syntax;

fun add_command (name, kind) cmd = CRITICAL (fn () =>
  let
    val context = ML_Context.the_generic_context ();
    val thy = Context.theory_of context;
    val Command {pos, ...} = cmd;
    val command_name = quote (Markup.markup Markup.keyword1 name);
    val _ =
      (case try (Thy_Header.the_keyword thy) name of
        SOME spec =>
          if Option.map #1 spec = SOME (Keyword.kind_files_of kind) then ()
          else error ("Inconsistent outer syntax keyword declaration " ^
            command_name ^ Position.here pos)
      | NONE =>
          if Context.theory_name thy = Context.PureN
          then Keyword.define (name, SOME kind)
          else error ("Undeclared outer syntax command " ^ command_name ^ Position.here pos));
    val _ = Context_Position.report_generic context pos (command_markup true (name, cmd));
  in
    Unsynchronized.change global_outer_syntax (map_commands (fn commands =>
     (if not (Symtab.defined commands name) then ()
      else if ! batch_mode then
        error ("Attempt to redefine outer syntax command " ^ command_name)
      else
        warning ("Redefining outer syntax command " ^ command_name ^
          Position.here (Position.thread_data ()));
      Symtab.update (name, cmd) commands)))
  end);

in

fun get_syntax () = CRITICAL (fn () => (Keyword.get_lexicons (), ! global_outer_syntax));

fun check_syntax () =
  let
    val ((_, major), syntax) = CRITICAL (fn () => (Keyword.dest (), ! global_outer_syntax));
  in
    (case subtract (op =) (map #1 (dest_commands syntax)) major of
      [] => ()
    | missing => error ("Missing outer syntax command(s) " ^ commas_quote missing))
  end;

fun lookup_commands_dynamic () = lookup_commands (! global_outer_syntax);

fun command (spec, pos) comment parse =
  add_command spec (new_command comment NONE false parse pos);

fun markup_command markup (spec, pos) comment parse =
  add_command spec (new_command comment (SOME markup) false parse pos);

fun improper_command (spec, pos) comment parse =
  add_command spec (new_command comment NONE true parse pos);

end;


(* local_theory commands *)

fun local_theory_command trans command_spec comment parse =
  command command_spec comment (Parse.opt_target -- parse >> (fn (loc, f) => trans loc f));

val local_theory' = local_theory_command Toplevel.local_theory';
val local_theory = local_theory_command Toplevel.local_theory;
val local_theory_to_proof' = local_theory_command Toplevel.local_theory_to_proof';
val local_theory_to_proof = local_theory_command Toplevel.local_theory_to_proof;


(* inspect syntax *)

fun help_outer_syntax pats =
  dest_commands (#2 (get_syntax ()))
  |> filter (fn (name, _) => forall (fn pat => match_string pat name) pats)
  |> map pretty_command
  |> Pretty.writeln_chunks;

fun print_outer_syntax () =
  let
    val ((keywords, _), outer_syntax) =
      CRITICAL (fn () => (Keyword.dest (), #2 (get_syntax ())));
    val (int_cmds, cmds) =
      List.partition (fn (_, Command {int_only, ...}) => int_only) (dest_commands outer_syntax);
  in
    [Pretty.strs ("syntax keywords:" :: map quote keywords),
      Pretty.big_list "commands:" (map pretty_command cmds),
      Pretty.big_list "interactive-only commands:" (map pretty_command int_cmds)]
    |> Pretty.writeln_chunks
  end;



(** toplevel parsing **)

(* basic sources *)

fun toplevel_source term do_recover cmd src =
  let
    val no_terminator =
      Scan.unless Parse.semicolon (Scan.one (Token.not_sync andf Token.not_eof));
    fun recover int =
      (int, fn _ => Scan.prompt "recover# " (Scan.repeat no_terminator) >> K [NONE]);
  in
    src
    |> Token.source_proper
    |> Source.source Token.stopper
      (Scan.bulk (Parse.$$$ "--" -- Parse.!!! Parse.document_source >> K NONE || Parse.not_eof >> SOME))
        (Option.map recover do_recover)
    |> Source.map_filter I
    |> Source.source Token.stopper
        (Scan.bulk (fn xs => Parse.!!! (parse_command term (cmd ())) xs))
        (Option.map recover do_recover)
    |> Source.map_filter I
  end;


(* off-line scanning/parsing *)

fun scan pos str =
  Source.of_string str
  |> Symbol.source
  |> Token.source {do_recover = SOME false} Keyword.get_lexicons pos
  |> Source.exhaust;

fun parse pos str =
  Source.of_string str
  |> Symbol.source
  |> Token.source {do_recover = SOME false} Keyword.get_lexicons pos
  |> toplevel_source false NONE lookup_commands_dynamic
  |> Source.exhaust;


(* interactive source of toplevel transformers *)

type isar =
  (Toplevel.transition, (Toplevel.transition option,
    (Token.T, (Token.T option, (Token.T, (Token.T,
      (Symbol_Pos.T,
        Position.T * (Symbol.symbol, (Symbol.symbol, (string, unit) Source.source) Source.source)
  Source.source) Source.source) Source.source) Source.source)
  Source.source) Source.source) Source.source) Source.source;

fun isar in_stream term : isar =
  Source.tty in_stream
  |> Symbol.source
  |> Source.map_filter (fn "\<^newline>" => SOME "\n" | s => SOME s)  (*Proof General legacy*)
  |> Token.source {do_recover = SOME true} Keyword.get_lexicons Position.none
  |> toplevel_source term (SOME true) lookup_commands_dynamic;


(* side-comments *)

fun cmts (t1 :: t2 :: toks) =
      if Token.keyword_with (fn s => s = "--") t1 then t2 :: cmts toks
      else cmts (t2 :: toks)
  | cmts _ = [];

val side_comments = filter Token.is_proper #> cmts;


(* read commands *)

fun command_reports outer_syntax tok =
  if Token.is_command tok then
    let val name = Token.content_of tok in
      (case lookup_commands outer_syntax name of
        NONE => []
      | SOME cmd => [((Token.pos_of tok, command_markup false (name, cmd)), "")])
    end
  else [];

fun read_spans outer_syntax toks =
  Source.of_list toks
  |> toplevel_source false NONE (K (lookup_commands outer_syntax))
  |> Source.exhaust;

end;

