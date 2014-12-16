(*  Title:      Pure/ML/ml_context.ML
    Author:     Makarius

ML context and antiquotations.
*)

signature BASIC_ML_CONTEXT =
sig
  val bind_thm: string * thm -> unit
  val bind_thms: string * thm list -> unit
end

signature ML_CONTEXT =
sig
  include BASIC_ML_CONTEXT
  val the_generic_context: unit -> Context.generic
  val the_global_context: unit -> theory
  val the_local_context: unit -> Proof.context
  val thm: xstring -> thm
  val thms: xstring -> thm list
  val exec: (unit -> unit) -> Context.generic -> Context.generic
  val get_stored_thms: unit -> thm list
  val get_stored_thm: unit -> thm
  val ml_store_thms: string * thm list -> unit
  val ml_store_thm: string * thm -> unit
  type antiq = Proof.context -> (Proof.context -> string * string) * Proof.context
  val add_antiq: binding -> (Position.T -> antiq context_parser) -> theory -> theory
  val intern_antiq: theory -> xstring -> string
  val defined_antiq: theory -> string -> bool
  val trace_raw: Config.raw
  val trace: bool Config.T
  val eval_antiquotes: ML_Lex.token Antiquote.antiquote list * Position.T ->
    Context.generic option -> (ML_Lex.token list * ML_Lex.token list) * Context.generic option
  val eval: bool -> Position.T -> ML_Lex.token Antiquote.antiquote list -> unit
  val eval_text: bool -> Position.T -> Symbol_Pos.text -> unit
  val eval_in: Proof.context option -> bool -> Position.T ->
    ML_Lex.token Antiquote.antiquote list -> unit
  val eval_text_in: Proof.context option -> bool -> Position.T -> Symbol_Pos.text -> unit
  val expression: Position.T -> string -> string -> ML_Lex.token Antiquote.antiquote list ->
    Context.generic -> Context.generic
end

structure ML_Context: ML_CONTEXT =
struct

(** implicit ML context **)

val the_generic_context = Context.the_thread_data;
val the_global_context = Context.theory_of o the_generic_context;
val the_local_context = Context.proof_of o the_generic_context;

fun thm name = Proof_Context.get_thm (the_local_context ()) name;
fun thms name = Proof_Context.get_thms (the_local_context ()) name;

fun exec (e: unit -> unit) context =
  (case Context.setmp_thread_data (SOME context) (fn () => (e (); Context.thread_data ())) () of
    SOME context' => context'
  | NONE => error "Missing context after execution");


(* theorem bindings *)

structure Stored_Thms = Theory_Data
(
  type T = thm list;
  val empty = [];
  fun extend _ = [];
  fun merge _ = [];
);

fun get_stored_thms () = Stored_Thms.get (the_global_context ());
val get_stored_thm = hd o get_stored_thms;

fun ml_store get (name, ths) =
  let
    val ths' = Context.>>> (Context.map_theory_result
      (Global_Theory.store_thms (Binding.name name, ths)));
    val _ = Context.>> (Context.map_theory (Stored_Thms.put ths'));
    val _ =
      if name = "" then ()
      else if not (ML_Syntax.is_identifier name) then
        error ("Cannot bind theorem(s) " ^ quote name ^ " as ML value")
      else
        ML_Compiler.eval true Position.none
          (ML_Lex.tokenize ("val " ^ name ^ " = " ^ get ^ " ();"));
    val _ = Context.>> (Context.map_theory (Stored_Thms.put []));
  in () end;

val ml_store_thms = ml_store "ML_Context.get_stored_thms";
fun ml_store_thm (name, th) = ml_store "ML_Context.get_stored_thm" (name, [th]);

fun bind_thm (name, thm) = ml_store_thm (name, Drule.export_without_context thm);
fun bind_thms (name, thms) = ml_store_thms (name, map Drule.export_without_context thms);



(** ML antiquotations **)

(* antiquotation commands *)

type antiq = Proof.context -> (Proof.context -> string * string) * Proof.context;

structure Antiq_Parsers = Theory_Data
(
  type T = (Position.T -> antiq context_parser) Name_Space.table;
  val empty : T = Name_Space.empty_table Markup.ML_antiquotationN;
  val extend = I;
  fun merge data : T = Name_Space.merge_tables data;
);

fun add_antiq name scan thy = thy
  |> Antiq_Parsers.map (Name_Space.define (Context.Theory thy) true (name, scan) #> snd);

val intern_antiq = Name_Space.intern o #1 o Antiq_Parsers.get;
val defined_antiq = Symtab.defined o #2 o Antiq_Parsers.get;

fun antiquotation src ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;
    val ((xname, _), pos) = Args.dest_src src;
    val (_, scan) = Name_Space.check (Context.Proof ctxt) (Antiq_Parsers.get thy) (xname, pos);
  in Args.context_syntax Markup.ML_antiquotationN (scan pos) src ctxt end;


(* parsing and evaluation *)

local

val antiq =
  Parse.!!! (Parse.position Parse.xname -- Args.parse --| Scan.ahead Parse.eof)
  >> (fn ((x, pos), y) => Args.src ((x, y), pos));

val begin_env0 = ML_Lex.tokenize "structure Isabelle =\nstruct\n";
val begin_env =
  ML_Lex.tokenize
    "structure Isabelle =\nstruct\nval ML_context = ML_Context.the_local_context ();\n";

val end_env = ML_Lex.tokenize "end;";
val reset_env = ML_Lex.tokenize "structure Isabelle = struct end";

in

fun eval_antiquotes (ants, pos) opt_context =
  let
    val opt_ctxt = Option.map (Context.Proof o Context.proof_of) opt_context;
    val ((ml_env, ml_body), opt_ctxt') =
      if forall Antiquote.is_text ants
      then ((begin_env0, map (fn Antiquote.Text tok => tok) ants), opt_ctxt)
      else
        let
          val ctxt =
            (case opt_ctxt of
              NONE => error ("No context -- cannot expand ML antiquotations" ^ Position.here pos)
            | SOME ctxt => Context.proof_of ctxt);

          val lex = #1 (Keyword.get_lexicons ());
          fun no_decl _ = ([], []);

          fun expand (Antiquote.Text tok) state = (K ([], [tok]), state)
            | expand (Antiquote.Antiq (ss, range)) (scope, background) =
                let
                  val context = Stack.top scope;
                  val (f, context') =
                    antiquotation (Token.read_antiq lex antiq (ss, #1 range)) context;
                  val (decl, background') = f background;
                  val decl' = decl #> pairself (ML_Lex.tokenize #> map (ML_Lex.set_range range));
                in (decl', (Stack.map_top (K context') scope, background')) end
            | expand (Antiquote.Open _) (scope, background) =
                (no_decl, (Stack.push scope, background))
            | expand (Antiquote.Close _) (scope, background) =
                (no_decl, (Stack.pop scope, background));

          val (decls, (_, ctxt')) = fold_map expand ants (Stack.init ctxt, ctxt);
          val (ml_env, ml_body) = decls |> map (fn decl => decl ctxt') |> split_list |> pairself flat;
        in ((begin_env @ ml_env, ml_body), SOME (Context.Proof ctxt')) end;
  in ((ml_env @ end_env, ml_body), opt_ctxt') end;

val trace_raw = Config.declare "ML_trace" (fn _ => Config.Bool false);
val trace = Config.bool trace_raw;

fun eval verbose pos ants =
  let
    (*prepare source text*)
    val ((env, body), env_ctxt) = eval_antiquotes (ants, pos) (Context.thread_data ());
    val _ =
      (case Option.map Context.proof_of env_ctxt of
        SOME ctxt =>
          if Config.get ctxt trace then
            Context_Position.if_visible ctxt
              tracing (cat_lines [ML_Lex.flatten env, ML_Lex.flatten body])
          else ()
      | NONE => ());

    (*prepare static ML environment*)
    val _ =
      Context.setmp_thread_data
        (Option.map (Context.mapping I (Context_Position.set_visible false)) env_ctxt)
        (fn () => (ML_Compiler.eval false Position.none env; Context.thread_data ())) ()
      |> (fn NONE => () | SOME context' => Context.>> (ML_Env.inherit context'));

    val _ = ML_Compiler.eval verbose pos body;
    val _ = ML_Compiler.eval false Position.none reset_env;
  in () end;

end;


(* derived versions *)

fun eval_text verbose pos txt = eval verbose pos (ML_Lex.read pos txt);

fun eval_in ctxt verbose pos ants =
  Context.setmp_thread_data (Option.map Context.Proof ctxt) (fn () => eval verbose pos ants) ();

fun eval_text_in ctxt verbose pos txt =
  Context.setmp_thread_data (Option.map Context.Proof ctxt) (fn () => eval_text verbose pos txt) ();

fun expression pos bind body ants =
  exec (fn () => eval false pos
    (ML_Lex.read Position.none ("Context.set_thread_data (SOME (let " ^ bind ^ " = ") @ ants @
      ML_Lex.read Position.none (" in " ^ body ^ " end (ML_Context.the_generic_context ())));")));

end;

structure Basic_ML_Context: BASIC_ML_CONTEXT = ML_Context;
open Basic_ML_Context;
