(*  Title:      Pure/ML/ml_context.ML
    Author:     Makarius

ML context and antiquotations.
*)

signature ML_CONTEXT =
sig
  val the_generic_context: unit -> Context.generic
  val the_global_context: unit -> theory
  val the_local_context: unit -> Proof.context
  val thm: xstring -> thm
  val thms: xstring -> thm list
  val exec: (unit -> unit) -> Context.generic -> Context.generic
  val check_antiquotation: Proof.context -> xstring * Position.T -> string
  type decl = Proof.context -> string * string
  val add_antiquotation: binding -> (Args.src -> Proof.context -> decl * Proof.context) ->
    theory -> theory
  val print_antiquotations: Proof.context -> unit
  val eval_antiquotes: ML_Lex.token Antiquote.antiquote list * Position.T ->
    Context.generic option -> (ML_Lex.token list * ML_Lex.token list) * Context.generic option
  val eval: ML_Compiler.flags -> Position.T -> ML_Lex.token Antiquote.antiquote list -> unit
  val eval_file: ML_Compiler.flags -> Path.T -> unit
  val eval_source: ML_Compiler.flags -> Symbol_Pos.source -> unit
  val eval_in: Proof.context option -> ML_Compiler.flags -> Position.T ->
    ML_Lex.token Antiquote.antiquote list -> unit
  val eval_source_in: Proof.context option -> ML_Compiler.flags -> Symbol_Pos.source -> unit
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



(** ML antiquotations **)

(* theory data *)

type decl = Proof.context -> string * string;  (*final context -> ML env, ML body*)
structure Antiquotations = Theory_Data
(
  type T = (Args.src -> Proof.context -> decl * Proof.context) Name_Space.table;
  val empty : T = Name_Space.empty_table Markup.ML_antiquotationN;
  val extend = I;
  fun merge data : T = Name_Space.merge_tables data;
);

val get_antiquotations = Antiquotations.get o Proof_Context.theory_of;

fun check_antiquotation ctxt =
  #1 o Name_Space.check (Context.Proof ctxt) (get_antiquotations ctxt);

fun add_antiquotation name f thy = thy
  |> Antiquotations.map (Name_Space.define (Context.Theory thy) true (name, f) #> snd);

fun print_antiquotations ctxt =
  Pretty.big_list "ML antiquotations:"
    (map (Pretty.mark_str o #1) (Name_Space.markup_table ctxt (get_antiquotations ctxt)))
  |> Pretty.writeln;

fun apply_antiquotation src ctxt =
  let val (src', f) = Args.check_src ctxt (get_antiquotations ctxt) src
  in f src' ctxt end;


(* parsing and evaluation *)

local

val antiq =
  Parse.!!! (Parse.position Parse.xname -- Parse.args --| Scan.ahead Parse.eof)
  >> uncurry Args.src;

val begin_env0 = ML_Lex.tokenize "structure Isabelle =\nstruct\n";

fun begin_env visible =
  ML_Lex.tokenize
    ("structure Isabelle =\nstruct\n\
     \val ML_context = Context_Position.set_visible " ^ Bool.toString visible ^
     " (ML_Context.the_local_context ());\n\
     \val ML_print_depth =\n\
     \  let val default = ML_Options.get_print_depth ()\n\
     \  in fn () => ML_Options.get_print_depth_default default end;\n");

val end_env = ML_Lex.tokenize "end;";
val reset_env = ML_Lex.tokenize "structure Isabelle = struct end";

in

fun eval_antiquotes (ants, pos) opt_context =
  let
    val visible =
      (case opt_context of
        SOME (Context.Proof ctxt) => Context_Position.is_visible ctxt
      | _ => true);
    val opt_ctxt = Option.map (Context.Proof o Context.proof_of) opt_context;

    val ((ml_env, ml_body), opt_ctxt') =
      if forall Antiquote.is_text ants
      then ((begin_env0, map (fn Antiquote.Text tok => tok) ants), opt_ctxt)
      else
        let
          val lex = #1 (Keyword.get_lexicons ());
          fun no_decl _ = ([], []);

          fun expand (Antiquote.Text tok) ctxt = (K ([], [tok]), ctxt)
            | expand (Antiquote.Antiq (ss, {range, ...})) ctxt =
                let
                  val (decl, ctxt') =
                    apply_antiquotation (Token.read_antiq lex antiq (ss, #1 range)) ctxt;
                  val decl' = decl #> pairself (ML_Lex.tokenize #> map (ML_Lex.set_range range));
                in (decl', ctxt') end;

          val ctxt =
            (case opt_ctxt of
              NONE => error ("No context -- cannot expand ML antiquotations" ^ Position.here pos)
            | SOME ctxt => Context.proof_of ctxt);

          val (decls, ctxt') = fold_map expand ants ctxt;
          val (ml_env, ml_body) =
            decls |> map (fn decl => decl ctxt') |> split_list |> pairself flat;
        in ((begin_env visible @ ml_env, ml_body), SOME (Context.Proof ctxt')) end;
  in ((ml_env @ end_env, ml_body), opt_ctxt') end;

fun eval flags pos ants =
  let
    val non_verbose = ML_Compiler.verbose false flags;

    (*prepare source text*)
    val ((env, body), env_ctxt) = eval_antiquotes (ants, pos) (Context.thread_data ());
    val _ =
      (case Option.map Context.proof_of env_ctxt of
        SOME ctxt =>
          if Config.get ctxt ML_Options.source_trace andalso Context_Position.is_visible ctxt
          then tracing (cat_lines [ML_Lex.flatten env, ML_Lex.flatten body])
          else ()
      | NONE => ());

    (*prepare static ML environment*)
    val _ =
      Context.setmp_thread_data
        (Option.map (Context.mapping I (Context_Position.set_visible false)) env_ctxt)
        (fn () => (ML_Compiler.eval non_verbose Position.none env; Context.thread_data ())) ()
      |> (fn NONE => () | SOME context' => Context.>> (ML_Env.inherit context'));

    val _ = ML_Compiler.eval flags pos body;
    val _ = ML_Compiler.eval non_verbose Position.none reset_env;
  in () end;

end;


(* derived versions *)

fun eval_file flags path =
  let val pos = Path.position path
  in eval flags pos (ML_Lex.read pos (File.read path)) end;

fun eval_source flags source =
  eval flags (#pos source) (ML_Lex.read_source (#SML flags) source);

fun eval_in ctxt flags pos ants =
  Context.setmp_thread_data (Option.map Context.Proof ctxt)
    (fn () => eval flags pos ants) ();

fun eval_source_in ctxt flags source =
  Context.setmp_thread_data (Option.map Context.Proof ctxt)
    (fn () => eval_source flags source) ();

fun expression pos bind body ants =
  exec (fn () =>
    eval ML_Compiler.flags pos
     (ML_Lex.read Position.none ("Context.set_thread_data (SOME (let " ^ bind ^ " = ") @ ants @
      ML_Lex.read Position.none (" in " ^ body ^ " end (ML_Context.the_generic_context ())));")));

end;

