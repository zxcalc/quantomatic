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
  val struct_name: Proof.context -> string
  val variant: string -> Proof.context -> string * Proof.context
  type decl = Proof.context -> string * string
  val value_decl: string -> string -> Proof.context -> decl * Proof.context
  val add_antiquotation: binding -> (Token.src -> Proof.context -> decl * Proof.context) ->
    theory -> theory
  val print_antiquotations: bool -> Proof.context -> unit
  val eval: ML_Compiler.flags -> Position.T -> ML_Lex.token Antiquote.antiquote list -> unit
  val eval_file: ML_Compiler.flags -> Path.T -> unit
  val eval_source: ML_Compiler.flags -> Input.source -> unit
  val eval_in: Proof.context option -> ML_Compiler.flags -> Position.T ->
    ML_Lex.token Antiquote.antiquote list -> unit
  val eval_source_in: Proof.context option -> ML_Compiler.flags -> Input.source -> unit
  val expression: Position.range -> string -> string -> string ->
    ML_Lex.token Antiquote.antiquote list -> Context.generic -> Context.generic
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

(* names for generated environment *)

structure Names = Proof_Data
(
  type T = string * Name.context;
  val init_names = ML_Syntax.reserved |> fold Name.declare ["ML_context", "ML_print_depth"];
  fun init _ = ("Isabelle0", init_names);
);

fun struct_name ctxt = #1 (Names.get ctxt);
val struct_begin = (Names.map o apfst) (fn _ => "Isabelle" ^ serial_string ());

fun variant a ctxt =
  let
    val names = #2 (Names.get ctxt);
    val (b, names') = Name.variant (Name.desymbolize (SOME false) a) names;
    val ctxt' = (Names.map o apsnd) (K names') ctxt;
  in (b, ctxt') end;


(* decl *)

type decl = Proof.context -> string * string;  (*final context -> ML env, ML body*)

fun value_decl a s ctxt =
  let
    val (b, ctxt') = variant a ctxt;
    val env = "val " ^ b ^ " = " ^ s ^ ";\n";
    val body = struct_name ctxt ^ "." ^ b;
    fun decl (_: Proof.context) = (env, body);
  in (decl, ctxt') end;


(* theory data *)

structure Antiquotations = Theory_Data
(
  type T = (Token.src -> Proof.context -> decl * Proof.context) Name_Space.table;
  val empty : T = Name_Space.empty_table Markup.ML_antiquotationN;
  val extend = I;
  fun merge data : T = Name_Space.merge_tables data;
);

val get_antiquotations = Antiquotations.get o Proof_Context.theory_of;

fun check_antiquotation ctxt =
  #1 o Name_Space.check (Context.Proof ctxt) (get_antiquotations ctxt);

fun add_antiquotation name f thy = thy
  |> Antiquotations.map (Name_Space.define (Context.Theory thy) true (name, f) #> snd);

fun print_antiquotations verbose ctxt =
  Pretty.big_list "ML antiquotations:"
    (map (Pretty.mark_str o #1) (Name_Space.markup_table verbose ctxt (get_antiquotations ctxt)))
  |> Pretty.writeln;

fun apply_antiquotation src ctxt =
  let val (src', f) = Token.check_src ctxt (get_antiquotations ctxt) src
  in f src' ctxt end;


(* parsing and evaluation *)

local

val antiq =
  Parse.!!! (Parse.position Parse.xname -- Parse.args --| Scan.ahead Parse.eof)
  >> uncurry Token.src;

fun make_env name visible =
  (ML_Lex.tokenize
    ("structure " ^ name ^ " =\nstruct\n\
     \val ML_context = Context_Position.set_visible " ^ Bool.toString visible ^
     " (ML_Context.the_local_context ());\n\
     \val ML_print_depth =\n\
     \  let val default = ML_Options.get_print_depth ()\n\
     \  in fn () => ML_Options.get_print_depth_default default end;\n"),
   ML_Lex.tokenize "end;");

fun reset_env name = ML_Lex.tokenize ("structure " ^ name ^ " = struct end");

fun expanding (Antiquote.Text tok) = ML_Lex.is_cartouche tok
  | expanding (Antiquote.Antiq _) = true;

fun eval_antiquotes (ants, pos) opt_context =
  let
    val visible =
      (case opt_context of
        SOME (Context.Proof ctxt) => Context_Position.is_visible ctxt
      | _ => true);
    val opt_ctxt = Option.map Context.proof_of opt_context;

    val ((ml_env, ml_body), opt_ctxt') =
      if forall (not o expanding) ants
      then (([], map (fn Antiquote.Text tok => tok) ants), opt_ctxt)
      else
        let
          fun tokenize range = apply2 (ML_Lex.tokenize #> map (ML_Lex.set_range range));

          fun expand (Antiquote.Antiq (ss, {range, ...})) ctxt =
                let
                  val keywords = Thy_Header.get_keywords' ctxt;
                  val (decl, ctxt') =
                    apply_antiquotation (Token.read_antiq keywords antiq (ss, #1 range)) ctxt;
                in (decl #> tokenize range, ctxt') end
            | expand (Antiquote.Text tok) ctxt =
                if ML_Lex.is_cartouche tok then
                  let
                    val range = ML_Lex.range_of tok;
                    val text =
                      Symbol_Pos.explode (ML_Lex.content_of tok, #1 range)
                      |> Symbol_Pos.cartouche_content
                      |> Symbol_Pos.implode_range range |> #1;
                    val (decl, ctxt') =
                      value_decl "input"
                        ("Input.source true " ^ ML_Syntax.print_string text  ^ " " ^
                          ML_Syntax.atomic (ML_Syntax.print_range range)) ctxt;
                  in (decl #> tokenize range, ctxt') end
                else (K ([], [tok]), ctxt);

          val ctxt =
            (case opt_ctxt of
              NONE => error ("No context -- cannot expand ML antiquotations" ^ Position.here pos)
            | SOME ctxt => struct_begin ctxt);

          val (begin_env, end_env) = make_env (struct_name ctxt) visible;
          val (decls, ctxt') = fold_map expand ants ctxt;
          val (ml_env, ml_body) =
            decls |> map (fn decl => decl ctxt') |> split_list |> apply2 flat;
        in ((begin_env @ ml_env @ end_env, ml_body), SOME ctxt') end;
  in ((ml_env, ml_body), opt_ctxt') end;

in

fun eval flags pos ants =
  let
    val non_verbose = ML_Compiler.verbose false flags;

    (*prepare source text*)
    val ((env, body), env_ctxt) = eval_antiquotes (ants, pos) (Context.thread_data ());
    val _ =
      (case env_ctxt of
        SOME ctxt =>
          if Config.get ctxt ML_Options.source_trace andalso Context_Position.is_visible ctxt
          then tracing (cat_lines [ML_Lex.flatten env, ML_Lex.flatten body])
          else ()
      | NONE => ());

    (*prepare environment*)
    val _ =
      Context.setmp_thread_data
        (Option.map (Context.Proof o Context_Position.set_visible false) env_ctxt)
        (fn () => (ML_Compiler.eval non_verbose Position.none env; Context.thread_data ())) ()
      |> (fn NONE => () | SOME context' => Context.>> (ML_Env.inherit context'));

    (*eval body*)
    val _ = ML_Compiler.eval flags pos body;

    (*clear environment*)
    val _ =
      (case (env_ctxt, is_some (Context.thread_data ())) of
        (SOME ctxt, true) =>
          let
            val name = struct_name ctxt;
            val _ = ML_Compiler.eval non_verbose Position.none (reset_env name);
            val _ = Context.>> (ML_Env.forget_structure name);
          in () end
      | _ => ());
  in () end;

end;


(* derived versions *)

fun eval_file flags path =
  let val pos = Path.position path
  in eval flags pos (ML_Lex.read_pos pos (File.read path)) end;

fun eval_source flags source =
  eval flags (Input.pos_of source) (ML_Lex.read_source (#SML flags) source);

fun eval_in ctxt flags pos ants =
  Context.setmp_thread_data (Option.map Context.Proof ctxt)
    (fn () => eval flags pos ants) ();

fun eval_source_in ctxt flags source =
  Context.setmp_thread_data (Option.map Context.Proof ctxt)
    (fn () => eval_source flags source) ();

fun expression range name constraint body ants =
  exec (fn () =>
    eval ML_Compiler.flags (#1 range)
     (ML_Lex.read "Context.set_thread_data (SOME (let val " @ ML_Lex.read_set_range range name @
      ML_Lex.read (": " ^ constraint ^ " =") @ ants @
      ML_Lex.read ("in " ^ body ^ " end (ML_Context.the_generic_context ())));")));

end;

