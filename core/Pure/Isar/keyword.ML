(*  Title:      Pure/Isar/keyword.ML
    Author:     Makarius

Isar keyword classification.
*)

signature KEYWORD =
sig
  val diag: string
  val document_heading: string
  val document_body: string
  val document_raw: string
  val thy_begin: string
  val thy_end: string
  val thy_decl: string
  val thy_decl_block: string
  val thy_load: string
  val thy_goal: string
  val qed: string
  val qed_script: string
  val qed_block: string
  val qed_global: string
  val prf_goal: string
  val prf_block: string
  val prf_open: string
  val prf_close: string
  val prf_chain: string
  val prf_decl: string
  val prf_asm: string
  val prf_asm_goal: string
  val prf_asm_goal_script: string
  val prf_script: string
  type spec = (string * string list) * string list
  type keywords
  val minor_keywords: keywords -> Scan.lexicon
  val major_keywords: keywords -> Scan.lexicon
  val no_command_keywords: keywords -> keywords
  val empty_keywords: keywords
  val merge_keywords: keywords * keywords -> keywords
  val add_keywords: ((string * Position.T) * spec option) list -> keywords -> keywords
  val is_keyword: keywords -> string -> bool
  val is_command: keywords -> string -> bool
  val is_literal: keywords -> string -> bool
  val command_markup: keywords -> string -> Markup.T option
  val command_kind: keywords -> string -> string option
  val command_files: keywords -> string -> Path.T -> Path.T list
  val command_tags: keywords -> string -> string list
  val is_vacuous: keywords -> string -> bool
  val is_diag: keywords -> string -> bool
  val is_document_heading: keywords -> string -> bool
  val is_document_body: keywords -> string -> bool
  val is_document_raw: keywords -> string -> bool
  val is_document: keywords -> string -> bool
  val is_theory_end: keywords -> string -> bool
  val is_theory_load: keywords -> string -> bool
  val is_theory: keywords -> string -> bool
  val is_theory_body: keywords -> string -> bool
  val is_proof: keywords -> string -> bool
  val is_proof_body: keywords -> string -> bool
  val is_theory_goal: keywords -> string -> bool
  val is_proof_goal: keywords -> string -> bool
  val is_qed: keywords -> string -> bool
  val is_qed_global: keywords -> string -> bool
  val is_proof_asm: keywords -> string -> bool
  val is_improper: keywords -> string -> bool
  val is_printed: keywords -> string -> bool
end;

structure Keyword: KEYWORD =
struct

(** keyword classification **)

(* kinds *)

val diag = "diag";
val document_heading = "document_heading";
val document_body = "document_body";
val document_raw = "document_raw";
val thy_begin = "thy_begin";
val thy_end = "thy_end";
val thy_decl = "thy_decl";
val thy_decl_block = "thy_decl_block";
val thy_load = "thy_load";
val thy_goal = "thy_goal";
val qed = "qed";
val qed_script = "qed_script";
val qed_block = "qed_block";
val qed_global = "qed_global";
val prf_goal = "prf_goal";
val prf_block = "prf_block";
val prf_open = "prf_open";
val prf_close = "prf_close";
val prf_chain = "prf_chain";
val prf_decl = "prf_decl";
val prf_asm = "prf_asm";
val prf_asm_goal = "prf_asm_goal";
val prf_asm_goal_script = "prf_asm_goal_script";
val prf_script = "prf_script";

val kinds =
  [diag, document_heading, document_body, document_raw, thy_begin, thy_end, thy_load, thy_decl,
    thy_decl_block, thy_goal, qed, qed_script, qed_block, qed_global, prf_goal, prf_block, prf_open,
    prf_close, prf_chain, prf_decl, prf_asm, prf_asm_goal, prf_asm_goal_script, prf_script];


(* specifications *)

type spec = (string * string list) * string list;

type entry =
 {pos: Position.T,
  id: serial,
  kind: string,
  files: string list,  (*extensions of embedded files*)
  tags: string list};

fun check_spec pos ((kind, files), tags) : entry =
  if not (member (op =) kinds kind) then
    error ("Unknown outer syntax keyword kind " ^ quote kind)
  else if not (null files) andalso kind <> thy_load then
    error ("Illegal specification of files for " ^ quote kind)
  else {pos = pos, id = serial (), kind = kind, files = files, tags = tags};



(** keyword tables **)

(* type keywords *)

datatype keywords = Keywords of
 {minor: Scan.lexicon,
  major: Scan.lexicon,
  commands: entry Symtab.table};

fun minor_keywords (Keywords {minor, ...}) = minor;
fun major_keywords (Keywords {major, ...}) = major;

fun make_keywords (minor, major, commands) =
  Keywords {minor = minor, major = major, commands = commands};

fun map_keywords f (Keywords {minor, major, commands}) =
  make_keywords (f (minor, major, commands));

val no_command_keywords =
  map_keywords (fn (minor, _, _) => (minor, Scan.empty_lexicon, Symtab.empty));


(* build keywords *)

val empty_keywords =
  make_keywords (Scan.empty_lexicon, Scan.empty_lexicon, Symtab.empty);

fun merge_keywords
  (Keywords {minor = minor1, major = major1, commands = commands1},
    Keywords {minor = minor2, major = major2, commands = commands2}) =
  make_keywords
   (Scan.merge_lexicons (minor1, minor2),
    Scan.merge_lexicons (major1, major2),
    Symtab.merge (K true) (commands1, commands2));

val add_keywords =
  fold (fn ((name, pos), opt_spec) => map_keywords (fn (minor, major, commands) =>
    (case opt_spec of
      NONE =>
        let
          val minor' = Scan.extend_lexicon (Symbol.explode name) minor;
        in (minor', major, commands) end
    | SOME spec =>
        let
          val entry = check_spec pos spec;
          val major' = Scan.extend_lexicon (Symbol.explode name) major;
          val commands' = Symtab.update (name, entry) commands;
        in (minor, major', commands') end)));


(* keyword status *)

fun is_keyword keywords s = Scan.is_literal (minor_keywords keywords) (Symbol.explode s);
fun is_command (Keywords {commands, ...}) = Symtab.defined commands;
fun is_literal keywords = is_keyword keywords orf is_command keywords;


(* command keywords *)

fun lookup_command (Keywords {commands, ...}) = Symtab.lookup commands;

fun command_markup keywords name =
  lookup_command keywords name
  |> Option.map (fn {pos, id, ...} =>
      Markup.properties (Position.entity_properties_of false id pos)
        (Markup.entity Markup.command_keywordN name));

fun command_kind keywords = Option.map #kind o lookup_command keywords;

fun command_files keywords name path =
  (case lookup_command keywords name of
    NONE => []
  | SOME {kind, files, ...} =>
      if kind <> thy_load then []
      else if null files then [path]
      else map (fn ext => Path.ext ext path) files);

fun command_tags keywords name =
  (case lookup_command keywords name of
    SOME {tags, ...} => tags
  | NONE => []);


(* command categories *)

fun command_category ks =
  let
    val tab = Symtab.make_set ks;
    fun pred keywords name =
      (case lookup_command keywords name of
        NONE => false
      | SOME {kind, ...} => Symtab.defined tab kind);
  in pred end;

val is_vacuous = command_category [diag, document_heading, document_body, document_raw];

val is_diag = command_category [diag];

val is_document_heading = command_category [document_heading];
val is_document_body = command_category [document_body];
val is_document_raw = command_category [document_raw];
val is_document = command_category [document_heading, document_body, document_raw];

val is_theory_end = command_category [thy_end];

val is_theory_load = command_category [thy_load];

val is_theory = command_category
  [thy_begin, thy_end, thy_load, thy_decl, thy_decl_block, thy_goal];

val is_theory_body = command_category
  [thy_load, thy_decl, thy_decl_block, thy_goal];

val is_proof = command_category
  [qed, qed_script, qed_block, qed_global, prf_goal, prf_block, prf_open, prf_close,
    prf_chain, prf_decl, prf_asm, prf_asm_goal, prf_asm_goal_script, prf_script];

val is_proof_body = command_category
  [diag, document_heading, document_body, document_raw, prf_block, prf_open, prf_close,
    prf_chain, prf_decl, prf_asm, prf_asm_goal, prf_asm_goal_script, prf_script];

val is_theory_goal = command_category [thy_goal];
val is_proof_goal = command_category [prf_goal, prf_asm_goal, prf_asm_goal_script];
val is_qed = command_category [qed, qed_script, qed_block];
val is_qed_global = command_category [qed_global];

val is_proof_asm = command_category [prf_asm, prf_asm_goal];
val is_improper = command_category [qed_script, prf_script, prf_asm_goal_script];

fun is_printed keywords = is_theory_goal keywords orf is_proof keywords;

end;
