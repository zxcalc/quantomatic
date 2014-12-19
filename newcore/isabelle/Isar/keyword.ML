(*  Title:      Pure/Isar/keyword.ML
    Author:     Makarius

Isar command keyword classification and global keyword tables.
*)

signature KEYWORD =
sig
  type T
  val kind_of: T -> string
  val kind_files_of: T -> string * string list
  val control: T
  val diag: T
  val thy_begin: T
  val thy_end: T
  val thy_heading1: T
  val thy_heading2: T
  val thy_heading3: T
  val thy_heading4: T
  val thy_decl: T
  val thy_load: T
  val thy_load_files: string list -> T
  val thy_goal: T
  val qed: T
  val qed_script: T
  val qed_block: T
  val qed_global: T
  val prf_heading2: T
  val prf_heading3: T
  val prf_heading4: T
  val prf_goal: T
  val prf_block: T
  val prf_open: T
  val prf_close: T
  val prf_chain: T
  val prf_decl: T
  val prf_asm: T
  val prf_asm_goal: T
  val prf_asm_goal_script: T
  val prf_script: T
  val kinds: T list
  val tag: string -> T -> T
  val tags_of: T -> string list
  val tag_theory: T -> T
  val tag_proof: T -> T
  val tag_ml: T -> T
  type spec = (string * string list) * string list
  val spec: spec -> T
  val command_spec: (string * spec) * Position.T -> (string * T) * Position.T
  val get_lexicons: unit -> Scan.lexicon * Scan.lexicon
  val is_keyword: string -> bool
  val command_keyword: string -> T option
  val command_files: string -> Path.T -> Path.T list
  val command_tags: string -> string list
  val dest: unit -> string list * string list
  val define: string * T option -> unit
  val is_diag: string -> bool
  val is_control: string -> bool
  val is_regular: string -> bool
  val is_heading: string -> bool
  val is_theory_begin: string -> bool
  val is_theory_load: string -> bool
  val is_theory: string -> bool
  val is_theory_body: string -> bool
  val is_proof: string -> bool
  val is_proof_body: string -> bool
  val is_theory_goal: string -> bool
  val is_proof_goal: string -> bool
  val is_qed: string -> bool
  val is_qed_global: string -> bool
  val is_printed: string -> bool
end;

structure Keyword: KEYWORD =
struct

(** keyword classification **)

datatype T = Keyword of
 {kind: string,
  files: string list,  (*extensions of embedded files*)
  tags: string list};  (*tags in canonical reverse order*)

fun kind s = Keyword {kind = s, files = [], tags = []};
fun kind_of (Keyword {kind, ...}) = kind;
fun kind_files_of (Keyword {kind, files, ...}) = (kind, files);

fun add_files fs (Keyword {kind, files, tags}) =
  Keyword {kind = kind, files = files @ fs, tags = tags};


(* kinds *)

val control = kind "control";
val diag = kind "diag";
val thy_begin = kind "thy_begin";
val thy_end = kind "thy_end";
val thy_heading1 = kind "thy_heading1";
val thy_heading2 = kind "thy_heading2";
val thy_heading3 = kind "thy_heading3";
val thy_heading4 = kind "thy_heading4";
val thy_decl = kind "thy_decl";
val thy_load = kind "thy_load";
fun thy_load_files files = Keyword {kind = "thy_load", files = files, tags = []};
val thy_goal = kind "thy_goal";
val qed = kind "qed";
val qed_script = kind "qed_script";
val qed_block = kind "qed_block";
val qed_global = kind "qed_global";
val prf_heading2 = kind "prf_heading2";
val prf_heading3 = kind "prf_heading3";
val prf_heading4 = kind "prf_heading4";
val prf_goal = kind "prf_goal";
val prf_block = kind "prf_block";
val prf_open = kind "prf_open";
val prf_close = kind "prf_close";
val prf_chain = kind "prf_chain";
val prf_decl = kind "prf_decl";
val prf_asm = kind "prf_asm";
val prf_asm_goal = kind "prf_asm_goal";
val prf_asm_goal_script = kind "prf_asm_goal_script";
val prf_script = kind "prf_script";

val kinds =
  [control, diag, thy_begin, thy_end, thy_heading1, thy_heading2, thy_heading3, thy_heading4,
    thy_load, thy_decl, thy_goal, qed, qed_script, qed_block, qed_global,
    prf_heading2, prf_heading3, prf_heading4, prf_goal, prf_block, prf_open,
    prf_close, prf_chain, prf_decl, prf_asm, prf_asm_goal, prf_asm_goal_script, prf_script];


(* tags *)

fun tag t (Keyword {kind, files, tags}) =
  Keyword {kind = kind, files = files, tags = update (op =) t tags};
fun tags_of (Keyword {tags, ...}) = tags;

val tag_theory = tag "theory";
val tag_proof = tag "proof";
val tag_ml = tag "ML";


(* external names *)

val name_table = Symtab.make (map (`kind_of) kinds);

type spec = (string * string list) * string list;

fun spec ((name, files), tags) =
  (case Symtab.lookup name_table name of
    SOME kind =>
      let val kind' = kind |> fold tag tags in
        if null files then kind'
        else if name = kind_of thy_load then kind' |> add_files files
        else error ("Illegal specification of files for " ^ quote name)
      end
  | NONE => error ("Unknown outer syntax keyword kind " ^ quote name));

fun command_spec ((name, s), pos) = ((name, spec s), pos);



(** global keyword tables **)

datatype keywords = Keywords of
 {lexicons: Scan.lexicon * Scan.lexicon,  (*minor, major*)
  commands: T Symtab.table};  (*command classification*)

fun make_keywords (lexicons, commands) =
  Keywords {lexicons = lexicons, commands = commands};

local

val global_keywords =
  Unsynchronized.ref (make_keywords ((Scan.empty_lexicon, Scan.empty_lexicon), Symtab.empty));

in

fun get_keywords () = ! global_keywords;

fun change_keywords f = CRITICAL (fn () =>
  Unsynchronized.change global_keywords
    (fn Keywords {lexicons, commands} => make_keywords (f (lexicons, commands))));

end;

fun get_lexicons () = get_keywords () |> (fn Keywords {lexicons, ...} => lexicons);
fun get_commands () = get_keywords () |> (fn Keywords {commands, ...} => commands);


(* lookup *)

fun is_keyword s =
  let
    val (minor, major) = get_lexicons ();
    val syms = Symbol.explode s;
  in Scan.is_literal minor syms orelse Scan.is_literal major syms end;

fun command_keyword name = Symtab.lookup (get_commands ()) name;

fun command_files name path =
  (case command_keyword name of
    NONE => []
  | SOME (Keyword {kind, files, ...}) =>
      if kind <> kind_of thy_load then []
      else if null files then [path]
      else map (fn ext => Path.ext ext path) files);

val command_tags = these o Option.map tags_of o command_keyword;

fun dest () = pairself (sort_strings o Scan.dest_lexicon) (get_lexicons ());


(* define *)

fun define (name, opt_kind) = change_keywords (fn ((minor, major), commands) =>
  (case opt_kind of
    NONE =>
      let
        val minor' = Scan.extend_lexicon (Symbol.explode name) minor;
      in ((minor', major), commands) end
  | SOME kind =>
      let
        val major' = Scan.extend_lexicon (Symbol.explode name) major;
        val commands' = Symtab.update (name, kind) commands;
      in ((minor, major'), commands') end));


(* command categories *)

fun command_category ks =
  let val tab = Symtab.make_set (map kind_of ks) in
    fn name =>
      (case command_keyword name of
        NONE => false
      | SOME k => Symtab.defined tab (kind_of k))
  end;

val is_diag = command_category [diag];
val is_control = command_category [control];
val is_regular = not o command_category [diag, control];

val is_heading =
  command_category [thy_heading1, thy_heading2, thy_heading3, thy_heading4,
    prf_heading2, prf_heading3, prf_heading4];

val is_theory_begin = command_category [thy_begin];

val is_theory_load = command_category [thy_load];

val is_theory = command_category
  [thy_begin, thy_end, thy_heading1, thy_heading2, thy_heading3, thy_heading4,
    thy_load, thy_decl, thy_goal];

val is_theory_body = command_category
  [thy_heading1, thy_heading2, thy_heading3, thy_heading4, thy_load, thy_decl, thy_goal];

val is_proof = command_category
  [qed, qed_script, qed_block, qed_global, prf_heading2, prf_heading3, prf_heading4,
    prf_goal, prf_block, prf_open, prf_close, prf_chain, prf_decl,
    prf_asm, prf_asm_goal, prf_asm_goal_script, prf_script];

val is_proof_body = command_category
  [diag, prf_heading2, prf_heading3, prf_heading4, prf_block, prf_open, prf_close, prf_chain,
    prf_decl, prf_asm, prf_asm_goal, prf_asm_goal_script, prf_script];

val is_theory_goal = command_category [thy_goal];
val is_proof_goal = command_category [prf_goal, prf_asm_goal, prf_asm_goal_script];
val is_qed = command_category [qed, qed_script, qed_block];
val is_qed_global = command_category [qed_global];

val is_printed = is_theory_goal orf is_proof;

end;

