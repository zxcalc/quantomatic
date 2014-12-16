(*  Title:      Pure/PIDE/markup.ML
    Author:     Makarius

Isabelle-specific implementation of quasi-abstract markup elements.
*)

signature MARKUP =
sig
  val parse_int: string -> int
  val print_int: int -> string
  type T = string * Properties.T
  val empty: T
  val is_empty: T -> bool
  val properties: Properties.T -> T -> T
  val nameN: string
  val name: string -> T -> T
  val kindN: string
  val bindingN: string val binding: T
  val entityN: string val entity: string -> string -> T
  val get_entity_kind: T -> string option
  val defN: string
  val refN: string
  val lineN: string
  val offsetN: string
  val end_offsetN: string
  val fileN: string
  val idN: string
  val position_properties': string list
  val position_properties: string list
  val positionN: string val position: T
  val pathN: string val path: string -> T
  val indentN: string
  val blockN: string val block: int -> T
  val widthN: string
  val breakN: string val break: int -> T
  val fbreakN: string val fbreak: T
  val hiddenN: string val hidden: T
  val theoryN: string
  val classN: string
  val type_nameN: string
  val constantN: string
  val fixedN: string val fixed: string -> T
  val dynamic_factN: string val dynamic_fact: string -> T
  val tfreeN: string val tfree: T
  val tvarN: string val tvar: T
  val freeN: string val free: T
  val skolemN: string val skolem: T
  val boundN: string val bound: T
  val varN: string val var: T
  val numeralN: string val numeral: T
  val literalN: string val literal: T
  val delimiterN: string val delimiter: T
  val inner_stringN: string val inner_string: T
  val inner_commentN: string val inner_comment: T
  val token_rangeN: string val token_range: T
  val sortN: string val sort: T
  val typN: string val typ: T
  val termN: string val term: T
  val propN: string val prop: T
  val sortingN: string val sorting: T
  val typingN: string val typing: T
  val ML_keywordN: string val ML_keyword: T
  val ML_delimiterN: string val ML_delimiter: T
  val ML_tvarN: string val ML_tvar: T
  val ML_numeralN: string val ML_numeral: T
  val ML_charN: string val ML_char: T
  val ML_stringN: string val ML_string: T
  val ML_commentN: string val ML_comment: T
  val ML_defN: string
  val ML_openN: string
  val ML_structN: string
  val ML_typingN: string val ML_typing: T
  val ML_sourceN: string val ML_source: T
  val doc_sourceN: string val doc_source: T
  val antiqN: string val antiq: T
  val ML_antiquotationN: string
  val document_antiquotationN: string
  val document_antiquotation_optionN: string
  val paragraphN: string val paragraph: T
  val text_foldN: string val text_fold: T
  val keywordN: string val keyword: T
  val operatorN: string val operator: T
  val commandN: string val command: T
  val stringN: string val string: T
  val altstringN: string val altstring: T
  val verbatimN: string val verbatim: T
  val commentN: string val comment: T
  val controlN: string val control: T
  val tokenN: string val token: Properties.T -> T
  val keyword1N: string val keyword1: T
  val keyword2N: string val keyword2: T
  val elapsedN: string
  val cpuN: string
  val gcN: string
  val timing_properties: Timing.timing -> Properties.T
  val timingN: string val timing: Timing.timing -> T
  val subgoalsN: string
  val proof_stateN: string val proof_state: int -> T
  val stateN: string val state: T
  val goalN: string val goal: T
  val subgoalN: string val subgoal: string -> T
  val taskN: string
  val acceptedN: string val accepted: T
  val forkedN: string val forked: T
  val joinedN: string val joined: T
  val runningN: string val running: T
  val finishedN: string val finished: T
  val failedN: string val failed: T
  val serialN: string
  val exec_idN: string
  val initN: string
  val statusN: string
  val resultN: string
  val writelnN: string
  val tracingN: string
  val warningN: string
  val errorN: string
  val protocolN: string
  val legacyN: string val legacy: T
  val promptN: string val prompt: T
  val reportN: string val report: T
  val no_reportN: string val no_report: T
  val badN: string val bad: T
  val intensifyN: string val intensify: T
  val browserN: string
  val graphviewN: string
  val sendbackN: string
  val paddingN: string
  val padding_line: Properties.entry
  val dialogN: string val dialog: serial -> string -> T
  val functionN: string
  val assign_execs: Properties.T
  val removed_versions: Properties.T
  val invoke_scala: string -> string -> Properties.T
  val cancel_scala: string -> Properties.T
  val ML_statistics: Properties.entry
  val task_statistics: Properties.entry
  val loading_theory: string -> Properties.T
  val dest_loading_theory: Properties.T -> string option
  val no_output: Output.output * Output.output
  val default_output: T -> Output.output * Output.output
  val add_mode: string -> (T -> Output.output * Output.output) -> unit
  val output: T -> Output.output * Output.output
  val enclose: T -> Output.output -> Output.output
  val markup: T -> string -> string
  val markup_only: T -> string
end;

structure Markup: MARKUP =
struct

(** markup elements **)

(* integers *)

fun parse_int s =
  let val i = Int.fromString s in
    if is_none i orelse String.isPrefix "~" s
    then raise Fail ("Bad integer: " ^ quote s)
    else the i
  end;

val print_int = signed_string_of_int;


(* basic markup *)

type T = string * Properties.T;

val empty = ("", []);

fun is_empty ("", _) = true
  | is_empty _ = false;


fun properties more_props ((elem, props): T) =
  (elem, fold_rev Properties.put more_props props);

fun markup_elem elem = (elem, (elem, []): T);
fun markup_string elem prop = (elem, fn s => (elem, [(prop, s)]): T);
fun markup_int elem prop = (elem, fn i => (elem, [(prop, print_int i)]): T);


(* misc properties *)

val nameN = "name";
fun name a = properties [(nameN, a)];

val kindN = "kind";


(* formal entities *)

val (bindingN, binding) = markup_elem "binding";

val entityN = "entity";
fun entity kind name = (entityN, [(nameN, name), (kindN, kind)]);

fun get_entity_kind (name, props) =
  if name = entityN then AList.lookup (op =) props kindN
  else NONE;

val defN = "def";
val refN = "ref";


(* position *)

val lineN = "line";
val offsetN = "offset";
val end_offsetN = "end_offset";
val fileN = "file";
val idN = "id";

val position_properties' = [fileN, idN];
val position_properties = [lineN, offsetN, end_offsetN] @ position_properties';

val (positionN, position) = markup_elem "position";


(* path *)

val (pathN, path) = markup_string "path" nameN;


(* pretty printing *)

val indentN = "indent";
val (blockN, block) = markup_int "block" indentN;

val widthN = "width";
val (breakN, break) = markup_int "break" widthN;

val (fbreakN, fbreak) = markup_elem "fbreak";


(* hidden text *)

val (hiddenN, hidden) = markup_elem "hidden";


(* logical entities *)

val theoryN = "theory";
val classN = "class";
val type_nameN = "type_name";
val constantN = "constant";

val (fixedN, fixed) = markup_string "fixed" nameN;
val (dynamic_factN, dynamic_fact) = markup_string "dynamic_fact" nameN;


(* inner syntax *)

val (tfreeN, tfree) = markup_elem "tfree";
val (tvarN, tvar) = markup_elem "tvar";
val (freeN, free) = markup_elem "free";
val (skolemN, skolem) = markup_elem "skolem";
val (boundN, bound) = markup_elem "bound";
val (varN, var) = markup_elem "var";
val (numeralN, numeral) = markup_elem "numeral";
val (literalN, literal) = markup_elem "literal";
val (delimiterN, delimiter) = markup_elem "delimiter";
val (inner_stringN, inner_string) = markup_elem "inner_string";
val (inner_commentN, inner_comment) = markup_elem "inner_comment";

val (token_rangeN, token_range) = markup_elem "token_range";

val (sortN, sort) = markup_elem "sort";
val (typN, typ) = markup_elem "typ";
val (termN, term) = markup_elem "term";
val (propN, prop) = markup_elem "prop";

val (sortingN, sorting) = markup_elem "sorting";
val (typingN, typing) = markup_elem "typing";


(* ML syntax *)

val (ML_keywordN, ML_keyword) = markup_elem "ML_keyword";
val (ML_delimiterN, ML_delimiter) = markup_elem "ML_delimiter";
val (ML_tvarN, ML_tvar) = markup_elem "ML_tvar";
val (ML_numeralN, ML_numeral) = markup_elem "ML_numeral";
val (ML_charN, ML_char) = markup_elem "ML_char";
val (ML_stringN, ML_string) = markup_elem "ML_string";
val (ML_commentN, ML_comment) = markup_elem "ML_comment";

val ML_defN = "ML_def";
val ML_openN = "ML_open";
val ML_structN = "ML_struct";
val (ML_typingN, ML_typing) = markup_elem "ML_typing";


(* embedded source text *)

val (ML_sourceN, ML_source) = markup_elem "ML_source";
val (doc_sourceN, doc_source) = markup_elem "doc_source";

val (antiqN, antiq) = markup_elem "antiq";
val ML_antiquotationN = "ML_antiquotation";
val document_antiquotationN = "document_antiquotation";
val document_antiquotation_optionN = "document_antiquotation_option";


(* text structure *)

val (paragraphN, paragraph) = markup_elem "paragraph";
val (text_foldN, text_fold) = markup_elem "text_fold";


(* outer syntax *)

val (keywordN, keyword) = markup_elem "keyword";
val (operatorN, operator) = markup_elem "operator";
val (commandN, command) = markup_elem "command";
val (stringN, string) = markup_elem "string";
val (altstringN, altstring) = markup_elem "altstring";
val (verbatimN, verbatim) = markup_elem "verbatim";
val (commentN, comment) = markup_elem "comment";
val (controlN, control) = markup_elem "control";

val tokenN = "token";
fun token props = (tokenN, props);

val (keyword1N, keyword1) = markup_elem "keyword1";
val (keyword2N, keyword2) = markup_elem "keyword2";


(* timing *)

val elapsedN = "elapsed";
val cpuN = "cpu";
val gcN = "gc";

fun timing_properties {elapsed, cpu, gc} =
 [(elapsedN, Time.toString elapsed),
  (cpuN, Time.toString cpu),
  (gcN, Time.toString gc)];

val timingN = "timing";
fun timing t = (timingN, timing_properties t);


(* toplevel *)

val subgoalsN = "subgoals";
val (proof_stateN, proof_state) = markup_int "proof_state" subgoalsN;

val (stateN, state) = markup_elem "state";
val (goalN, goal) = markup_elem "goal";
val (subgoalN, subgoal) = markup_string "subgoal" nameN;


(* command status *)

val taskN = "task";

val (acceptedN, accepted) = markup_elem "accepted";
val (forkedN, forked) = markup_elem "forked";
val (joinedN, joined) = markup_elem "joined";
val (runningN, running) = markup_elem "running";
val (finishedN, finished) = markup_elem "finished";
val (failedN, failed) = markup_elem "failed";


(* messages *)

val serialN = "serial";
val exec_idN = "exec_id";

val initN = "init";
val statusN = "status";
val resultN = "result";
val writelnN = "writeln";
val tracingN = "tracing";
val warningN = "warning";
val errorN = "error";
val protocolN = "protocol";

val (legacyN, legacy) = markup_elem "legacy";
val (promptN, prompt) = markup_elem "prompt";

val (reportN, report) = markup_elem "report";
val (no_reportN, no_report) = markup_elem "no_report";

val (badN, bad) = markup_elem "bad";

val (intensifyN, intensify) = markup_elem "intensify";


(* active areas *)

val browserN = "browser"
val graphviewN = "graphview";

val sendbackN = "sendback";
val paddingN = "padding";
val padding_line = (paddingN, lineN);

val dialogN = "dialog";
fun dialog i result = (dialogN, [(serialN, print_int i), (resultN, result)]);


(* protocol message functions *)

val functionN = "function"

val assign_execs = [(functionN, "assign_execs")];
val removed_versions = [(functionN, "removed_versions")];

fun invoke_scala name id = [(functionN, "invoke_scala"), (nameN, name), (idN, id)];
fun cancel_scala id = [(functionN, "cancel_scala"), (idN, id)];

val ML_statistics = (functionN, "ML_statistics");

val task_statistics = (functionN, "task_statistics");

fun loading_theory name = [("function", "loading_theory"), ("name", name)];

fun dest_loading_theory [("function", "loading_theory"), ("name", name)] = SOME name
  | dest_loading_theory _ = NONE;



(** print mode operations **)

val no_output = ("", "");
fun default_output (_: T) = no_output;

local
  val default = {output = default_output};
  val modes = Synchronized.var "Markup.modes" (Symtab.make [("", default)]);
in
  fun add_mode name output =
    Synchronized.change modes (fn tab =>
      (if not (Symtab.defined tab name) then ()
       else warning ("Redefining markup mode " ^ quote name);
       Symtab.update (name, {output = output}) tab));
  fun get_mode () =
    the_default default
      (Library.get_first (Symtab.lookup (Synchronized.value modes)) (print_mode_value ()));
end;

fun output m = if is_empty m then no_output else #output (get_mode ()) m;

val enclose = output #-> Library.enclose;

fun markup m =
  let val (bg, en) = output m
  in Library.enclose (Output.escape bg) (Output.escape en) end;

fun markup_only m = markup m "";

end;

fun legacy_feature s = warning (Markup.markup Markup.legacy ("Legacy feature! " ^ s));

