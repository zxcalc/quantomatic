(*  Title:      Pure/PIDE/resources.ML
    Author:     Makarius

Resources for theories and auxiliary files.
*)

signature RESOURCES =
sig
  val master_directory: theory -> Path.T
  val imports_of: theory -> (string * Position.T) list
  val thy_path: Path.T -> Path.T
  val check_thy: Path.T -> string ->
   {master: Path.T * SHA1.digest, text: string, theory_pos: Position.T,
    imports: (string * Position.T) list, keywords: Thy_Header.keywords}
  val parse_files: string -> (theory -> Token.file list) parser
  val provide: Path.T * SHA1.digest -> theory -> theory
  val provide_parse_files: string -> (theory -> Token.file list * theory) parser
  val loaded_files_current: theory -> bool
  val begin_theory: Path.T -> Thy_Header.header -> theory list -> theory
  val load_thy: bool -> (Toplevel.transition -> Time.time option) -> int -> Path.T ->
    Thy_Header.header -> Position.T -> string -> theory list -> theory * (unit -> unit) * int
end;

structure Resources: RESOURCES =
struct

(* manage source files *)

type files =
 {master_dir: Path.T,  (*master directory of theory source*)
  imports: (string * Position.T) list,  (*source specification of imports*)
  provided: (Path.T * SHA1.digest) list};  (*source path, digest*)

fun make_files (master_dir, imports, provided): files =
 {master_dir = master_dir, imports = imports, provided = provided};

structure Files = Theory_Data
(
  type T = files;
  val empty = make_files (Path.current, [], []);
  fun extend _ = empty;
  fun merge _ = empty;
);

fun map_files f =
  Files.map (fn {master_dir, imports, provided} =>
    make_files (f (master_dir, imports, provided)));


val master_directory = #master_dir o Files.get;
val imports_of = #imports o Files.get;

fun put_deps master_dir imports = map_files (fn _ => (master_dir, imports, []));


(* theory files *)

val thy_path = Path.ext "thy";

fun check_file dir file = File.check_file (File.full_path dir file);

fun check_thy dir thy_name =
  let
    val path = thy_path (Path.basic thy_name);
    val master_file = check_file dir path;
    val text = File.read master_file;

    val {name = (name, pos), imports, keywords} =
      Thy_Header.read (Path.position master_file) text;
    val _ = thy_name <> name andalso
      error ("Bad theory name " ^ quote name ^
        " for file " ^ Path.print path ^ Position.here pos);
  in
   {master = (master_file, SHA1.digest text), text = text, theory_pos = pos,
    imports = imports, keywords = keywords}
  end;


(* load files *)

fun parse_files cmd =
  Scan.ahead Parse.not_eof -- Parse.path >> (fn (tok, name) => fn thy =>
    (case Token.get_files tok of
      [] =>
        let
          val keywords = Thy_Header.get_keywords thy;
          val master_dir = master_directory thy;
          val pos = Token.pos_of tok;
          val src_paths = Keyword.command_files keywords cmd (Path.explode name);
        in map (Command.read_file master_dir pos) src_paths end
    | files => map Exn.release files));

fun provide (src_path, id) =
  map_files (fn (master_dir, imports, provided) =>
    if AList.defined (op =) provided src_path then
      error ("Duplicate use of source file: " ^ Path.print src_path)
    else (master_dir, imports, (src_path, id) :: provided));

fun provide_parse_files cmd =
  parse_files cmd >> (fn files => fn thy =>
    let
      val fs = files thy;
      val thy' = fold (fn {src_path, digest, ...} => provide (src_path, digest)) fs thy;
    in (fs, thy') end);

fun load_file thy src_path =
  let
    val full_path = check_file (master_directory thy) src_path;
    val text = File.read full_path;
    val id = SHA1.digest text;
  in ((full_path, id), text) end;

fun loaded_files_current thy =
  #provided (Files.get thy) |>
    forall (fn (src_path, id) =>
      (case try (load_file thy) src_path of
        NONE => false
      | SOME ((_, id'), _) => id = id'));


(* load theory *)

fun begin_theory master_dir {name, imports, keywords} parents =
  Theory.begin_theory name parents
  |> put_deps master_dir imports
  |> Thy_Header.add_keywords keywords;

fun excursion keywords master_dir last_timing init elements =
  let
    fun prepare_span st span =
      Command_Span.content span
      |> Command.read keywords (Command.read_thy st) master_dir init ([], ~1)
      |> (fn tr => Toplevel.put_timing (last_timing tr) tr);

    fun element_result span_elem (st, _) =
      let
        val elem = Thy_Syntax.map_element (prepare_span st) span_elem;
        val (results, st') = Toplevel.element_result keywords elem st;
        val pos' = Toplevel.pos_of (Thy_Syntax.last_element elem);
      in (results, (st', pos')) end;

    val (results, (end_state, end_pos)) =
      fold_map element_result elements (Toplevel.toplevel, Position.none);

    val thy = Toplevel.end_theory end_pos end_state;
  in (results, thy) end;

fun load_thy document last_timing update_time master_dir header text_pos text parents =
  let
    val (name, _) = #name header;
    val keywords =
      fold (curry Keyword.merge_keywords o Thy_Header.get_keywords) parents
        (Keyword.add_keywords (#keywords header) Keyword.empty_keywords);

    val toks = Token.explode keywords text_pos text;
    val spans = Outer_Syntax.parse_spans toks;
    val elements = Thy_Syntax.parse_elements keywords spans;

    fun init () =
      begin_theory master_dir header parents
      |> Present.begin_theory update_time
          (fn () => HTML.html_mode (implode o map (Thy_Syntax.present_span keywords)) spans);

    val (results, thy) =
      cond_timeit true ("theory " ^ quote name)
        (fn () => excursion keywords master_dir last_timing init elements);

    fun present () =
      let
        val res = filter_out (Toplevel.is_ignored o #1) (maps Toplevel.join_results results);
      in
        if exists (Toplevel.is_skipped_proof o #2) res then
          warning ("Cannot present theory with skipped proofs: " ^ quote name)
        else
          let val tex_source = Thy_Output.present_thy thy res toks |> Buffer.content;
          in if document then Present.theory_output name tex_source else () end
      end;

  in (thy, present, size text) end;


(* antiquotations *)

local

fun check_path strict ctxt dir (name, pos) =
  let
    val _ = Context_Position.report ctxt pos Markup.language_path;

    val path = Path.append dir (Path.explode name)
      handle ERROR msg => error (msg ^ Position.here pos);

    val _ = Context_Position.report ctxt pos (Markup.path (Path.smart_implode path));
    val _ =
      if can Path.expand path andalso File.exists path then ()
      else
        let
          val path' = perhaps (try Path.expand) path;
          val msg = "Bad file: " ^ Path.print path' ^ Position.here pos;
        in
          if strict then error msg
          else if Context_Position.is_visible ctxt then
            Output.report
              [Markup.markup (Markup.bad |> Markup.properties (Position.properties_of pos)) msg]
          else ()
        end;
  in path end;

fun file_antiq strict ctxt (name, pos) =
  let
    val dir = master_directory (Proof_Context.theory_of ctxt);
    val _ = check_path strict ctxt dir (name, pos);
  in
    space_explode "/" name
    |> map Latex.output_ascii
    |> space_implode (Latex.output_ascii "/" ^ "\\discretionary{}{}{}")
    |> enclose "\\isatt{" "}"
  end;

in

val _ = Theory.setup
 (Thy_Output.antiquotation @{binding file} (Scan.lift (Parse.position Parse.path))
    (file_antiq true o #context) #>
  Thy_Output.antiquotation @{binding file_unchecked} (Scan.lift (Parse.position Parse.path))
    (file_antiq false o #context) #>
  ML_Antiquotation.value @{binding path}
    (Args.context -- Scan.lift (Parse.position Parse.path) >> (fn (ctxt, arg) =>
      let val path = check_path true ctxt Path.current arg
      in "Path.explode " ^ ML_Syntax.print_string (Path.implode path) end)));

end;

end;
