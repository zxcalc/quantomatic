(*  Title:      Pure/Thy/present.ML
    Author:     Markus Wenzel and Stefan Berghofer, TU Muenchen

Theory presentation: HTML, graph files, (PDF)LaTeX documents.
*)

signature PRESENT =
sig
  val session_name: theory -> string
  val document_enabled: string -> bool
  val document_variants: string -> (string * string) list
  val init: bool -> bool -> Path.T -> string -> bool -> string -> (string * string) list ->
    (Path.T * Path.T) list -> string * string -> bool -> theory list -> unit  (*not thread-safe!*)
  val finish: unit -> unit  (*not thread-safe!*)
  val theory_output: string -> string -> unit
  val begin_theory: int -> (unit -> HTML.text) -> theory -> theory
  val display_drafts: Path.T list -> int
end;

structure Present: PRESENT =
struct


(** paths **)

val tex_ext = Path.ext "tex";
val tex_path = tex_ext o Path.basic;
val html_ext = Path.ext "html";
val html_path = html_ext o Path.basic;
val index_path = Path.basic "index.html";
val readme_html_path = Path.basic "README.html";
val documentN = "document";
val document_path = Path.basic documentN;
val doc_indexN = "session";
val graph_path = Path.basic "session.graph";
val graph_pdf_path = Path.basic "session_graph.pdf";
val graph_eps_path = Path.basic "session_graph.eps";

fun show_path path = Path.implode (Path.expand (Path.append (File.pwd ()) path));



(** additional theory data **)

structure Browser_Info = Theory_Data
(
  type T = {chapter: string, name: string};
  val empty = {chapter = "Unsorted", name = "Unknown"}: T;
  fun extend _ = empty;
  fun merge _ = empty;
);

val _ = Theory.setup
  (Browser_Info.put {chapter = Context.PureN, name = Context.PureN});

val session_name = #name o Browser_Info.get;
val session_chapter_name = (fn {chapter, name} => [chapter, name]) o Browser_Info.get;



(** graphs **)

fun ID_of sess s = space_implode "/" (sess @ [s]);
fun ID_of_thy thy = ID_of (session_chapter_name thy) (Context.theory_name thy);

fun theory_link (curr_chapter, curr_session) thy =
  let
    val {chapter, name = session} = Browser_Info.get thy;
    val link = html_path (Context.theory_name thy);
  in
    if curr_session = session then SOME link
    else if curr_chapter = chapter then
      SOME (Path.appends [Path.parent, Path.basic session, link])
    else if chapter = Context.PureN then NONE
    else SOME (Path.appends [Path.parent, Path.parent, Path.basic chapter, Path.basic session, link])
  end;

(*retrieve graph data from initial collection of theories*)
fun init_graph (curr_chapter, curr_session) = rev o map (fn thy =>
  let
    val {chapter, name = session_name} = Browser_Info.get thy;
    val thy_name = Context.theory_name thy;
    val path =
      (case theory_link (curr_chapter, curr_session) thy of
        NONE => ""
      | SOME p => Path.implode p);
    val entry =
     {name = thy_name,
      ID = ID_of [chapter, session_name] thy_name,
      dir = session_name,
      path = path,
      unfold = false,
      parents = map ID_of_thy (Theory.parents_of thy),
      content = []};
  in (0, entry) end);

fun ins_graph_entry (i, entry as {ID, ...}) (gr: (int * Graph_Display.node) list) =
  (i, entry) :: filter_out (fn (_, entry') => #ID entry' = ID) gr;



(** global browser info state **)

(* type theory_info *)

type theory_info = {tex_source: string, html_source: string};

fun make_theory_info (tex_source, html_source) =
  {tex_source = tex_source, html_source = html_source}: theory_info;

fun map_theory_info f {tex_source, html_source} =
  make_theory_info (f (tex_source, html_source));


(* type browser_info *)

type browser_info =
 {theories: theory_info Symtab.table,
  tex_index: (int * string) list,
  html_index: (int * string) list,
  graph: (int * Graph_Display.node) list};

fun make_browser_info (theories, tex_index, html_index, graph) : browser_info =
  {theories = theories, tex_index = tex_index, html_index = html_index, graph = graph};

val empty_browser_info = make_browser_info (Symtab.empty, [], [], []);

fun init_browser_info session thys =
  make_browser_info (Symtab.empty, [], [], init_graph session thys);

fun map_browser_info f {theories, tex_index, html_index, graph} =
  make_browser_info (f (theories, tex_index, html_index, graph));


(* state *)

val browser_info = Unsynchronized.ref empty_browser_info;
fun change_browser_info f =
  CRITICAL (fn () => Unsynchronized.change browser_info (map_browser_info f));

fun init_theory_info name info =
  change_browser_info (fn (theories, tex_index, html_index, graph) =>
    (Symtab.update (name, info) theories, tex_index, html_index, graph));

fun change_theory_info name f =
  change_browser_info (fn (theories, tex_index, html_index, graph) =>
    (case Symtab.lookup theories name of
      NONE => error ("Browser info: cannot access theory document " ^ quote name)
    | SOME info =>
        (Symtab.update (name, map_theory_info f info) theories, tex_index, html_index, graph)));


fun add_tex_index txt =
  change_browser_info (fn (theories, tex_index, html_index, graph) =>
    (theories, txt :: tex_index, html_index, graph));

fun add_html_index txt =
  change_browser_info (fn (theories, tex_index, html_index, graph) =>
    (theories, tex_index, txt :: html_index, graph));

fun add_graph_entry entry =
  change_browser_info (fn (theories, tex_index, html_index, graph) =>
    (theories, tex_index, html_index, ins_graph_entry entry graph));



(** global session state **)

(* session_info *)

type session_info =
  {name: string, chapter: string, info_path: Path.T, info: bool,
    doc_format: string, doc_graph: bool, doc_output: Path.T option,
    doc_files: (Path.T * Path.T) list, documents: (string * string) list,
    verbose: bool, readme: Path.T option};

fun make_session_info
  (name, chapter, info_path, info, doc_format, doc_graph, doc_output,
    doc_files, documents, verbose, readme) =
  {name = name, chapter = chapter, info_path = info_path, info = info,
    doc_format = doc_format, doc_graph = doc_graph, doc_output = doc_output,
    doc_files = doc_files, documents = documents, verbose = verbose,
    readme = readme}: session_info;


(* state *)

val session_info = Unsynchronized.ref (NONE: session_info option);

fun with_session_info x f = (case ! session_info of NONE => x | SOME info => f info);



(** document preparation **)

(* options *)

fun document_enabled s = s <> "" andalso s <> "false";

fun document_variants str =
  let
    fun read_variant s =
      (case space_explode "=" s of
        [name] => (name, "")
      | [name, tags] => (name, tags)
      | _ => error ("Malformed document variant specification: " ^ quote s));
    val variants = map read_variant (space_explode ":" str);
    val _ =
      (case duplicates (op =) (map #1 variants) of
        [] => ()
      | dups => error ("Duplicate document variants: " ^ commas_quote dups));
  in variants end;


(* init session *)

fun init build info info_path doc doc_graph document_output doc_variants doc_files
    (chapter, name) verbose thys =
  if not build andalso not info andalso doc = "" then
    (browser_info := empty_browser_info; session_info := NONE)
  else
    let
      val doc_output =
        if document_output = "" then NONE else SOME (Path.explode document_output);

      val documents =
        if doc = "" then []
        else if null doc_files andalso not (can File.check_dir document_path) then
          (if verbose then Output.physical_stderr "Warning: missing document directory\n"
           else (); [])
        else doc_variants;

      val readme = if File.exists readme_html_path then SOME readme_html_path else NONE;

      val docs =
        (case readme of NONE => [] | SOME p => [(Url.File p, "README")]) @
          map (fn (name, _) => (Url.File (Path.ext doc (Path.basic name)), name)) documents;
    in
      session_info :=
        SOME (make_session_info (name, chapter, info_path, info, doc,
          doc_graph, doc_output, doc_files, documents, verbose, readme));
      browser_info := init_browser_info (chapter, name) thys;
      add_html_index (0, HTML.begin_session_index name docs (Url.explode "medium.html"))
    end;


(* isabelle tool wrappers *)

fun isabelle_document {verbose, purge} format name tags dir =
  let
    val s = "\"$ISABELLE_TOOL\" document" ^ (if purge then " -c" else "") ^ " -o '" ^ format ^ "' \
      \-n '" ^ name ^ "' -t '" ^ tags ^ "' " ^ File.shell_path dir ^ " 2>&1";
    val doc_path = Path.appends [dir, Path.parent, Path.basic name |> Path.ext format];
    val _ = if verbose then writeln s else ();
    val (out, rc) = Isabelle_System.bash_output s;
    val _ =
      if not (File.exists doc_path) orelse rc <> 0 then
        cat_error out ("Failed to build document " ^ quote (show_path doc_path))
      else if verbose then writeln out
      else ();
  in doc_path end;

fun isabelle_browser graph = Isabelle_System.with_tmp_dir "browser" (fn dir =>
  let
    val pdf_path = Path.append dir graph_pdf_path;
    val eps_path = Path.append dir graph_eps_path;
    val graph_path = Path.append dir graph_path;
    val _ = Graph_Display.write_graph_browser graph_path graph;
    val args = "-o " ^ File.shell_path pdf_path ^ " " ^ File.shell_path graph_path;
  in
    if Isabelle_System.isabelle_tool "browser" args = 0 andalso
      File.exists pdf_path andalso File.exists eps_path
    then (File.read pdf_path, File.read eps_path)
    else error "Failed to prepare dependency graph"
  end);


(* finish session -- output all generated text *)

fun sorted_index index = map snd (sort (int_ord o pairself fst) (rev index));
fun index_buffer index = Buffer.add (implode (sorted_index index)) Buffer.empty;

fun write_tex src name path =
  File.write_buffer (Path.append path (tex_path name)) src;

fun write_tex_index tex_index path =
  write_tex (index_buffer tex_index |> Buffer.add Latex.tex_trailer) doc_indexN path;

fun finish () =
  with_session_info () (fn {name, chapter, info, info_path, doc_format, doc_graph,
    doc_output, doc_files, documents, verbose, readme, ...} =>
  let
    val {theories, tex_index, html_index, graph} = ! browser_info;
    val thys = Symtab.dest theories;

    val chapter_prefix = Path.append info_path (Path.basic chapter);
    val session_prefix = Path.append chapter_prefix (Path.basic name);

    fun finish_html (a, {html_source, ...}: theory_info) =
      File.write (Path.append session_prefix (html_path a)) html_source;

    val sorted_graph = sorted_index graph;
    val opt_graphs =
      if doc_graph andalso not (null documents) then
        SOME (isabelle_browser sorted_graph)
      else NONE;

    val _ =
      if info then
       (Isabelle_System.mkdirs session_prefix;
        File.write_buffer (Path.append session_prefix index_path)
          (index_buffer html_index |> Buffer.add HTML.end_document);
        (case readme of NONE => () | SOME path => Isabelle_System.copy_file path session_prefix);
        Graph_Display.write_graph_browser (Path.append session_prefix graph_path) sorted_graph;
        Isabelle_System.isabelle_tool "browser" "-b";
        Isabelle_System.copy_file (Path.explode "~~/lib/browser/GraphBrowser.jar") session_prefix;
        List.app (fn (a, txt) => File.write (Path.append session_prefix (Path.basic a)) txt)
          (HTML.applet_pages name (Url.File index_path, name));
        Isabelle_System.copy_file (Path.explode "~~/etc/isabelle.css") session_prefix;
        List.app finish_html thys;
        if verbose
        then Output.physical_stderr ("Browser info at " ^ show_path session_prefix ^ "\n")
        else ())
      else ();

    fun document_job doc_prefix backdrop (doc_name, tags) =
      let
        val doc_dir = Path.append doc_prefix (Path.basic doc_name);
        val _ = Isabelle_System.mkdirs doc_dir;
        val _ =
          Isabelle_System.isabelle_tool "latex"
            ("-o sty " ^ File.shell_path (Path.append doc_dir (Path.basic "root.tex")));
        val _ =
          if null doc_files then Isabelle_System.copy_dir document_path doc_dir
          else List.app (fn file => Isabelle_System.copy_file_base file doc_dir) doc_files;
        val _ =
          (case opt_graphs of
            NONE => ()
          | SOME (pdf, eps) =>
              (File.write (Path.append doc_dir graph_pdf_path) pdf;
                File.write (Path.append doc_dir graph_eps_path) eps));
        val _ = write_tex_index tex_index doc_dir;
        val _ =
          List.app (fn (a, {tex_source, ...}) =>
            write_tex (Buffer.add tex_source Buffer.empty) a doc_dir) thys;
      in
        fn () =>
          (isabelle_document {verbose = true, purge = backdrop} doc_format doc_name tags doc_dir,
            fn doc =>
              if verbose orelse not backdrop then
                Output.physical_stderr ("Document at " ^ show_path doc ^ "\n")
              else ())
      end;

    val jobs =
      (if info orelse is_none doc_output then
        map (document_job session_prefix true) documents
       else []) @
      (case doc_output of
        NONE => []
      | SOME path => map (document_job path false) documents);

    val _ =
      if not (null jobs) andalso null doc_files then
        Output.physical_stderr ("### Legacy feature! Document preparation for session " ^ quote name ^
          " without 'document_files'\n")
      else ();

    val _ = jobs |> Par_List.map (fn job => job ()) |> List.app (op |>);
  in
    browser_info := empty_browser_info;
    session_info := NONE
  end);


(* theory elements *)

fun theory_output name s =
  with_session_info () (fn _ =>
    change_theory_info name (fn (_, html_source) => (Latex.isabelle_file name s, html_source)));

fun begin_theory update_time mk_text thy =
  with_session_info thy (fn {name = session_name, chapter, ...} =>
    let
      val name = Context.theory_name thy;
      val parents = Theory.parents_of thy;

      val parent_specs = parents |> map (fn parent =>
        (Option.map Url.File (theory_link (chapter, session_name) parent),
          (Context.theory_name parent)));
      val html_source = HTML.theory name parent_specs (mk_text ());

      val graph_entry =
       {name = name,
        ID = ID_of [chapter, session_name] name,
        dir = session_name,
        unfold = true,
        path = Path.implode (html_path name),
        parents = map ID_of_thy parents,
        content = []};
    in
      init_theory_info name (make_theory_info ("", html_source));
      add_graph_entry (update_time, graph_entry);
      add_html_index (update_time, HTML.theory_entry (Url.File (html_path name), name));
      add_tex_index (update_time, Latex.theory_entry name);
      Browser_Info.put {chapter = chapter, name = session_name} thy
    end);



(** draft document output **)

fun display_drafts src_paths = Isabelle_System.with_tmp_dir "drafts" (fn dir =>
  let
    fun prep_draft path i =
      let
        val base = Path.base path;
        val name =
          (case Path.implode (#1 (Path.split_ext base)) of
            "" => "DUMMY"
          | s => s)  ^ serial_string ();
      in
        if File.exists path then
          (((name, base, File.read path), (i, Latex.theory_entry name)), i + 1)
        else error ("Bad file: " ^ Path.print path)
      end;
    val (srcs, tex_index) = split_list (fst (fold_map prep_draft src_paths 0));

    val doc_path = Path.append dir document_path;
    val _ = Isabelle_System.mkdirs doc_path;
    val root_path = Path.append doc_path (Path.basic "root.tex");
    val _ = Isabelle_System.copy_file (Path.explode "~~/lib/texinputs/draft.tex") root_path;
    val _ = Isabelle_System.isabelle_tool "latex" ("-o sty " ^ File.shell_path root_path);
    val _ = Isabelle_System.isabelle_tool "latex" ("-o syms " ^ File.shell_path root_path);

    fun known name =
      let val ss = split_lines (File.read (Path.append doc_path (Path.basic name)))
      in member (op =) ss end;
    val known_syms = known "syms.lst";
    val known_ctrls = known "ctrls.lst";

    val _ = srcs |> List.app (fn (name, base, txt) =>
      Symbol.explode txt
      |> Latex.symbol_source (known_syms, known_ctrls) (Path.implode base)
      |> File.write (Path.append doc_path (tex_path name)));
    val _ = write_tex_index tex_index doc_path;

    val result =
      isabelle_document {verbose = false, purge = true} "pdf" documentN "" doc_path;

    val target_dir = Path.explode "$ISABELLE_HOME_USER/tmp";
    val target = Path.explode "$ISABELLE_HOME_USER/tmp/drafts.pdf"
    val _ = Isabelle_System.mkdirs target_dir;
    val _ = Isabelle_System.copy_file result target;
  in
    Isabelle_System.isabelle_tool "display" (File.shell_path target ^ " &")
  end);

end;

