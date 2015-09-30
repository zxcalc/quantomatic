/*  Title:      Pure/Tools/build.scala
    Author:     Makarius
    Options:    :folding=explicit:

Build and manage Isabelle sessions.
*/

package isabelle


import java.io.{BufferedInputStream, FileInputStream,
  BufferedReader, InputStreamReader, IOException}
import java.util.zip.GZIPInputStream

import scala.collection.SortedSet
import scala.collection.mutable
import scala.annotation.tailrec


object Build
{
  /** progress context **/

  class Progress
  {
    def echo(msg: String) {}
    def theory(session: String, theory: String) {}
    def stopped: Boolean = false
    override def toString: String = if (stopped) "Progress(stopped)" else "Progress"
  }

  object Ignore_Progress extends Progress

  class Console_Progress(verbose: Boolean = false) extends Progress
  {
    override def echo(msg: String) { Console.println(msg) }
    override def theory(session: String, theory: String): Unit =
      if (verbose) echo(session + ": theory " + theory)

    @volatile private var is_stopped = false
    def interrupt_handler[A](e: => A): A = POSIX_Interrupt.handler { is_stopped = true } { e }
    override def stopped: Boolean =
    {
      if (Thread.interrupted) is_stopped = true
      is_stopped
    }
  }



  /** session information **/

  // external version
  abstract class Entry
  sealed case class Chapter(name: String) extends Entry
  sealed case class Session_Entry(
    pos: Position.T,
    name: String,
    groups: List[String],
    path: String,
    parent: Option[String],
    description: String,
    options: List[Options.Spec],
    theories: List[(Boolean, List[Options.Spec], List[String])],
    files: List[String],
    document_files: List[(String, String)]) extends Entry

  // internal version
  sealed case class Session_Info(
    chapter: String,
    select: Boolean,
    pos: Position.T,
    groups: List[String],
    dir: Path,
    parent: Option[String],
    description: String,
    options: Options,
    theories: List[(Boolean, Options, List[Path])],
    files: List[Path],
    document_files: List[(Path, Path)],
    entry_digest: SHA1.Digest)

  def is_pure(name: String): Boolean = name == "RAW" || name == "Pure"

  def session_info(options: Options, select: Boolean, dir: Path,
      chapter: String, entry: Session_Entry): (String, Session_Info) =
    try {
      val name = entry.name

      if (name == "") error("Bad session name")
      if (is_pure(name) && entry.parent.isDefined) error("Illegal parent session")
      if (!is_pure(name) && !entry.parent.isDefined) error("Missing parent session")

      val session_options = options ++ entry.options

      val theories =
        entry.theories.map({ case (global, opts, thys) =>
          (global, session_options ++ opts, thys.map(Path.explode(_))) })
      val files = entry.files.map(Path.explode(_))
      val document_files =
        entry.document_files.map({ case (s1, s2) => (Path.explode(s1), Path.explode(s2)) })

      val entry_digest =
        SHA1.digest((chapter, name, entry.parent, entry.options,
          entry.theories, entry.files, entry.document_files).toString)

      val info =
        Session_Info(chapter, select, entry.pos, entry.groups, dir + Path.explode(entry.path),
          entry.parent, entry.description, session_options, theories, files,
          document_files, entry_digest)

      (name, info)
    }
    catch {
      case ERROR(msg) =>
        error(msg + "\nThe error(s) above occurred in session entry " +
          quote(entry.name) + Position.here(entry.pos))
    }


  /* session tree */

  object Session_Tree
  {
    def apply(infos: Seq[(String, Session_Info)]): Session_Tree =
    {
      val graph1 =
        (Graph.string[Session_Info] /: infos) {
          case (graph, (name, info)) =>
            if (graph.defined(name))
              error("Duplicate session " + quote(name) + Position.here(info.pos) +
                Position.here(graph.get_node(name).pos))
            else graph.new_node(name, info)
        }
      val graph2 =
        (graph1 /: graph1.iterator) {
          case (graph, (name, (info, _))) =>
            info.parent match {
              case None => graph
              case Some(parent) =>
                if (!graph.defined(parent))
                  error("Bad parent session " + quote(parent) + " for " +
                    quote(name) + Position.here(info.pos))

                try { graph.add_edge_acyclic(parent, name) }
                catch {
                  case exn: Graph.Cycles[_] =>
                    error(cat_lines(exn.cycles.map(cycle =>
                      "Cyclic session dependency of " +
                        cycle.map(c => quote(c.toString)).mkString(" via "))) +
                          Position.here(info.pos))
                }
            }
        }
      new Session_Tree(graph2)
    }
  }

  final class Session_Tree private(val graph: Graph[String, Session_Info])
    extends PartialFunction[String, Session_Info]
  {
    def apply(name: String): Session_Info = graph.get_node(name)
    def isDefinedAt(name: String): Boolean = graph.defined(name)

    def selection(
      requirements: Boolean = false,
      all_sessions: Boolean = false,
      exclude_session_groups: List[String] = Nil,
      exclude_sessions: List[String] = Nil,
      session_groups: List[String] = Nil,
      sessions: List[String] = Nil): (List[String], Session_Tree) =
    {
      val bad_sessions =
        SortedSet((exclude_sessions ::: sessions).filterNot(isDefinedAt(_)): _*).toList
      if (bad_sessions.nonEmpty) error("Undefined session(s): " + commas_quote(bad_sessions))

      val excluded =
      {
        val exclude_group = exclude_session_groups.toSet
        val exclude_group_sessions =
          (for {
            (name, (info, _)) <- graph.iterator
            if apply(name).groups.exists(exclude_group)
          } yield name).toList
        graph.all_succs(exclude_group_sessions ::: exclude_sessions).toSet
      }

      val pre_selected =
      {
        if (all_sessions) graph.keys
        else {
          val select_group = session_groups.toSet
          val select = sessions.toSet
          (for {
            (name, (info, _)) <- graph.iterator
            if info.select || select(name) || apply(name).groups.exists(select_group)
          } yield name).toList
        }
      }.filterNot(excluded)

      val selected =
        if (requirements) (graph.all_preds(pre_selected).toSet -- pre_selected).toList
        else pre_selected

      val graph1 = graph.restrict(graph.all_preds(selected).toSet)
      (selected, new Session_Tree(graph1))
    }

    def topological_order: List[(String, Session_Info)] =
      graph.topological_order.map(name => (name, apply(name)))

    override def toString: String = graph.keys_iterator.mkString("Session_Tree(", ", ", ")")
  }


  /* parser */

  val chapter_default = "Unsorted"

  private val CHAPTER = "chapter"
  private val SESSION = "session"
  private val IN = "in"
  private val DESCRIPTION = "description"
  private val OPTIONS = "options"
  private val GLOBAL_THEORIES = "global_theories"
  private val THEORIES = "theories"
  private val FILES = "files"
  private val DOCUMENT_FILES = "document_files"

  lazy val root_syntax =
    Outer_Syntax.init() + "(" + ")" + "+" + "," + "=" + "[" + "]" +
      (CHAPTER, Keyword.THY_DECL) + (SESSION, Keyword.THY_DECL) +
      IN + DESCRIPTION + OPTIONS + GLOBAL_THEORIES + THEORIES + FILES + DOCUMENT_FILES

  object Parser extends Parse.Parser
  {
    private val chapter: Parser[Chapter] =
    {
      val chapter_name = atom("chapter name", _.is_name)

      command(CHAPTER) ~! chapter_name ^^ { case _ ~ a => Chapter(a) }
    }

    private val session_entry: Parser[Session_Entry] =
    {
      val session_name = atom("session name", _.is_name)

      val option =
        name ~ opt($$$("=") ~! name ^^ { case _ ~ x => x }) ^^ { case x ~ y => (x, y) }
      val options = $$$("[") ~> rep1sep(option, $$$(",")) <~ $$$("]")

      val theories =
        ($$$(GLOBAL_THEORIES) | $$$(THEORIES)) ~!
          ((options | success(Nil)) ~ rep(theory_xname)) ^^
          { case x ~ (y ~ z) => (x == GLOBAL_THEORIES, y, z) }

      val document_files =
        $$$(DOCUMENT_FILES) ~!
          (($$$("(") ~! ($$$(IN) ~! (path ~ $$$(")"))) ^^
              { case _ ~ (_ ~ (x ~ _)) => x } | success("document")) ~
            rep1(path)) ^^ { case _ ~ (x ~ y) => y.map((x, _)) }

      command(SESSION) ~!
        (position(session_name) ~
          (($$$("(") ~! (rep1(name) <~ $$$(")")) ^^ { case _ ~ x => x }) | success(Nil)) ~
          (($$$(IN) ~! path ^^ { case _ ~ x => x }) | success(".")) ~
          ($$$("=") ~!
            (opt(session_name ~! $$$("+") ^^ { case x ~ _ => x }) ~
              (($$$(DESCRIPTION) ~! text ^^ { case _ ~ x => x }) | success("")) ~
              (($$$(OPTIONS) ~! options ^^ { case _ ~ x => x }) | success(Nil)) ~
              rep1(theories) ~
              (($$$(FILES) ~! rep1(path) ^^ { case _ ~ x => x }) | success(Nil)) ~
              (rep(document_files) ^^ (x => x.flatten))))) ^^
        { case _ ~ ((a, pos) ~ b ~ c ~ (_ ~ (d ~ e ~ f ~ g ~ h ~ i))) =>
            Session_Entry(pos, a, b, c, d, e, f, g, h, i) }
    }

    def parse_entries(root: Path): List[(String, Session_Entry)] =
    {
      val toks = Token.explode(root_syntax.keywords, File.read(root))
      val start = Token.Pos.file(root.implode)

      parse_all(rep(chapter | session_entry), Token.reader(toks, start)) match {
        case Success(result, _) =>
          var chapter = chapter_default
          val entries = new mutable.ListBuffer[(String, Session_Entry)]
          result.foreach {
            case Chapter(name) => chapter = name
            case session_entry: Session_Entry => entries += ((chapter, session_entry))
          }
          entries.toList
        case bad => error(bad.toString)
      }
    }
  }


  /* find sessions within certain directories */

  private val ROOT = Path.explode("ROOT")
  private val ROOTS = Path.explode("ROOTS")

  private def is_session_dir(dir: Path): Boolean =
    (dir + ROOT).is_file || (dir + ROOTS).is_file

  private def check_session_dir(dir: Path): Path =
    if (is_session_dir(dir)) dir
    else error("Bad session root directory: " + dir.toString)

  def find_sessions(options: Options, dirs: List[Path] = Nil, select_dirs: List[Path] = Nil)
    : Session_Tree =
  {
    def find_dir(select: Boolean, dir: Path): List[(String, Session_Info)] =
      find_root(select, dir) ::: find_roots(select, dir)

    def find_root(select: Boolean, dir: Path): List[(String, Session_Info)] =
    {
      val root = dir + ROOT
      if (root.is_file)
        Parser.parse_entries(root).map(p => session_info(options, select, dir, p._1, p._2))
      else Nil
    }

    def find_roots(select: Boolean, dir: Path): List[(String, Session_Info)] =
    {
      val roots = dir + ROOTS
      if (roots.is_file) {
        for {
          line <- split_lines(File.read(roots))
          if !(line == "" || line.startsWith("#"))
          dir1 =
            try { check_session_dir(dir + Path.explode(line)) }
            catch {
              case ERROR(msg) =>
                error(msg + "\nThe error(s) above occurred in session catalog " + roots.toString)
            }
          info <- find_dir(select, dir1)
        } yield info
      }
      else Nil
    }

    val default_dirs = Isabelle_System.components().filter(is_session_dir(_))
    dirs.foreach(check_session_dir(_))
    select_dirs.foreach(check_session_dir(_))

    Session_Tree(
      for {
        (select, dir) <- (default_dirs ::: dirs).map((false, _)) ::: select_dirs.map((true, _))
        info <- find_dir(select, dir)
      } yield info)
  }



  /** build **/

  /* queue */

  object Queue
  {
    def apply(tree: Session_Tree, load_timings: String => (List[Properties.T], Double)): Queue =
    {
      val graph = tree.graph
      val sessions = graph.keys

      val timings = Par_List.map((name: String) => (name, load_timings(name)), sessions)
      val command_timings =
        Map(timings.map({ case (name, (ts, _)) => (name, ts) }): _*).withDefaultValue(Nil)
      val session_timing =
        Map(timings.map({ case (name, (_, t)) => (name, t) }): _*).withDefaultValue(0.0)

      def outdegree(name: String): Int = graph.imm_succs(name).size
      def timeout(name: String): Double = tree(name).options.real("timeout")

      object Ordering extends scala.math.Ordering[String]
      {
        def compare_timing(name1: String, name2: String): Int =
        {
          val t1 = session_timing(name1)
          val t2 = session_timing(name2)
          if (t1 == 0.0 || t2 == 0.0) 0
          else t1 compare t2
        }

        def compare(name1: String, name2: String): Int =
          outdegree(name2) compare outdegree(name1) match {
            case 0 =>
              compare_timing(name2, name1) match {
                case 0 =>
                  timeout(name2) compare timeout(name1) match {
                    case 0 => name1 compare name2
                    case ord => ord
                  }
                case ord => ord
              }
            case ord => ord
          }
      }

      new Queue(graph, SortedSet(sessions: _*)(Ordering), command_timings)
    }
  }

  final class Queue private(
    graph: Graph[String, Session_Info],
    order: SortedSet[String],
    val command_timings: String => List[Properties.T])
  {
    def is_inner(name: String): Boolean = !graph.is_maximal(name)

    def is_empty: Boolean = graph.is_empty

    def - (name: String): Queue =
      new Queue(graph.del_node(name),
        order - name,  // FIXME scala-2.10.0 TreeSet problem!?
        command_timings)

    def dequeue(skip: String => Boolean): Option[(String, Session_Info)] =
    {
      val it = order.iterator.dropWhile(name =>
        skip(name)
          || !graph.defined(name)  // FIXME scala-2.10.0 TreeSet problem!?
          || !graph.is_minimal(name))
      if (it.hasNext) { val name = it.next; Some((name, graph.get_node(name))) }
      else None
    }
  }


  /* source dependencies and static content */

  sealed case class Session_Content(
    loaded_theories: Set[String],
    known_theories: Map[String, Document.Node.Name],
    keywords: Thy_Header.Keywords,
    syntax: Outer_Syntax,
    sources: List[(Path, SHA1.Digest)],
    session_graph: Graph_Display.Graph)

  sealed case class Deps(deps: Map[String, Session_Content])
  {
    def is_empty: Boolean = deps.isEmpty
    def apply(name: String): Session_Content = deps(name)
    def sources(name: String): List[SHA1.Digest] = deps(name).sources.map(_._2)
  }

  def dependencies(
      progress: Progress = Ignore_Progress,
      inlined_files: Boolean = false,
      verbose: Boolean = false,
      list_files: Boolean = false,
      check_keywords: Set[String] = Set.empty,
      tree: Session_Tree): Deps =
    Deps((Map.empty[String, Session_Content] /: tree.topological_order)(
      { case (deps, (name, info)) =>
          if (progress.stopped) throw Exn.Interrupt()

          try {
            val (loaded_theories0, known_theories0, syntax0) =
              info.parent.map(deps(_)) match {
                case None =>
                  (Set.empty[String], Map.empty[String, Document.Node.Name],
                    Thy_Header.bootstrap_syntax)
                case Some(parent) =>
                  (parent.loaded_theories, parent.known_theories, parent.syntax)
              }
            val resources = new Resources(loaded_theories0, known_theories0, syntax0)
            val thy_info = new Thy_Info(resources)

            if (verbose || list_files) {
              val groups =
                if (info.groups.isEmpty) ""
                else info.groups.mkString(" (", " ", ")")
              progress.echo("Session " + info.chapter + "/" + name + groups)
            }

            val thy_deps =
            {
              val root_theories =
                info.theories.flatMap({
                  case (global, _, thys) =>
                    thys.map(thy =>
                      (resources.node_name(
                        if (global) "" else name, info.dir + Resources.thy_path(thy)), info.pos))
                })
              val thy_deps = thy_info.dependencies(name, root_theories)

              thy_deps.errors match {
                case Nil => thy_deps
                case errs => error(cat_lines(errs))
              }
            }

            val known_theories =
              (known_theories0 /: thy_deps.deps)({ case (known, dep) =>
                val name = dep.name
                known.get(name.theory) match {
                  case Some(name1) if name != name1 =>
                    error("Duplicate theory " + quote(name.node) + " vs. " + quote(name1.node))
                  case _ =>
                    known + (name.theory -> name) + (Long_Name.base_name(name.theory) -> name)
                }
              })

            val loaded_theories = thy_deps.loaded_theories
            val keywords = thy_deps.keywords
            val syntax = thy_deps.syntax.asInstanceOf[Outer_Syntax]

            val theory_files = thy_deps.deps.map(dep => Path.explode(dep.name.node))
            val loaded_files = if (inlined_files) thy_deps.loaded_files else Nil

            val all_files =
              (theory_files ::: loaded_files :::
                info.files.map(file => info.dir + file) :::
                info.document_files.map(file => info.dir + file._1 + file._2)).map(_.expand)

            if (list_files)
              progress.echo(cat_lines(all_files.map(_.implode).sorted.map("  " + _)))

            if (check_keywords.nonEmpty)
              Check_Keywords.check_keywords(progress, syntax.keywords, check_keywords, theory_files)

            val sources = all_files.map(p => (p, SHA1.digest(p.file)))

            val session_graph =
              Present.session_graph(info.parent getOrElse "", loaded_theories0, thy_deps.deps)

            val content =
              Session_Content(loaded_theories, known_theories, keywords, syntax,
                sources, session_graph)
            deps + (name -> content)
          }
          catch {
            case ERROR(msg) =>
              cat_error(msg, "The error(s) above occurred in session " +
                quote(name) + Position.here(info.pos))
          }
      }))

  def session_dependencies(
    options: Options,
    inlined_files: Boolean,
    dirs: List[Path],
    sessions: List[String]): Deps =
  {
    val (_, tree) = find_sessions(options, dirs = dirs).selection(sessions = sessions)
    dependencies(inlined_files = inlined_files, tree = tree)
  }

  def session_content(
    options: Options,
    inlined_files: Boolean,
    dirs: List[Path],
    session: String): Session_Content =
  {
    session_dependencies(options, inlined_files, dirs, List(session))(session)
  }

  def outer_syntax(options: Options, session: String): Outer_Syntax =
    session_content(options, false, Nil, session).syntax


  /* jobs */

  private class Job(progress: Progress,
    name: String, val info: Session_Info, output: Path, do_output: Boolean, verbose: Boolean,
    browser_info: Path, session_graph: Graph_Display.Graph, command_timings: List[Properties.T])
  {
    def output_path: Option[Path] = if (do_output) Some(output) else None

    private val parent = info.parent.getOrElse("")

    private val graph_file = Isabelle_System.tmp_file("session_graph", "pdf")
    try { isabelle.graphview.Graph_File.write(info.options, graph_file, session_graph) }
    catch { case ERROR(_) => /*error should be exposed in ML*/ }

    private val args_file = Isabelle_System.tmp_file("args")
    File.write(args_file, YXML.string_of_body(
      if (is_pure(name)) Options.encode(info.options)
      else
        {
          val theories = info.theories.map(x => (x._2, x._3))
          import XML.Encode._
              pair(list(properties), pair(bool, pair(Options.encode, pair(bool, pair(Path.encode,
                pair(list(pair(Path.encode, Path.encode)), pair(string, pair(string,
                pair(string, pair(string, list(pair(Options.encode, list(Path.encode)))))))))))))(
              (command_timings, (do_output, (info.options, (verbose, (browser_info,
                (info.document_files, (Isabelle_System.posix_path(graph_file), (parent,
                (info.chapter, (name, theories)))))))))))
        }))

    private val env =
      Map("INPUT" -> parent, "TARGET" -> name, "OUTPUT" -> Isabelle_System.standard_path(output),
        (if (is_pure(name)) "ISABELLE_PROCESS_OPTIONS" else "ARGS_FILE") ->
          Isabelle_System.posix_path(args_file))

    private val script =
      if (is_pure(name)) {
        if (do_output) "./build " + name + " \"$OUTPUT\""
        else """ rm -f "$OUTPUT"; ./build """ + name
      }
      else {
        """
        . "$ISABELLE_HOME/lib/scripts/timestart.bash"
        """ +
          (if (do_output)
            """
            "$ISABELLE_PROCESS" -e "Build.build \"$ARGS_FILE\";" -q -w "$INPUT" "$OUTPUT"
            """
          else
            """
            rm -f "$OUTPUT"; "$ISABELLE_PROCESS" -e "Build.build \"$ARGS_FILE\";" -r -q "$INPUT"
            """) +
        """
        RC="$?"

        . "$ISABELLE_HOME/lib/scripts/timestop.bash"

        if [ "$RC" -eq 0 ]; then
          echo "Finished $TARGET ($TIMES_REPORT)" >&2
        fi

        exit "$RC"
        """
      }

    private val (thread, result) =
      Simple_Thread.future("build") {
        Isabelle_System.bash_env(info.dir.file, env, script,
          progress_stdout = (line: String) =>
            Library.try_unprefix("\floading_theory = ", line) match {
              case Some(theory) => progress.theory(name, theory)
              case None =>
            },
          progress_limit =
            info.options.int("process_output_limit") match {
              case 0 => None
              case m => Some(m * 1000000L)
            },
          strict = false)
      }

    def terminate: Unit = thread.interrupt
    def is_finished: Boolean = result.is_finished

    @volatile private var was_timeout = false
    private val timeout_request: Option[Event_Timer.Request] =
    {
      val timeout = info.options.seconds("timeout")
      if (timeout > Time.zero)
        Some(Event_Timer.request(Time.now() + timeout) { terminate; was_timeout = true })
      else None
    }

    def join: Isabelle_System.Bash_Result =
    {
      val res = result.join

      graph_file.delete
      args_file.delete
      timeout_request.foreach(_.cancel)

      if (res.rc == Exn.Interrupt.return_code) {
        if (was_timeout) res.add_err(Output.error_text("Timeout")).set_rc(1)
        else res.add_err(Output.error_text("Interrupt"))
      }
      else res
    }
  }


  /* inlined properties (YXML) */

  object Props
  {
    def parse(text: String): Properties.T = XML.Decode.properties(YXML.parse_body(text))

    def parse_lines(prefix: String, lines: List[String]): List[Properties.T] =
      for (line <- lines; s <- Library.try_unprefix(prefix, line)) yield parse(s)

    def find_parse_line(prefix: String, lines: List[String]): Option[Properties.T] =
      lines.find(_.startsWith(prefix)).map(line => parse(line.substring(prefix.length)))
  }


  /* log files */

  private val LOG = Path.explode("log")
  private def log(name: String): Path = LOG + Path.basic(name)
  private def log_gz(name: String): Path = log(name).ext("gz")

  private val SESSION_NAME = "\fSession.name = "


  sealed case class Log_Info(
    name: String,
    stats: List[Properties.T],
    tasks: List[Properties.T],
    command_timings: List[Properties.T],
    session_timing: Properties.T)

  def parse_log(full_stats: Boolean, text: String): Log_Info =
  {
    val lines = split_lines(text)
    val xml_cache = new XML.Cache()
    def parse_lines(prfx: String): List[Properties.T] =
      Props.parse_lines(prfx, lines).map(xml_cache.props(_))

    val name =
      lines.find(_.startsWith(SESSION_NAME)).map(_.substring(SESSION_NAME.length)) getOrElse ""
    val stats = if (full_stats) parse_lines("\fML_statistics = ") else Nil
    val tasks = if (full_stats) parse_lines("\ftask_statistics = ") else Nil
    val command_timings = parse_lines("\fcommand_timing = ")
    val session_timing = Props.find_parse_line("\fTiming = ", lines) getOrElse Nil
    Log_Info(name, stats, tasks, command_timings, session_timing)
  }


  /* sources and heaps */

  private def sources_stamp(digests: List[SHA1.Digest]): String =
    digests.map(_.toString).sorted.mkString("sources: ", " ", "")

  private val no_heap: String = "heap: -"

  private def heap_stamp(heap: Option[Path]): String =
  {
    "heap: " +
      (heap match {
        case Some(path) =>
          val file = path.file
          if (file.isFile) file.length.toString + " " + file.lastModified.toString
          else "-"
        case None => "-"
      })
  }

  private def read_stamps(path: Path): Option[(String, String, String)] =
    if (path.is_file) {
      val stream = new GZIPInputStream (new BufferedInputStream(new FileInputStream(path.file)))
      val reader = new BufferedReader(new InputStreamReader(stream, UTF8.charset))
      val (s, h1, h2) =
        try { (reader.readLine, reader.readLine, reader.readLine) }
        finally { reader.close }
      if (s != null && s.startsWith("sources: ") &&
          h1 != null && h1.startsWith("heap: ") &&
          h2 != null && h2.startsWith("heap: ")) Some((s, h1, h2))
      else None
    }
    else None


  /* build_results */

  def build_results(
    options: Options,
    progress: Progress = Ignore_Progress,
    requirements: Boolean = false,
    all_sessions: Boolean = false,
    build_heap: Boolean = false,
    clean_build: Boolean = false,
    dirs: List[Path] = Nil,
    select_dirs: List[Path] = Nil,
    exclude_session_groups: List[String] = Nil,
    session_groups: List[String] = Nil,
    max_jobs: Int = 1,
    list_files: Boolean = false,
    check_keywords: Set[String] = Set.empty,
    no_build: Boolean = false,
    system_mode: Boolean = false,
    verbose: Boolean = false,
    exclude_sessions: List[String] = Nil,
    sessions: List[String] = Nil): Map[String, Int] =
  {
    /* session tree and dependencies */

    val full_tree = find_sessions(options.int("completion_limit") = 0, dirs, select_dirs)
    val (selected, selected_tree) =
      full_tree.selection(requirements, all_sessions,
        exclude_session_groups, exclude_sessions, session_groups, sessions)

    val deps = dependencies(progress, true, verbose, list_files, check_keywords, selected_tree)

    def make_stamp(name: String): String =
      sources_stamp(selected_tree(name).entry_digest :: deps.sources(name))


    /* persistent information */

    val (input_dirs, output_dir, browser_info) =
      if (system_mode) {
        val output_dir = Path.explode("~~/heaps/$ML_IDENTIFIER")
        (List(output_dir), output_dir, Path.explode("~~/browser_info"))
      }
      else {
        val output_dir = Path.explode("$ISABELLE_OUTPUT")
        (output_dir :: Isabelle_System.find_logics_dirs(), output_dir,
         Path.explode("$ISABELLE_BROWSER_INFO"))
      }

    def find_log(name: String): Option[(Path, Path)] =
      input_dirs.find(dir => (dir + log(name)).is_file).map(dir => (dir, dir + log(name)))


    /* queue with scheduling information */

    def load_timings(name: String): (List[Properties.T], Double) =
    {
      val (path, text) =
        find_log(name + ".gz") match {
          case Some((_, path)) => (path, File.read_gzip(path))
          case None =>
            find_log(name) match {
              case Some((_, path)) => (path, File.read(path))
              case None => (Path.current, "")
            }
        }

      def ignore_error(msg: String): (List[Properties.T], Double) =
      {
        Output.warning("Ignoring bad log file: " + path + (if (msg == "") "" else "\n" + msg))
        (Nil, 0.0)
      }

      try {
        val info = parse_log(false, text)
        val session_timing = Markup.Elapsed.unapply(info.session_timing) getOrElse 0.0
        (info.command_timings, session_timing)
      }
      catch {
        case ERROR(msg) => ignore_error(msg)
        case exn: java.lang.Error => ignore_error(Exn.message(exn))
        case _: XML.Error => ignore_error("")
      }
    }

    val queue = Queue(selected_tree, load_timings)


    /* main build process */

    // prepare log dir
    Isabelle_System.mkdirs(output_dir + LOG)

    // optional cleanup
    if (clean_build) {
      for (name <- full_tree.graph.all_succs(selected)) {
        val files =
          List(Path.basic(name), log(name), log_gz(name)).map(output_dir + _).filter(_.is_file)
        if (files.nonEmpty) progress.echo("Cleaning " + name + " ...")
        if (!files.forall(p => p.file.delete)) progress.echo(name + " FAILED to delete")
      }
    }

    // scheduler loop
    case class Result(current: Boolean, heap: String, rc: Int)

    def sleep()
    {
      try { Thread.sleep(500) }
      catch { case Exn.Interrupt() => Exn.Interrupt.impose() }
    }

    @tailrec def loop(
      pending: Queue,
      running: Map[String, (String, Job)],
      results: Map[String, Result]): Map[String, Result] =
    {
      if (pending.is_empty) results
      else {
        if (progress.stopped)
          for ((_, (_, job)) <- running) job.terminate

        running.find({ case (_, (_, job)) => job.is_finished }) match {
          case Some((name, (parent_heap, job))) =>
            //{{{ finish job

            val res = job.join
            progress.echo(res.err)

            val heap =
              if (res.rc == 0) {
                (output_dir + log(name)).file.delete

                val sources = make_stamp(name)
                val heap = heap_stamp(job.output_path)
                File.write_gzip(output_dir + log_gz(name),
                  Library.terminate_lines(sources :: parent_heap :: heap :: res.out_lines))

                heap
              }
              else {
                (output_dir + Path.basic(name)).file.delete
                (output_dir + log_gz(name)).file.delete

                File.write(output_dir + log(name), Library.terminate_lines(res.out_lines))
                progress.echo(name + " FAILED")
                if (res.rc != Exn.Interrupt.return_code) {
                  progress.echo("(see also " + (output_dir + log(name)).file.toString + ")")
                  val lines = res.out_lines.filterNot(_.startsWith("\f"))
                  val tail = lines.drop(lines.length - 20 max 0)
                  progress.echo("\n" + cat_lines(tail))
                }

                no_heap
              }
            loop(pending - name, running - name,
              results + (name -> Result(false, heap, res.rc)))
            //}}}
          case None if running.size < (max_jobs max 1) =>
            //{{{ check/start next job
            pending.dequeue(running.isDefinedAt(_)) match {
              case Some((name, info)) =>
                val parent_result =
                  info.parent match {
                    case None => Result(true, no_heap, 0)
                    case Some(parent) => results(parent)
                  }
                val output = output_dir + Path.basic(name)
                val do_output = build_heap || queue.is_inner(name)

                val (current, heap) =
                {
                  find_log(name + ".gz") match {
                    case Some((dir, path)) =>
                      read_stamps(path) match {
                        case Some((s, h1, h2)) =>
                          val heap = heap_stamp(Some(dir + Path.basic(name)))
                          (s == make_stamp(name) && h1 == parent_result.heap && h2 == heap &&
                            !(do_output && heap == no_heap), heap)
                        case None => (false, no_heap)
                      }
                    case None => (false, no_heap)
                  }
                }
                val all_current = current && parent_result.current

                if (all_current)
                  loop(pending - name, running, results + (name -> Result(true, heap, 0)))
                else if (no_build) {
                  if (verbose) progress.echo("Skipping " + name + " ...")
                  loop(pending - name, running, results + (name -> Result(false, heap, 1)))
                }
                else if (parent_result.rc == 0 && !progress.stopped) {
                  progress.echo((if (do_output) "Building " else "Running ") + name + " ...")
                  val job =
                    new Job(progress, name, info, output, do_output, verbose, browser_info,
                      deps(name).session_graph, queue.command_timings(name))
                  loop(pending, running + (name -> (parent_result.heap, job)), results)
                }
                else {
                  progress.echo(name + " CANCELLED")
                  loop(pending - name, running, results + (name -> Result(false, heap, 1)))
                }
              case None => sleep(); loop(pending, running, results)
            }
            ///}}}
          case None => sleep(); loop(pending, running, results)
        }
      }
    }


    /* build results */

    val results =
      if (deps.is_empty) {
        progress.echo(Output.warning_text("Nothing to build"))
        Map.empty[String, Result]
      }
      else loop(queue, Map.empty, Map.empty)


    /* global browser info */

    if (!no_build) {
      val browser_chapters =
        (for {
          (name, result) <- results.iterator
          if result.rc == 0
          info = full_tree(name)
          if info.options.bool("browser_info")
        } yield (info.chapter, (name, info.description))).toList.groupBy(_._1).
            map({ case (chapter, es) => (chapter, es.map(_._2)) }).filterNot(_._2.isEmpty)

      for ((chapter, entries) <- browser_chapters)
        Present.update_chapter_index(browser_info, chapter, entries)

      if (browser_chapters.nonEmpty && !(browser_info + Path.explode("index.html")).is_file)
      {
        Isabelle_System.mkdirs(browser_info)
        File.copy(Path.explode("~~/lib/logo/isabelle.gif"),
          browser_info + Path.explode("isabelle.gif"))
        File.write(browser_info + Path.explode("index.html"),
          File.read(Path.explode("~~/lib/html/library_index_header.template")) +
          File.read(Path.explode("~~/lib/html/library_index_content.template")) +
          File.read(Path.explode("~~/lib/html/library_index_footer.template")))
      }
    }


    /* results */

    results.map({ case (name, result) => (name, result.rc) })
  }


  /* build */

  def build(
    options: Options,
    progress: Progress = Ignore_Progress,
    requirements: Boolean = false,
    all_sessions: Boolean = false,
    build_heap: Boolean = false,
    clean_build: Boolean = false,
    dirs: List[Path] = Nil,
    select_dirs: List[Path] = Nil,
    exclude_session_groups: List[String] = Nil,
    session_groups: List[String] = Nil,
    max_jobs: Int = 1,
    list_files: Boolean = false,
    check_keywords: Set[String] = Set.empty,
    no_build: Boolean = false,
    system_mode: Boolean = false,
    verbose: Boolean = false,
    exclude_sessions: List[String] = Nil,
    sessions: List[String] = Nil): Int =
  {
    val results =
      build_results(options, progress, requirements, all_sessions, build_heap, clean_build,
        dirs, select_dirs, exclude_session_groups, session_groups, max_jobs, list_files,
        check_keywords, no_build, system_mode, verbose, exclude_sessions, sessions)

    val rc = (0 /: results)({ case (rc1, (_, rc2)) => rc1 max rc2 })
    if (rc != 0 && (verbose || !no_build)) {
      val unfinished =
        (for ((name, r) <- results.iterator if r != 0) yield name).toList.sorted
      progress.echo("Unfinished session(s): " + commas(unfinished))
    }
    rc
  }


  /* command line entry point */

  def main(args: Array[String])
  {
    Command_Line.tool {
      args.toList match {
        case
          Properties.Value.Boolean(requirements) ::
          Properties.Value.Boolean(all_sessions) ::
          Properties.Value.Boolean(build_heap) ::
          Properties.Value.Boolean(clean_build) ::
          Properties.Value.Int(max_jobs) ::
          Properties.Value.Boolean(list_files) ::
          Properties.Value.Boolean(no_build) ::
          Properties.Value.Boolean(system_mode) ::
          Properties.Value.Boolean(verbose) ::
          Command_Line.Chunks(dirs, select_dirs, session_groups, check_keywords,
              build_options, exclude_session_groups, exclude_sessions, sessions) =>
            val options = (Options.init() /: build_options)(_ + _)
            val progress = new Console_Progress(verbose)
            progress.interrupt_handler {
              build(options, progress, requirements, all_sessions, build_heap, clean_build,
                dirs.map(Path.explode(_)), select_dirs.map(Path.explode(_)), exclude_session_groups,
                session_groups, max_jobs, list_files, check_keywords.toSet, no_build, system_mode,
                verbose, exclude_sessions, sessions)
            }
        case _ => error("Bad arguments:\n" + cat_lines(args))
      }
    }
  }


  /* PIDE protocol */

  def build_theories(
    session: Session, master_dir: Path, theories: List[(Options, List[Path])]): Promise[XML.Body] =
      session.get_protocol_handler(classOf[Handler].getName) match {
        case Some(handler: Handler) => handler.build_theories(session, master_dir, theories)
        case _ => error("Cannot invoke build_theories: bad protocol handler")
      }

  class Handler(progress: Progress, session_name: String) extends Session.Protocol_Handler
  {
    private val pending = Synchronized(Map.empty[String, Promise[XML.Body]])

    def build_theories(
      session: Session, master_dir: Path, theories: List[(Options, List[Path])]): Promise[XML.Body] =
    {
      val promise = Future.promise[XML.Body]
      val id = Document_ID.make().toString
      pending.change(promises => promises + (id -> promise))
      session.build_theories(id, master_dir, theories)
      promise
    }

    private def loading_theory(prover: Prover, msg: Prover.Protocol_Output): Boolean =
      msg.properties match {
        case Markup.Loading_Theory(name) => progress.theory(session_name, name); true
        case _ => false
      }

    private def build_theories_result(prover: Prover, msg: Prover.Protocol_Output): Boolean =
      msg.properties match {
        case Markup.Build_Theories_Result(id) =>
          pending.change_result(promises =>
            promises.get(id) match {
              case Some(promise) =>
                val error_message =
                  try { YXML.parse_body(Symbol.decode(msg.text)) }
                  catch { case exn: Throwable => List(XML.Text(Exn.message(exn))) }
                promise.fulfill(error_message)
                (true, promises - id)
              case None =>
                (false, promises)
            })
        case _ => false
      }

    override def stop(prover: Prover): Unit =
      pending.change(promises => { for ((_, promise) <- promises) promise.cancel; Map.empty })

    val functions =
      Map(
        Markup.BUILD_THEORIES_RESULT -> build_theories_result _,
        Markup.LOADING_THEORY -> loading_theory _)
  }
}
