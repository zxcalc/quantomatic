/*  Title:      Pure/Admin/build_log.scala
    Author:     Makarius

Build log parsing for current and historic formats.
*/

package isabelle


import java.io.{File => JFile}
import java.time.ZoneId
import java.time.format.{DateTimeFormatter, DateTimeParseException}
import java.util.Locale

import scala.collection.mutable
import scala.util.matching.Regex


object Build_Log
{
  /** content **/

  /* properties */

  object Prop
  {
    val separator = '\u000b'

    def multiple(name: String, args: List[String]): Properties.T =
      if (args.isEmpty) Nil
      else List(name -> args.mkString(separator.toString))

    val build_tags = "build_tags"  // multiple
    val build_args = "build_args"  // multiple
    val build_group_id = "build_group_id"
    val build_id = "build_id"
    val build_engine = "build_engine"
    val build_host = "build_host"
    val build_start = "build_start"
    val build_end = "build_end"
    val isabelle_version = "isabelle_version"
    val afp_version = "afp_version"
  }


  /* settings */

  object Settings
  {
    val build_settings = List("ISABELLE_BUILD_OPTIONS")
    val ml_settings = List("ML_PLATFORM", "ML_HOME", "ML_SYSTEM", "ML_OPTIONS")
    val all_settings = build_settings ::: ml_settings

    type Entry = (String, String)
    type T = List[Entry]

    object Entry
    {
      def unapply(s: String): Option[Entry] =
        s.indexOf('=') match {
          case -1 => None
          case i =>
            val a = s.substring(0, i)
            val b = Library.perhaps_unquote(s.substring(i + 1))
            Some((a, b))
        }
      def apply(a: String, b: String): String = a + "=" + quote(b)
      def getenv(a: String): String = apply(a, Isabelle_System.getenv(a))
    }

    def show(): String =
      cat_lines(
        build_settings.map(Entry.getenv(_)) ::: List("") ::: ml_settings.map(Entry.getenv(_)))
  }


  /* file names */

  def log_date(date: Date): String =
    String.format(Locale.ROOT, "%s.%05d",
      DateTimeFormatter.ofPattern("yyyy-MM-dd").format(date.rep),
      new java.lang.Long((date.time - date.midnight.time).ms / 1000))

  def log_subdir(date: Date): Path =
    Path.explode("log") + Path.explode(date.rep.getYear.toString)

  def log_filename(engine: String, date: Date, more: List[String] = Nil): Path =
    Path.explode((engine :: log_date(date) :: more).mkString("", "_", ".log"))


  /* log file collections */

  def is_log(file: JFile): Boolean =
    List(".log", ".log.gz", ".log.xz").exists(ext => file.getName.endsWith(ext))

  def isatest_files(dir: Path): List[JFile] =
    File.find_files(dir.file, file => is_log(file) && file.getName.startsWith("isatest-makeall-"))

  def afp_test_files(dir: Path): List[JFile] =
    File.find_files(dir.file, file => is_log(file) && file.getName.startsWith("afp-test-devel-"))



  /** log file **/

  def print_date(date: Date): String = Log_File.Date_Format(date)

  object Log_File
  {
    def apply(name: String, lines: List[String]): Log_File =
      new Log_File(name, lines)

    def apply(name: String, text: String): Log_File =
      Log_File(name, Library.trim_split_lines(text))

    def apply(file: JFile): Log_File =
    {
      val name = file.getName
      val (base_name, text) =
        Library.try_unsuffix(".gz", name) match {
          case Some(base_name) => (base_name, File.read_gzip(file))
          case None =>
            Library.try_unsuffix(".xz", name) match {
              case Some(base_name) => (base_name, File.read_xz(file))
              case None => (name, File.read(file))
            }
          }
      apply(base_name, text)
    }

    def apply(path: Path): Log_File = apply(path.file)


    /* date format */

    val Date_Format =
    {
      val fmts =
        Date.Formatter.variants(
          List("EEE MMM d HH:mm:ss O yyyy", "EEE MMM d HH:mm:ss VV yyyy"),
          List(Locale.ENGLISH, Locale.GERMAN)) :::
        List(
          DateTimeFormatter.RFC_1123_DATE_TIME,
          Date.Formatter.pattern("EEE MMM d HH:mm:ss yyyy").withZone(ZoneId.of("Europe/Berlin")))

      def tune_timezone(s: String): String =
        s match {
          case "CET" | "MET" => "GMT+1"
          case "CEST" | "MEST" => "GMT+2"
          case "EST" => "Europe/Berlin"
          case _ => s
        }
      def tune_weekday(s: String): String =
        s match {
          case "Die" => "Di"
          case "Mit" => "Mi"
          case "Don" => "Do"
          case "Fre" => "Fr"
          case "Sam" => "Sa"
          case "Son" => "So"
          case _ => s
        }

      def tune(s: String): String =
        Word.implode(
          Word.explode(s) match {
            case a :: "M\uFFFDr" :: bs => tune_weekday(a) :: "Mär" :: bs.map(tune_timezone(_))
            case a :: bs => tune_weekday(a) :: bs.map(tune_timezone(_))
            case Nil => Nil
          }
        )

      Date.Format.make(fmts, tune)
    }


    /* inlined content */

    def print_props(marker: String, props: Properties.T): String =
      marker + YXML.string_of_body(XML.Encode.properties(props))
  }

  class Log_File private(val name: String, val lines: List[String])
  {
    log_file =>

    override def toString: String = name

    def text: String = cat_lines(lines)

    def err(msg: String): Nothing =
      error("Error in log file " + quote(name) + ": " + msg)


    /* date format */

    object Strict_Date
    {
      def unapply(s: String): Some[Date] =
        try { Some(Log_File.Date_Format.parse(s)) }
        catch { case exn: DateTimeParseException => log_file.err(exn.getMessage) }
    }


    /* inlined content */

    def find[A](f: String => Option[A]): Option[A] =
      lines.iterator.map(f).find(_.isDefined).map(_.get)

    def find_line(marker: String): Option[String] =
      find(Library.try_unprefix(marker, _))

    def find_match(regex: Regex): Option[String] =
      lines.iterator.map(regex.unapplySeq(_)).find(res => res.isDefined && res.get.length == 1).
        map(res => res.get.head)


    /* settings */

    def get_setting(a: String): Option[Settings.Entry] =
      lines.find(_.startsWith(a + "=")) match {
        case Some(line) => Settings.Entry.unapply(line)
        case None => None
      }

    def get_settings(as: List[String]): Settings.T =
      for { a <- as; entry <- get_setting(a) } yield entry


    /* properties (YXML) */

    val xml_cache = new XML.Cache()

    def parse_props(text: String): Properties.T =
      xml_cache.props(XML.Decode.properties(YXML.parse_body(text)))

    def filter_props(marker: String): List[Properties.T] =
      for {
        line <- lines
        s <- Library.try_unprefix(marker, line)
        if YXML.detect(s)
      } yield parse_props(s)

    def find_props(marker: String): Option[Properties.T] =
      find_line(marker) match {
        case Some(text) if YXML.detect(text) => Some(parse_props(text))
        case _ => None
      }


    /* parse various formats */

    def parse_meta_info(): Meta_Info = Build_Log.parse_meta_info(log_file)

    def parse_build_info(): Build_Info = Build_Log.parse_build_info(log_file)

    def parse_session_info(
        default_name: String = "",
        command_timings: Boolean = false,
        ml_statistics: Boolean = false,
        task_statistics: Boolean = false): Session_Info =
      Build_Log.parse_session_info(
        log_file, default_name, command_timings, ml_statistics, task_statistics)
  }



  /** meta info **/

  object Meta_Info
  {
    val empty: Meta_Info = Meta_Info(Nil, Nil)
  }

  sealed case class Meta_Info(props: Properties.T, settings: List[(String, String)])
  {
    def is_empty: Boolean = props.isEmpty && settings.isEmpty
  }

  object Isatest
  {
    val engine = "isatest"
    val Start = new Regex("""^------------------- starting test --- (.+) --- (.+)$""")
    val End = new Regex("""^------------------- test (?:successful|FAILED) --- (.+) --- .*$""")
    val Isabelle_Version = new Regex("""^Isabelle version: (\S+)$""")
    val No_AFP_Version = new Regex("""$.""")
  }

  object AFP_Test
  {
    val engine = "afp-test"
    val Start = new Regex("""^Start test(?: for .+)? at ([^,]+), (.*)$""")
    val Start_Old = new Regex("""^Start test(?: for .+)? at ([^,]+)$""")
    val End = new Regex("""^End test on (.+), .+, elapsed time:.*$""")
    val Isabelle_Version = new Regex("""^Isabelle version: .* -- hg id (\S+)$""")
    val AFP_Version = new Regex("""^AFP version: .* -- hg id (\S+)$""")
    val Bad_Init = new Regex("""^cp:.*: Disc quota exceeded$""")
  }

  object Jenkins
  {
    val engine = "jenkins"
    val Start = new Regex("""^Started .*$""")
    val Start_Date = new Regex("""^Build started at (.+)$""")
    val No_End = new Regex("""$.""")
    val Isabelle_Version = new Regex("""^Isabelle id (\S+)$""")
    val AFP_Version = new Regex("""^AFP id (\S+)$""")
    val CONFIGURATION = "=== CONFIGURATION ==="
    val BUILD = "=== BUILD ==="
    val FINISHED = "Finished: "
  }

  private def parse_meta_info(log_file: Log_File): Meta_Info =
  {
    def parse(engine: String, host: String, start: Date,
      End: Regex, Isabelle_Version: Regex, AFP_Version: Regex): Meta_Info =
    {
      val build_id =
      {
        val prefix = if (host != "") host else if (engine != "") engine else ""
        (if (prefix == "") "build" else prefix) + ":" + start.time.ms
      }
      val build_engine = if (engine == "") Nil else List(Prop.build_engine -> engine)
      val build_host = if (host == "") Nil else List(Prop.build_host -> host)

      val start_date = List(Prop.build_start -> start.toString)
      val end_date =
        log_file.lines.last match {
          case End(log_file.Strict_Date(end_date)) =>
            List(Prop.build_end -> end_date.toString)
          case _ => Nil
        }

      val isabelle_version =
        log_file.find_match(Isabelle_Version).map(Prop.isabelle_version -> _)
      val afp_version =
        log_file.find_match(AFP_Version).map(Prop.afp_version -> _)

      Meta_Info((Prop.build_id -> build_id) :: build_engine ::: build_host :::
          start_date ::: end_date ::: isabelle_version.toList ::: afp_version.toList,
        log_file.get_settings(Settings.all_settings))
    }

    log_file.lines match {
      case line :: _ if line.startsWith(Build_History.META_INFO_MARKER) =>
        Meta_Info(log_file.find_props(Build_History.META_INFO_MARKER).get,
          log_file.get_settings(Settings.all_settings))

      case Isatest.Start(log_file.Strict_Date(start), host) :: _ =>
        parse(Isatest.engine, host, start, Isatest.End,
          Isatest.Isabelle_Version, Isatest.No_AFP_Version)

      case AFP_Test.Start(log_file.Strict_Date(start), host) :: _ =>
        parse(AFP_Test.engine, host, start, AFP_Test.End,
          AFP_Test.Isabelle_Version, AFP_Test.AFP_Version)

      case AFP_Test.Start_Old(log_file.Strict_Date(start)) :: _ =>
        parse(AFP_Test.engine, "", start, AFP_Test.End,
          AFP_Test.Isabelle_Version, AFP_Test.AFP_Version)

      case Jenkins.Start() :: _
      if log_file.lines.contains(Jenkins.CONFIGURATION) ||
         log_file.lines.last.startsWith(Jenkins.FINISHED) =>
        log_file.lines.dropWhile(_ != Jenkins.BUILD) match {
          case Jenkins.BUILD :: _ :: Jenkins.Start_Date(log_file.Strict_Date(start)) :: _ =>
            parse(Jenkins.engine, "", start.to(ZoneId.of("Europe/Berlin")), Jenkins.No_End,
              Jenkins.Isabelle_Version, Jenkins.AFP_Version)
          case _ => Meta_Info.empty
        }

      case line :: _ if line.startsWith("\u0000") => Meta_Info.empty
      case List(Isatest.End(_)) => Meta_Info.empty
      case _ :: AFP_Test.Bad_Init() :: _ => Meta_Info.empty
      case Nil => Meta_Info.empty

      case _ => log_file.err("cannot detect log file format")
    }
  }



  /** build info: produced by isabelle build or build_history **/

  val ML_STATISTICS_MARKER = "\fML_statistics = "
  val SESSION_NAME = "session_name"

  object Session_Status extends Enumeration
  {
    val EXISTING = Value("existing")
    val FINISHED = Value("finished")
    val FAILED = Value("failed")
    val CANCELLED = Value("cancelled")
  }

  sealed case class Session_Entry(
    chapter: String,
    groups: List[String],
    threads: Option[Int],
    timing: Timing,
    ml_timing: Timing,
    ml_statistics: List[Properties.T],
    heap_size: Option[Long],
    status: Session_Status.Value)
  {
    def finished: Boolean = status == Session_Status.FINISHED
  }

  sealed case class Build_Info(sessions: Map[String, Session_Entry])
  {
    def session(name: String): Session_Entry = sessions(name)
    def get_session(name: String): Option[Session_Entry] = sessions.get(name)

    def get_default[A](name: String, f: Session_Entry => A, x: A): A =
      get_session(name) match {
        case Some(entry) => f(entry)
        case None => x
      }

    def finished_sessions: List[String] = sessions.keySet.iterator.filter(finished(_)).toList
    def finished(name: String): Boolean = get_default(name, _.finished, false)
    def timing(name: String): Timing = get_default(name, _.timing, Timing.zero)
    def ml_timing(name: String): Timing = get_default(name, _.ml_timing, Timing.zero)
  }

  private def parse_build_info(log_file: Log_File): Build_Info =
  {
    object Chapter_Name
    {
      def unapply(s: String): Some[(String, String)] =
        space_explode('/', s) match {
          case List(chapter, name) => Some((chapter, name))
          case _ => Some(("", s))
        }
    }

    val Session_No_Groups = new Regex("""^Session (\S+)$""")
    val Session_Groups = new Regex("""^Session (\S+) \((.*)\)$""")
    val Session_Finished1 =
      new Regex("""^Finished (\S+) \((\d+):(\d+):(\d+) elapsed time, (\d+):(\d+):(\d+) cpu time.*$""")
    val Session_Finished2 =
      new Regex("""^Finished (\S+) \((\d+):(\d+):(\d+) elapsed time.*$""")
    val Session_Timing =
      new Regex("""^Timing (\S+) \((\d) threads, (\d+\.\d+)s elapsed time, (\d+\.\d+)s cpu time, (\d+\.\d+)s GC time.*$""")
    val Session_Started = new Regex("""^(?:Running|Building) (\S+) \.\.\.$""")
    val Session_Failed = new Regex("""^(\S+) FAILED""")
    val Session_Cancelled = new Regex("""^(\S+) CANCELLED""")
    val Heap = new Regex("""^Heap (\S+) \((\d+) bytes\)$""")

    var chapter = Map.empty[String, String]
    var groups = Map.empty[String, List[String]]
    var threads = Map.empty[String, Int]
    var timing = Map.empty[String, Timing]
    var ml_timing = Map.empty[String, Timing]
    var started = Set.empty[String]
    var failed = Set.empty[String]
    var cancelled = Set.empty[String]
    var ml_statistics = Map.empty[String, List[Properties.T]]
    var heap_sizes = Map.empty[String, Long]

    def all_sessions: Set[String] =
      chapter.keySet ++ groups.keySet ++ threads.keySet ++ timing.keySet ++ ml_timing.keySet ++
      failed ++ cancelled ++ started ++ ml_statistics.keySet ++ heap_sizes.keySet


    for (line <- log_file.lines) {
      line match {
        case Session_No_Groups(Chapter_Name(chapt, name)) =>
          chapter += (name -> chapt)
          groups += (name -> Nil)

        case Session_Groups(Chapter_Name(chapt, name), grps) =>
          chapter += (name -> chapt)
          groups += (name -> Word.explode(grps))

        case Session_Started(name) =>
          started += name

        case Session_Finished1(name,
            Value.Int(e1), Value.Int(e2), Value.Int(e3),
            Value.Int(c1), Value.Int(c2), Value.Int(c3)) =>
          val elapsed = Time.hms(e1, e2, e3)
          val cpu = Time.hms(c1, c2, c3)
          timing += (name -> Timing(elapsed, cpu, Time.zero))

        case Session_Finished2(name,
            Value.Int(e1), Value.Int(e2), Value.Int(e3)) =>
          val elapsed = Time.hms(e1, e2, e3)
          timing += (name -> Timing(elapsed, Time.zero, Time.zero))

        case Session_Timing(name,
            Value.Int(t), Value.Double(e), Value.Double(c), Value.Double(g)) =>
          val elapsed = Time.seconds(e)
          val cpu = Time.seconds(c)
          val gc = Time.seconds(g)
          ml_timing += (name -> Timing(elapsed, cpu, gc))
          threads += (name -> t)

        case Heap(name, Value.Long(size)) =>
          heap_sizes += (name -> size)

        case _ if line.startsWith(ML_STATISTICS_MARKER) =>
          val (name, props) =
            Library.try_unprefix(ML_STATISTICS_MARKER, line).map(log_file.parse_props(_)) match {
              case Some((SESSION_NAME, session_name) :: props) => (session_name, props)
              case _ => log_file.err("malformed ML_statistics " + quote(line))
            }
          ml_statistics += (name -> (props :: ml_statistics.getOrElse(name, Nil)))

        case _ =>
      }
    }

    val sessions =
      Map(
        (for (name <- all_sessions.toList) yield {
          val status =
            if (failed(name)) Session_Status.FAILED
            else if (cancelled(name)) Session_Status.CANCELLED
            else if (timing.isDefinedAt(name) || ml_timing.isDefinedAt(name))
              Session_Status.FINISHED
            else if (started(name)) Session_Status.FAILED
            else Session_Status.EXISTING
          val entry =
            Session_Entry(
              chapter.getOrElse(name, ""),
              groups.getOrElse(name, Nil),
              threads.get(name),
              timing.getOrElse(name, Timing.zero),
              ml_timing.getOrElse(name, Timing.zero),
              ml_statistics.getOrElse(name, Nil).reverse,
              heap_sizes.get(name),
              status)
          (name -> entry)
        }):_*)
    Build_Info(sessions)
  }



  /** session info: produced by "isabelle build" **/

  sealed case class Session_Info(
    session_name: String,
    session_timing: Properties.T,
    command_timings: List[Properties.T],
    ml_statistics: List[Properties.T],
    task_statistics: List[Properties.T])

  private def parse_session_info(
    log_file: Log_File,
    default_name: String,
    command_timings: Boolean,
    ml_statistics: Boolean,
    task_statistics: Boolean): Session_Info =
  {
    val xml_cache = new XML.Cache()

    val session_name =
      log_file.find_line("\fSession.name = ") match {
        case None => default_name
        case Some(name) if default_name == "" || default_name == name => name
        case Some(name) => log_file.err("log from different session " + quote(name))
      }
    val session_timing = log_file.find_props("\fTiming = ") getOrElse Nil
    val command_timings_ =
      if (command_timings) log_file.filter_props("\fcommand_timing = ") else Nil
    val ml_statistics_ =
      if (ml_statistics) log_file.filter_props(ML_STATISTICS_MARKER) else Nil
    val task_statistics_ =
      if (task_statistics) log_file.filter_props("\ftask_statistics = ") else Nil

    Session_Info(session_name, session_timing, command_timings_, ml_statistics_, task_statistics_)
  }
}
