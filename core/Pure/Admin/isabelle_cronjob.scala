/*  Title:      Pure/Admin/isabelle_cronjob.scala
    Author:     Makarius

Main entry point for administrative cronjob at TUM.
*/

package isabelle


import scala.annotation.tailrec
import scala.collection.mutable


object Isabelle_Cronjob
{
  /* file-system state: owned by main cronjob */

  val main_dir = Path.explode("~/cronjob")
  val main_state_file = main_dir + Path.explode("run/main.state")
  val current_log = main_dir + Path.explode("run/main.log")  // owned by log service
  val cumulative_log = main_dir + Path.explode("log/main.log")  // owned by log service

  val isabelle_repos = main_dir + Path.explode("isabelle")
  val isabelle_repos_test = main_dir + Path.explode("isabelle-test")
  val afp_repos = main_dir + Path.explode("AFP")

  val isabelle_dev_source = "http://isabelle.in.tum.de/repos/isabelle"
  val isabelle_release_source = "http://bitbucket.org/isabelle_project/isabelle-release"
  val afp_source = "https://bitbucket.org/isa-afp/afp-devel"

  val release_snapshot = Path.explode("~/html-data/release_snapshot")



  /** particular tasks **/

  /* identify Isabelle + AFP repository snapshots and build release */

  private val build_release =
    Logger_Task("build_release", logger =>
      Isabelle_System.with_tmp_dir("isadist")(base_dir =>
        {
          val rev = Mercurial.repository(isabelle_repos).id()
          val afp_rev = Mercurial.setup_repository(afp_source, afp_repos).id()

          File.write(logger.log_dir + Build_Log.log_filename("isabelle_identify", logger.start_date),
            terminate_lines(
              List("isabelle_identify: " + Build_Log.print_date(logger.start_date),
                "",
                "Isabelle version: " + rev,
                "AFP version: " + afp_rev)))

          val new_snapshot = release_snapshot.ext("new")
          val old_snapshot = release_snapshot.ext("old")

          Isabelle_System.rm_tree(new_snapshot)
          Isabelle_System.rm_tree(old_snapshot)

          Build_Release.build_release(base_dir, rev = rev, afp_rev = afp_rev,
            parallel_jobs = 4, remote_mac = "macbroy31", website = Some(new_snapshot))

          if (release_snapshot.is_dir) File.move(release_snapshot, old_snapshot)
          File.move(new_snapshot, release_snapshot)
          Isabelle_System.rm_tree(old_snapshot)
        }))


  /* integrity test of build_history vs. build_history_base */

  private val build_history_base =
    Logger_Task("build_history_base", logger =>
      {
        val hg =
          Mercurial.setup_repository(
            File.standard_path(isabelle_repos), isabelle_repos_test)
        for {
          (result, log_path) <-
            Build_History.build_history(
              hg, rev = "build_history_base", fresh = true, build_args = List("HOL"))
        } {
          result.check
          File.move(log_path, logger.log_dir + log_path.base)
        }
      })


  /* remote build_history */

  sealed case class Remote_Build(
    host: String,
    user: String = "",
    port: Int = SSH.default_port,
    shared_home: Boolean = true,
    options: String = "",
    args: String = "")

  private val remote_builds =
    List(
      List(Remote_Build("lxbroy8",
        options = "-m32 -B -M1x2,2 -t polyml-test -e 'init_component /home/isabelle/contrib/polyml-test-7a7b742897e9'",
        args = "-N -g timing")),
      List(Remote_Build("lxbroy9", options = "-m32 -B -M1x2,2", args = "-N -g timing")),
      List(Remote_Build("lxbroy10", options = "-m32 -B -M1x4,2,4,6", args = "-N -g timing")),
      List(
        Remote_Build("macbroy2", options = "-m32 -M8", args = "-a"),
        Remote_Build("macbroy2", options = "-m32 -M8 -t quick_and_dirty", args = "-a -o quick_and_dirty"),
        Remote_Build("macbroy2", options = "-m32 -M8 -t skip_proofs", args = "-a -o skip_proofs")),
      List(Remote_Build("macbroy30", options = "-m32 -M2", args = "-a")),
      List(Remote_Build("macbroy31", options = "-m32 -M2", args = "-a")),
      List(
        Remote_Build("vmnipkow9", shared_home = false, options = "-m32 -M4", args = "-a"),
        Remote_Build("vmnipkow9", shared_home = false, options = "-m64 -M4", args = "-a")))

  private def remote_build_history(rev: String, r: Remote_Build): Logger_Task =
  {
    val task_name = "build_history-" + r.host
    Logger_Task(task_name, logger =>
      {
        using(logger.ssh_context.open_session(host = r.host, user = r.user, port = r.port))(
          ssh =>
            {
              val self_update = !r.shared_home
              val push_isabelle_home = self_update && Mercurial.is_repository(Path.explode("~~"))

              val (results, _) =
                Build_History.remote_build_history(ssh,
                  isabelle_repos,
                  isabelle_repos.ext(r.host),
                  isabelle_repos_source = isabelle_release_source,
                  self_update = self_update,
                  push_isabelle_home = push_isabelle_home,
                  options =
                    "-r " + Bash.string(rev) + " -N " + Bash.string(task_name) + " -f " + r.options,
                  args = "-o timeout=10800 " + r.args)

              for ((log_name, bytes) <- results) {
                logger.log(Date.now(), log_name)
                Bytes.write(logger.log_dir + Path.explode(log_name), bytes)
              }
            })
      })
  }



  /** task logging **/

  sealed case class Logger_Task(name: String = "", body: Logger => Unit)

  class Log_Service private[Isabelle_Cronjob](progress: Progress, val ssh_context: SSH.Context)
  {
    current_log.file.delete

    private val thread: Consumer_Thread[String] =
      Consumer_Thread.fork("cronjob: logger", daemon = true)(
        consume = (text: String) =>
          { // critical
            File.append(current_log, text + "\n")
            File.append(cumulative_log, text + "\n")
            progress.echo(text)
            true
          })

    def shutdown() { thread.shutdown() }

    val hostname = Isabelle_System.hostname()

    def log(date: Date, task_name: String, msg: String): Unit =
      if (task_name != "")
        thread.send(
          "[" + Build_Log.print_date(date) + ", " + hostname + ", " + task_name + "]: " + msg)

    def start_logger(start_date: Date, task_name: String): Logger =
      new Logger(this, start_date, task_name)

    def run_task(start_date: Date, task: Logger_Task)
    {
      val logger = start_logger(start_date, task.name)
      val res = Exn.capture { task.body(logger) }
      val end_date = Date.now()
      val err =
        res match {
          case Exn.Res(_) => None
          case Exn.Exn(exn) =>
            System.err.println("Exception trace for " + quote(task.name) + ":")
            exn.printStackTrace()
            val first_line = Library.split_lines(Exn.message(exn)).headOption getOrElse "exception"
            Some(first_line)
        }
      logger.log_end(end_date, err)
    }

    def fork_task(start_date: Date, task: Logger_Task): Task =
      new Task(task.name, run_task(start_date, task))
  }

  class Logger private[Isabelle_Cronjob](
    val log_service: Log_Service, val start_date: Date, val task_name: String)
  {
    def ssh_context: SSH.Context = log_service.ssh_context

    def log(date: Date, msg: String): Unit = log_service.log(date, task_name, msg)

    def log_end(end_date: Date, err: Option[String])
    {
      val elapsed_time = end_date.time - start_date.time
      val msg =
        (if (err.isEmpty) "finished" else "ERROR " + err.get) +
        (if (elapsed_time.seconds < 3.0) "" else " (" + elapsed_time.message_hms + " elapsed time)")
      log(end_date, msg)
    }

    val log_dir: Path = main_dir + Build_Log.log_subdir(start_date)

    Isabelle_System.mkdirs(log_dir)
    log(start_date, "started")
  }

  class Task private[Isabelle_Cronjob](name: String, body: => Unit)
  {
    private val future: Future[Unit] = Future.thread("cronjob: " + name) { body }
    def is_finished: Boolean = future.is_finished
  }



  /** cronjob **/

  def cronjob(progress: Progress, exclude_task: Set[String])
  {
    /* soft lock */

    val still_running =
      try { Some(File.read(main_state_file)) }
      catch { case ERROR(_) => None }

    still_running match {
      case None | Some("") =>
      case Some(running) =>
        error("Isabelle cronjob appears to be still running: " + running)
    }


    /* log service */

    val log_service = new Log_Service(progress, SSH.init_context(Options.init()))

    def run(start_date: Date, task: Logger_Task) { log_service.run_task(start_date, task) }

    def run_now(task: Logger_Task) { run(Date.now(), task) }


    /* structured tasks */

    def SEQ(tasks: List[Logger_Task]): Logger_Task = Logger_Task(body = _ =>
      for (task <- tasks.iterator if !exclude_task(task.name) || task.name == "")
        run_now(task))

    def PAR(tasks: List[Logger_Task]): Logger_Task = Logger_Task(body = _ =>
      {
        @tailrec def join(running: List[Task])
        {
          running.partition(_.is_finished) match {
            case (Nil, Nil) =>
            case (Nil, _ :: _) => Thread.sleep(500); join(running)
            case (_ :: _, remaining) => join(remaining)
          }
        }
        val start_date = Date.now()
        val running =
          for (task <- tasks if !exclude_task(task.name))
            yield log_service.fork_task(start_date, task)
        join(running)
      })


    /* main */

    val main_start_date = Date.now()
    File.write(main_state_file, main_start_date + " " + log_service.hostname)

    val rev = Mercurial.repository(isabelle_repos).id()

    run(main_start_date,
      Logger_Task("isabelle_cronjob", _ =>
        run_now(
          SEQ(List(build_release, build_history_base,
            PAR(remote_builds.map(seq => SEQ(seq.map(remote_build_history(rev, _))))))))))

    log_service.shutdown()

    main_state_file.file.delete
  }



  /** command line entry point **/

  def main(args: Array[String])
  {
    Command_Line.tool0 {
      var force = false
      var verbose = false
      var exclude_task = Set.empty[String]

      val getopts = Getopts("""
Usage: Admin/cronjob/main [OPTIONS]

  Options are:
    -f           apply force to do anything
    -v           verbose
    -x NAME      exclude tasks with this name
""",
        "f" -> (_ => force = true),
        "v" -> (_ => verbose = true),
        "x:" -> (arg => exclude_task += arg))

      val more_args = getopts(args)
      if (more_args.nonEmpty) getopts.usage()

      val progress = if (verbose) new Console_Progress() else Ignore_Progress

      if (force) cronjob(progress, exclude_task)
      else error("Need to apply force to do anything")
    }
  }
}
