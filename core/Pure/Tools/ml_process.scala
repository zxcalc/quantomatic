/*  Title:      Pure/Tools/ml_process.scala
    Author:     Makarius

The raw ML process.
*/

package isabelle


import java.io.{File => JFile}


object ML_Process
{
  def apply(options: Options,
    logic: String = "",
    args: List[String] = Nil,
    dirs: List[Path] = Nil,
    modes: List[String] = Nil,
    raw_ml_system: Boolean = false,
    cwd: JFile = null,
    env: Map[String, String] = Isabelle_System.settings(),
    redirect: Boolean = false,
    cleanup: () => Unit = () => (),
    channel: Option[System_Channel] = None,
    tree: Option[Sessions.Tree] = None,
    store: Sessions.Store = Sessions.store()): Bash.Process =
  {
    val logic_name = Isabelle_System.default_logic(logic)
    val heaps: List[String] =
      if (raw_ml_system) Nil
      else {
        val (_, session_tree) =
          tree.getOrElse(Sessions.load(options, dirs)).selection(sessions = List(logic_name))
        (session_tree.ancestors(logic_name) ::: List(logic_name)).
          map(a => File.platform_path(store.heap(a)))
      }

    val eval_init =
      if (heaps.isEmpty) {
        List(
          """
          fun chapter (_: string) = ();
          fun section (_: string) = ();
          fun subsection (_: string) = ();
          fun subsubsection (_: string) = ();
          fun paragraph (_: string) = ();
          fun subparagraph (_: string) = ();
          val ML_file = PolyML.use;
          """,
          if (Isabelle_System.getenv("ML_SYSTEM") == "polyml-5.6") "structure FixedInt = IntInf"
          else "",
          if (Platform.is_windows)
            "fun exit 0 = OS.Process.exit OS.Process.success" +
            " | exit 1 = OS.Process.exit OS.Process.failure" +
            " | exit rc = OS.Process.exit (RunCall.unsafeCast (Word8.fromInt rc))"
          else
            "fun exit rc = Posix.Process.exit (Word8.fromInt rc)",
          "PolyML.Compiler.prompt1 := \"Poly/ML> \"",
          "PolyML.Compiler.prompt2 := \"Poly/ML# \"")
      }
      else
        List(
          "(PolyML.SaveState.loadHierarchy " +
            ML_Syntax.print_list(ML_Syntax.print_string0)(heaps) +
          "; PolyML.print_depth 0) handle exn => (TextIO.output (TextIO.stdErr, General.exnMessage exn ^ " +
          ML_Syntax.print_string0(": " + logic_name + "\n") +
          "); OS.Process.exit OS.Process.failure)")

    val eval_modes =
      if (modes.isEmpty) Nil
      else List("Print_Mode.add_modes " + ML_Syntax.print_list(ML_Syntax.print_string0)(modes))

    // options
    val isabelle_process_options = Isabelle_System.tmp_file("options")
    File.write(isabelle_process_options, YXML.string_of_body(options.encode))
    val env_options = Map("ISABELLE_PROCESS_OPTIONS" -> File.standard_path(isabelle_process_options))
    val eval_options = if (heaps.isEmpty) Nil else List("Options.load_default ()")

    val eval_process =
      if (heaps.isEmpty)
        List("PolyML.print_depth " + ML_Syntax.print_int(options.int("ML_print_depth")))
      else
        channel match {
          case None =>
            List("Isabelle_Process.init_options ()")
          case Some(ch) =>
            List("Isabelle_Process.init_protocol " + ML_Syntax.print_string0(ch.server_name))
        }

    // ISABELLE_TMP
    val isabelle_tmp = Isabelle_System.tmp_dir("process")
    val env_tmp = Map("ISABELLE_TMP" -> File.standard_path(isabelle_tmp))

    val ml_runtime_options =
    {
      val ml_options = Word.explode(Isabelle_System.getenv("ML_OPTIONS"))
      if (ml_options.exists(_.containsSlice("gcthreads"))) ml_options
      else ml_options ::: List("--gcthreads", options.int("threads").toString)
    }

    // bash
    val bash_args =
      ml_runtime_options :::
      (eval_init ::: eval_modes ::: eval_options ::: eval_process).
        map(eval => List("--eval", eval)).flatten ::: args

    Bash.process(
      "exec " + options.string("ML_process_policy") + """ "$ML_HOME/poly" -q """ +
        Bash.strings(bash_args),
      cwd = cwd,
      env =
        Isabelle_System.library_path(env ++ env_options ++ env_tmp,
          Isabelle_System.getenv_strict("ML_HOME")),
      redirect = redirect,
      cleanup = () =>
        {
          isabelle_process_options.delete
          Isabelle_System.rm_tree(isabelle_tmp)
          cleanup()
        })
  }


  /* Isabelle tool wrapper */

  val isabelle_tool = Isabelle_Tool("process", "raw ML process (batch mode)", args =>
  {
    var dirs: List[Path] = Nil
    var eval_args: List[String] = Nil
    var logic = Isabelle_System.getenv("ISABELLE_LOGIC")
    var modes: List[String] = Nil
    var options = Options.init()

    val getopts = Getopts("""
Usage: isabelle process [OPTIONS]

  Options are:
    -T THEORY    load theory
    -d DIR       include session directory
    -e ML_EXPR   evaluate ML expression on startup
    -f ML_FILE   evaluate ML file on startup
    -l NAME      logic session name (default ISABELLE_LOGIC=""" + quote(logic) + """)
    -m MODE      add print mode for output
    -o OPTION    override Isabelle system OPTION (via NAME=VAL or NAME)

  Run the raw Isabelle ML process in batch mode.
""",
      "T:" -> (arg =>
        eval_args = eval_args ::: List("--eval", "use_thy " + ML_Syntax.print_string0(arg))),
      "d:" -> (arg => dirs = dirs ::: List(Path.explode(arg))),
      "e:" -> (arg => eval_args = eval_args ::: List("--eval", arg)),
      "f:" -> (arg => eval_args = eval_args ::: List("--use", arg)),
      "l:" -> (arg => logic = arg),
      "m:" -> (arg => modes = arg :: modes),
      "o:" -> (arg => options = options + arg))

    val more_args = getopts(args)
    if (args.isEmpty || !more_args.isEmpty) getopts.usage()

    val rc = ML_Process(options, logic = logic, args = eval_args, dirs = dirs, modes = modes).
      result().print_stdout.rc
    sys.exit(rc)
  })
}
