/*  Title:      Pure/PIDE/batch_session.scala
    Author:     Makarius

PIDE session in batch mode.
*/

package isabelle


import isabelle._


object Batch_Session
{
  def batch_session(
    options: Options,
    verbose: Boolean = false,
    dirs: List[Path] = Nil,
    session: String): Batch_Session =
  {
    val (_, session_tree) =
      Build.find_sessions(options, dirs).selection(sessions = List(session))
    val session_info = session_tree(session)
    val parent_session =
      session_info.parent getOrElse error("No parent session for " + quote(session))

    if (Build.build(options, new Build.Console_Progress(verbose),
        verbose = verbose, build_heap = true,
        dirs = dirs, sessions = List(parent_session)) != 0)
      new RuntimeException

    val deps = Build.dependencies(verbose = verbose, tree = session_tree)
    val resources =
    {
      val content = deps(parent_session)
      new Resources(content.loaded_theories, content.known_theories, content.syntax)
    }

    val progress = new Build.Console_Progress(verbose)

    val prover_session = new Session(resources)
    val batch_session = new Batch_Session(prover_session)

    val handler = new Build.Handler(progress, session)

    prover_session.phase_changed +=
      Session.Consumer[Session.Phase](getClass.getName) {
        case Session.Ready =>
          prover_session.add_protocol_handler(handler)
          val master_dir = session_info.dir
          val theories = session_info.theories.map({ case (_, opts, thys) => (opts, thys) })
          batch_session.build_theories_result =
            Some(Build.build_theories(prover_session, master_dir, theories))
        case Session.Inactive | Session.Failed =>
          batch_session.session_result.fulfill_result(Exn.Exn(ERROR("Prover process terminated")))
        case Session.Shutdown =>
          batch_session.session_result.fulfill(())
        case _ =>
      }

    prover_session.start("Isabelle", List("-r", "-q", parent_session))

    batch_session
  }
}

class Batch_Session private(val session: Session)
{
  val session_result = Future.promise[Unit]
  @volatile var build_theories_result: Option[Promise[XML.Body]] = None
}

