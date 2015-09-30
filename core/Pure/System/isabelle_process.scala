/*  Title:      Pure/System/isabelle_process.scala
    Author:     Makarius

Isabelle process wrapper.
*/

package isabelle


object Isabelle_Process
{
  def apply(
    receiver: Prover.Message => Unit = Console.println(_),
    prover_args: List[String] = Nil): Isabelle_Process =
  {
    val system_channel = System_Channel()
    val system_process =
      try {
        val cmdline =
          Isabelle_System.getenv_strict("ISABELLE_PROCESS") ::
            (system_channel.prover_args ::: prover_args)
        val process =
          new Isabelle_System.Managed_Process(null, null, false, cmdline: _*) with
            Prover.System_Process
        process.stdin.close
        process
      }
      catch { case exn @ ERROR(_) => system_channel.accepted(); throw exn }

    new Isabelle_Process(receiver, system_channel, system_process)
  }
}

class Isabelle_Process private(
    receiver: Prover.Message => Unit,
    system_channel: System_Channel,
    system_process: Prover.System_Process)
  extends Prover(receiver, system_channel, system_process)
  {
    def encode(s: String): String = Symbol.encode(s)
    def decode(s: String): String = Symbol.decode(s)
  }

