/*  Title:      Pure/System/numa.scala
    Author:     Makarius

Support for Non-Uniform Memory Access of separate CPU nodes.
*/

package isabelle


object NUMA
{
  /* available nodes */

  def nodes(): List[Int] =
  {
    val numa_nodes_linux = Path.explode("/sys/devices/system/node/online")

    val Single = """^(\d+)$""".r
    val Multiple = """^(\d+)-(\d+)$""".r

    def read(s: String): List[Int] =
      s match {
        case Single(Value.Int(i)) => List(i)
        case Multiple(Value.Int(i), Value.Int(j)) => (i to j).toList
        case _ => error("Cannot parse CPU node specification: " + quote(s))
      }

    if (numa_nodes_linux.is_file) {
      Library.space_explode(',', File.read(numa_nodes_linux).trim).flatMap(read(_))
    }
    else Nil
  }


  /* CPU policy via numactl tool */

  lazy val numactl_available: Boolean = Isabelle_System.bash("numactl --hardware").ok

  def policy(node: Int): String =
    if (numactl_available) "numactl -m" + node + " -N" + node else ""


  /* shuffling of CPU nodes */

  def enabled_warning(enabled: Boolean): Boolean =
  {
    def warning =
      if (nodes().length < 2) Some("no NUMA nodes available")
      else if (!numactl_available) Some("missing numactl tool")
      else None

    enabled &&
      (warning match {
        case Some(s) => Output.warning("Shuffling of CPU nodes is disabled: " + s); false
        case _ => true
      })
  }

  class Nodes(enabled: Boolean = true)
  {
    private val available = nodes().zipWithIndex
    private var next_index = 0

    def next(used: Int => Boolean = _ => false): Option[Int] = synchronized {
      if (!enabled || available.isEmpty) None
      else {
        val candidates = available.drop(next_index) ::: available.take(next_index)
        val (n, i) =
          candidates.find({ case (n, i) => i == next_index && !used(n) }) orElse
            candidates.find({ case (n, _) => !used(n) }) getOrElse candidates.head
        next_index = (i + 1) % available.length
        Some(n)
      }
    }
  }
}
