/*  Title:      Pure/General/graph_display.scala
    Author:     Makarius

Support for graph display.
*/

package isabelle

object Graph_Display
{
  /* graph entries */

  type Entry = ((String, (String, XML.Body)), List[String])  // ident, name, content, parents


  /* graph structure */

  object Node
  {
    val dummy: Node = Node("", "")

    object Ordering extends scala.math.Ordering[Node]
    {
      def compare(node1: Node, node2: Node): Int =
        node1.name compare node2.name match {
          case 0 => node1.ident compare node2.ident
          case ord => ord
        }
    }
  }
  sealed case class Node(name: String, ident: String)
  {
    def is_dummy: Boolean = this == Node.dummy
    override def toString: String = name
  }

  type Edge = (Node, Node)

  type Graph = isabelle.Graph[Node, XML.Body]

  val empty_graph: Graph = isabelle.Graph.empty(Node.Ordering)

  def build_graph(entries: List[Entry]): Graph =
  {
    val node =
      (Map.empty[String, Node] /: entries) {
        case (m, ((ident, (name, _)), _)) => m + (ident -> Node(name, ident))
      }
    (((empty_graph /: entries) {
        case (g, ((ident, (_, content)), _)) => g.new_node(node(ident), content)
      }) /: entries) {
        case (g1, ((ident, _), parents)) =>
          (g1 /: parents) { case (g2, parent) => g2.add_edge(node(parent), node(ident)) }
      }
  }

  def decode_graph(body: XML.Body): Graph =
    build_graph(
      {
        import XML.Decode._
        list(pair(pair(string, pair(string, x => x)), list(string)))(body)
      })
}

