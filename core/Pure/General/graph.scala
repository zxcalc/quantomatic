/*  Title:      Pure/General/graph.scala
    Module:     PIDE
    Author:     Makarius

Directed graphs.
*/

package isabelle


import scala.collection.immutable.{SortedMap, SortedSet}
import scala.annotation.tailrec


object Graph
{
  class Duplicate[Key](val key: Key) extends Exception
  class Undefined[Key](val key: Key) extends Exception
  class Cycles[Key](val cycles: List[List[Key]]) extends Exception

  def empty[Key, A](implicit ord: Ordering[Key]): Graph[Key, A] =
    new Graph[Key, A](SortedMap.empty(ord))

  def make[Key, A](entries: List[((Key, A), List[Key])])(implicit ord: Ordering[Key])
    : Graph[Key, A] =
  {
    val graph1 =
      (empty[Key, A](ord) /: entries) { case (graph, ((x, info), _)) => graph.new_node(x, info) }
    val graph2 =
      (graph1 /: entries) { case (graph, ((x, _), ys)) => (graph /: ys)(_.add_edge(x, _)) }
    graph2
  }

  def string[A]: Graph[String, A] = empty(Ordering.String)
  def int[A]: Graph[Int, A] = empty(Ordering.Int)
  def long[A]: Graph[Long, A] = empty(Ordering.Long)


  /* XML data representation */

  def encode[Key, A](key: XML.Encode.T[Key], info: XML.Encode.T[A]): XML.Encode.T[Graph[Key, A]] =
    ((graph: Graph[Key, A]) => {
      import XML.Encode._
      list(pair(pair(key, info), list(key)))(graph.dest)
    })

  def decode[Key, A](key: XML.Decode.T[Key], info: XML.Decode.T[A])(implicit ord: Ordering[Key])
      : XML.Decode.T[Graph[Key, A]] =
    ((body: XML.Body) => {
      import XML.Decode._
      make(list(pair(pair(key, info), list(key)))(body))(ord)
    })
}


final class Graph[Key, A] private(rep: SortedMap[Key, (A, (SortedSet[Key], SortedSet[Key]))])
{
  type Keys = SortedSet[Key]
  type Entry = (A, (Keys, Keys))

  def ordering: Ordering[Key] = rep.ordering
  def empty_keys: Keys = SortedSet.empty[Key](ordering)


  /* graphs */

  def is_empty: Boolean = rep.isEmpty
  def defined(x: Key): Boolean = rep.isDefinedAt(x)

  def iterator: Iterator[(Key, Entry)] = rep.iterator

  def keys_iterator: Iterator[Key] = iterator.map(_._1)
  def keys: List[Key] = keys_iterator.toList

  def dest: List[((Key, A), List[Key])] =
    (for ((x, (i, (_, succs))) <- iterator) yield ((x, i), succs.toList)).toList

  override def toString: String =
    dest.map({ case ((x, _), ys) =>
        x.toString + " -> " + ys.iterator.map(_.toString).mkString("{", ", ", "}") })
      .mkString("Graph(", ", ", ")")

  private def get_entry(x: Key): Entry =
    rep.get(x) match {
      case Some(entry) => entry
      case None => throw new Graph.Undefined(x)
    }

  private def map_entry(x: Key, f: Entry => Entry): Graph[Key, A] =
    new Graph[Key, A](rep + (x -> f(get_entry(x))))


  /* nodes */

  def get_node(x: Key): A = get_entry(x)._1

  def map_node(x: Key, f: A => A): Graph[Key, A] =
    map_entry(x, { case (i, ps) => (f(i), ps) })


  /* reachability */

  /*nodes reachable from xs -- topologically sorted for acyclic graphs*/
  def reachable(next: Key => Keys, xs: List[Key]): (List[List[Key]], Keys) =
  {
    def reach(x: Key, reached: (List[Key], Keys)): (List[Key], Keys) =
    {
      val (rs, r_set) = reached
      if (r_set(x)) reached
      else {
        val (rs1, r_set1) = (next(x) :\ (rs, r_set + x))(reach)
        (x :: rs1, r_set1)
      }
    }
    def reachs(reached: (List[List[Key]], Keys), x: Key): (List[List[Key]], Keys) =
    {
      val (rss, r_set) = reached
      val (rs, r_set1) = reach(x, (Nil, r_set))
      (rs :: rss, r_set1)
    }
    ((List.empty[List[Key]], empty_keys) /: xs)(reachs)
  }

  /*immediate*/
  def imm_preds(x: Key): Keys = get_entry(x)._2._1
  def imm_succs(x: Key): Keys = get_entry(x)._2._2

  /*transitive*/
  def all_preds(xs: List[Key]): List[Key] = reachable(imm_preds, xs)._1.flatten
  def all_succs(xs: List[Key]): List[Key] = reachable(imm_succs, xs)._1.flatten

  /*strongly connected components; see: David King and John Launchbury,
    "Structuring Depth First Search Algorithms in Haskell"*/
  def strong_conn: List[List[Key]] =
    reachable(imm_preds, all_succs(keys))._1.filterNot(_.isEmpty).reverse


  /* minimal and maximal elements */

  def minimals: List[Key] =
    (List.empty[Key] /: rep) {
      case (ms, (m, (_, (preds, _)))) => if (preds.isEmpty) m :: ms else ms }

  def maximals: List[Key] =
    (List.empty[Key] /: rep) {
      case (ms, (m, (_, (_, succs)))) => if (succs.isEmpty) m :: ms else ms }

  def is_minimal(x: Key): Boolean = imm_preds(x).isEmpty
  def is_maximal(x: Key): Boolean = imm_succs(x).isEmpty


  /* node operations */

  def new_node(x: Key, info: A): Graph[Key, A] =
  {
    if (defined(x)) throw new Graph.Duplicate(x)
    else new Graph[Key, A](rep + (x -> (info, (empty_keys, empty_keys))))
  }

  def default_node(x: Key, info: A): Graph[Key, A] =
    if (defined(x)) this else new_node(x, info)

  private def del_adjacent(fst: Boolean, x: Key)(map: SortedMap[Key, Entry], y: Key)
      : SortedMap[Key, Entry] =
    map.get(y) match {
      case None => map
      case Some((i, (preds, succs))) =>
        map + (y -> (i, if (fst) (preds - x, succs) else (preds, succs - x)))
    }

  def del_node(x: Key): Graph[Key, A] =
  {
    val (preds, succs) = get_entry(x)._2
    new Graph[Key, A](
      (((rep - x) /: preds)(del_adjacent(false, x)) /: succs)(del_adjacent(true, x)))
  }

  def restrict(pred: Key => Boolean): Graph[Key, A] =
    (this /: iterator){ case (graph, (x, _)) => if (!pred(x)) graph.del_node(x) else graph }


  /* edge operations */

  def is_edge(x: Key, y: Key): Boolean =
    defined(x) && defined(y) && imm_succs(x)(y)

  def add_edge(x: Key, y: Key): Graph[Key, A] =
    if (is_edge(x, y)) this
    else
      map_entry(y, { case (i, (preds, succs)) => (i, (preds + x, succs)) }).
      map_entry(x, { case (i, (preds, succs)) => (i, (preds, succs + y)) })

  def del_edge(x: Key, y: Key): Graph[Key, A] =
    if (is_edge(x, y))
      map_entry(y, { case (i, (preds, succs)) => (i, (preds - x, succs)) }).
      map_entry(x, { case (i, (preds, succs)) => (i, (preds, succs - y)) })
    else this


  /* irreducible paths -- Hasse diagram */

  private def irreducible_preds(x_set: Keys, path: List[Key], z: Key): List[Key] =
  {
    def red(x: Key)(x1: Key) = is_edge(x, x1) && x1 != z
    @tailrec def irreds(xs0: List[Key], xs1: List[Key]): List[Key] =
      xs0 match {
        case Nil => xs1
        case x :: xs =>
          if (!(x_set(x)) || x == z || path.contains(x) ||
              xs.exists(red(x)) || xs1.exists(red(x)))
            irreds(xs, xs1)
          else irreds(xs, x :: xs1)
      }
    irreds(imm_preds(z).toList, Nil)
  }

  def irreducible_paths(x: Key, y: Key): List[List[Key]] =
  {
    val (_, x_set) = reachable(imm_succs, List(x))
    def paths(path: List[Key])(ps: List[List[Key]], z: Key): List[List[Key]] =
      if (x == z) (z :: path) :: ps
      else (ps /: irreducible_preds(x_set, path, z))(paths(z :: path))
    if ((x == y) && !is_edge(x, x)) List(Nil) else paths(Nil)(Nil, y)
  }


  /* transitive closure and reduction */

  private def transitive_step(z: Key): Graph[Key, A] =
  {
    val (preds, succs) = get_entry(z)._2
    var graph = this
    for (x <- preds; y <- succs) graph = graph.add_edge(x, y)
    graph
  }

  def transitive_closure: Graph[Key, A] = (this /: keys_iterator)(_.transitive_step(_))

  def transitive_reduction_acyclic: Graph[Key, A] =
  {
    val trans = this.transitive_closure
    if (trans.iterator.exists({ case (x, (_, (_, succs))) => succs.contains(x) }))
      error("Cyclic graph")

    var graph = this
    for {
      (x, (_, (_, succs))) <- iterator
      y <- succs
      if trans.imm_preds(y).exists(z => trans.is_edge(x, z))
    } graph = graph.del_edge(x, y)
    graph
  }


  /* maintain acyclic graphs */

  def add_edge_acyclic(x: Key, y: Key): Graph[Key, A] =
    if (is_edge(x, y)) this
    else {
      irreducible_paths(y, x) match {
        case Nil => add_edge(x, y)
        case cycles => throw new Graph.Cycles(cycles.map(x :: _))
      }
    }

  def add_deps_acyclic(y: Key, xs: List[Key]): Graph[Key, A] =
    (this /: xs)(_.add_edge_acyclic(_, y))

  def topological_order: List[Key] = all_succs(minimals)
}
