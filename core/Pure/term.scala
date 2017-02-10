/*  Title:      Pure/term.scala
    Author:     Makarius

Lambda terms, types, sorts.

Note: Isabelle/ML is the primary environment for logical operations.
*/

package isabelle


object Term
{
  type Indexname = (String, Int)

  type Sort = List[String]
  val dummyS: Sort = List("")

  sealed abstract class Typ
  case class Type(name: String, args: List[Typ] = Nil) extends Typ
  case class TFree(name: String, sort: Sort = dummyS) extends Typ
  case class TVar(name: Indexname, sort: Sort = dummyS) extends Typ
  val dummyT = Type("dummy")

  sealed abstract class Term
  case class Const(name: String, typ: Typ = dummyT) extends Term
  case class Free(name: String, typ: Typ = dummyT) extends Term
  case class Var(name: Indexname, typ: Typ = dummyT) extends Term
  case class Bound(index: Int) extends Term
  case class Abs(name: String, typ: Typ = dummyT, body: Term) extends Term
  case class App(fun: Term, arg: Term) extends Term
}

