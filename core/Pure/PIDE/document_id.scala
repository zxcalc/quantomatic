/*  Title:      Pure/PIDE/document_id.scala
    Module:     PIDE
    Author:     Makarius

Unique identifiers for document structure.

NB: ML ticks forwards > 0, JVM ticks backwards < 0.
*/

package isabelle


object Document_ID
{
  type Generic = Long
  type Version = Generic
  type Command = Generic
  type Exec = Generic

  val none: Generic = 0
  val make = Counter.make()

  def apply(id: Generic): String = Properties.Value.Long.apply(id)
  def unapply(s: String): Option[Generic] = Properties.Value.Long.unapply(s)
}

