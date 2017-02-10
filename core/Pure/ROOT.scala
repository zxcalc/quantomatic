/*  Title:      Pure/ROOT.scala
    Author:     Makarius

Root of isabelle package.
*/

package object isabelle
{
  val ERROR = Exn.ERROR
  val error = Exn.error _
  val cat_error = Exn.cat_error _

  def using[A <: { def close() }, B](x: A)(f: A => B): B = Library.using(x)(f)
  val space_explode = Library.space_explode _
  val split_lines = Library.split_lines _
  val cat_lines = Library.cat_lines _
  val terminate_lines = Library.terminate_lines _
  val quote = Library.quote _
  val commas = Library.commas _
  val commas_quote = Library.commas_quote _
}
