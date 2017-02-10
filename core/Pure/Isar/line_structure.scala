/*  Title:      Pure/Isar/line_structure.scala
    Author:     Makarius

Line-oriented document structure, e.g. for fold handling.
*/

package isabelle


object Line_Structure
{
  val init = Line_Structure()
}

sealed case class Line_Structure(
  improper: Boolean = true,
  command: Boolean = false,
  depth: Int = 0,
  span_depth: Int = 0,
  after_span_depth: Int = 0,
  element_depth: Int = 0)
{
  def update(keywords: Keyword.Keywords, tokens: List[Token]): Line_Structure =
  {
    val improper1 = tokens.forall(_.is_improper)
    val command1 = tokens.exists(_.is_begin_or_command)

    val command_depth =
      tokens.iterator.filter(_.is_proper).toStream.headOption match {
        case Some(tok) =>
          if (keywords.is_command(tok, Keyword.close_structure))
            Some(after_span_depth - 1)
          else None
        case None => None
      }

    val depth1 =
      if (tokens.exists(tok =>
            keywords.is_before_command(tok) ||
            !tok.is_end && keywords.is_command(tok, Keyword.theory))) element_depth
      else if (command_depth.isDefined) command_depth.get
      else if (command1) after_span_depth
      else span_depth

    val (span_depth1, after_span_depth1, element_depth1) =
      ((span_depth, after_span_depth, element_depth) /: tokens) {
        case (depths @ (x, y, z), tok) =>
          if (tok.is_begin) (z + 2, z + 1, z + 1)
          else if (tok.is_end) (z + 1, z - 1, z - 1)
          else if (tok.is_command) {
            val depth0 = element_depth
            if (keywords.is_command(tok, Keyword.theory_goal)) (depth0 + 2, depth0 + 1, z)
            else if (keywords.is_command(tok, Keyword.theory)) (depth0 + 1, depth0, z)
            else if (keywords.is_command(tok, Keyword.proof_open)) (y + 2, y + 1, z)
            else if (keywords.is_command(tok, Set(Keyword.PRF_BLOCK))) (y + 2, y + 1, z)
            else if (keywords.is_command(tok, Set(Keyword.QED_BLOCK))) (y - 1, y - 2, z)
            else if (keywords.is_command(tok, Set(Keyword.PRF_CLOSE))) (y, y - 1, z)
            else if (keywords.is_command(tok, Keyword.proof_close)) (y + 1, y - 1, z)
            else if (keywords.is_command(tok, Keyword.qed_global)) (depth0 + 1, depth0, z)
            else depths
          }
          else depths
      }

    Line_Structure(improper1, command1, depth1, span_depth1, after_span_depth1, element_depth1)
  }
}
