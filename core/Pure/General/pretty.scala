/*  Title:      Pure/General/pretty.scala
    Author:     Makarius

Generic pretty printing module.
*/

package isabelle


object Pretty
{
  /* spaces */

  val space = " "

  private val static_spaces = space * 4000

  def spaces(k: Int): String =
  {
    require(k >= 0)
    if (k < static_spaces.length) static_spaces.substring(0, k)
    else space * k
  }


  /* text metric -- standardized to width of space */

  abstract class Metric
  {
    val unit: Double
    def apply(s: String): Double
  }

  object Metric_Default extends Metric
  {
    val unit = 1.0
    def apply(s: String): Double = s.length.toDouble
  }


  /* markup trees with physical blocks and breaks */

  def block(body: XML.Body): XML.Tree = Block(2, body)

  object Block
  {
    def apply(i: Int, body: XML.Body): XML.Tree =
      XML.Elem(Markup.Block(i), body)

    def unapply(tree: XML.Tree): Option[(Int, XML.Body)] =
      tree match {
        case XML.Elem(Markup.Block(i), body) => Some((i, body))
        case _ => None
      }
  }

  object Break
  {
    def apply(w: Int): XML.Tree =
      XML.Elem(Markup.Break(w), List(XML.Text(spaces(w))))

    def unapply(tree: XML.Tree): Option[Int] =
      tree match {
        case XML.Elem(Markup.Break(w), _) => Some(w)
        case _ => None
      }
  }

  val FBreak = XML.Text("\n")

  def item(body: XML.Body): XML.Tree =
    Block(2, XML.elem(Markup.BULLET, List(XML.Text(space))) :: XML.Text(space) :: body)

  val Separator = List(XML.elem(Markup.SEPARATOR, List(XML.Text(space))), FBreak)
  def separate(ts: List[XML.Tree]): XML.Body = Library.separate(Separator, ts.map(List(_))).flatten


  /* standard form */

  def standard_form(body: XML.Body): XML.Body =
    body flatMap {
      case XML.Wrapped_Elem(markup, body1, body2) =>
        List(XML.Wrapped_Elem(markup, body1, standard_form(body2)))
      case XML.Elem(markup, body) =>
        if (markup.name == Markup.ITEM) List(item(standard_form(body)))
        else List(XML.Elem(markup, standard_form(body)))
      case XML.Text(text) => Library.separate(FBreak, split_lines(text).map(XML.Text))
    }


  /* formatted output */

  private val margin_default = 76.0

  def formatted(input: XML.Body, margin: Double = margin_default,
    metric: Metric = Metric_Default): XML.Body =
  {
    sealed case class Text(tx: XML.Body = Nil, pos: Double = 0.0, nl: Int = 0)
    {
      def newline: Text = copy(tx = FBreak :: tx, pos = 0.0, nl = nl + 1)
      def string(s: String): Text = copy(tx = XML.Text(s) :: tx, pos = pos + metric(s))
      def blanks(wd: Int): Text = string(spaces(wd))
      def content: XML.Body = tx.reverse
    }

    val breakgain = margin / 20
    val emergencypos = (margin / 2).round.toInt

    def content_length(tree: XML.Tree): Double =
      XML.traverse_text(List(tree))(0.0)(_ + metric(_))

    def breakdist(trees: XML.Body, after: Double): Double =
      trees match {
        case Break(_) :: _ => 0.0
        case FBreak :: _ => 0.0
        case t :: ts => content_length(t) + breakdist(ts, after)
        case Nil => after
      }

    def forcenext(trees: XML.Body): XML.Body =
      trees match {
        case Nil => Nil
        case FBreak :: _ => trees
        case Break(_) :: ts => FBreak :: ts
        case t :: ts => t :: forcenext(ts)
      }

    def format(trees: XML.Body, blockin: Int, after: Double, text: Text): Text =
      trees match {
        case Nil => text

        case Block(indent, body) :: ts =>
          val pos1 = (text.pos + indent).ceil.toInt
          val pos2 = pos1 % emergencypos
          val blockin1 =
            if (pos1 < emergencypos) pos1
            else pos2
          val btext = format(body, blockin1, breakdist(ts, after), text)
          val ts1 = if (text.nl < btext.nl) forcenext(ts) else ts
          format(ts1, blockin, after, btext)

        case Break(wd) :: ts =>
          if (text.pos + wd <= ((margin - breakdist(ts, after)) max (blockin + breakgain)))
            format(ts, blockin, after, text.blanks(wd))
          else format(ts, blockin, after, text.newline.blanks(blockin))
        case FBreak :: ts => format(ts, blockin, after, text.newline.blanks(blockin))

        case XML.Wrapped_Elem(markup, body1, body2) :: ts =>
          val btext = format(body2, blockin, breakdist(ts, after), text.copy(tx = Nil))
          val ts1 = if (text.nl < btext.nl) forcenext(ts) else ts
          val btext1 = btext.copy(tx = XML.Wrapped_Elem(markup, body1, btext.content) :: text.tx)
          format(ts1, blockin, after, btext1)

        case XML.Elem(markup, body) :: ts =>
          val btext = format(body, blockin, breakdist(ts, after), text.copy(tx = Nil))
          val ts1 = if (text.nl < btext.nl) forcenext(ts) else ts
          val btext1 = btext.copy(tx = XML.Elem(markup, btext.content) :: text.tx)
          format(ts1, blockin, after, btext1)

        case XML.Text(s) :: ts => format(ts, blockin, after, text.string(s))
      }

    format(standard_form(input), 0, 0.0, Text()).content
  }

  def string_of(input: XML.Body, margin: Double = margin_default,
      metric: Metric = Metric_Default): String =
    XML.content(formatted(input, margin, metric))


  /* unformatted output */

  def unformatted(input: XML.Body): XML.Body =
  {
    def fmt(tree: XML.Tree): XML.Body =
      tree match {
        case Block(_, body) => body.flatMap(fmt)
        case Break(wd) => List(XML.Text(spaces(wd)))
        case FBreak => List(XML.Text(space))
        case XML.Wrapped_Elem(markup, body1, body2) =>
          List(XML.Wrapped_Elem(markup, body1, body2.flatMap(fmt)))
        case XML.Elem(markup, body) => List(XML.Elem(markup, body.flatMap(fmt)))
        case XML.Text(_) => List(tree)
      }
    standard_form(input).flatMap(fmt)
  }

  def str_of(input: XML.Body): String = XML.content(unformatted(input))
}
