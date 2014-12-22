/*  Title:      Pure/General/symbol.scala
    Author:     Makarius

Detecting and recoding Isabelle symbols.
*/

package isabelle


import scala.collection.mutable
import scala.util.matching.Regex
import scala.annotation.tailrec


object Symbol
{
  type Symbol = String

  // counting Isabelle symbols, starting from 1
  type Offset = Text.Offset
  type Range = Text.Range


  /* ASCII characters */

  def is_ascii_letter(c: Char): Boolean = 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z'

  def is_ascii_digit(c: Char): Boolean = '0' <= c && c <= '9'

  def is_ascii_hex(c: Char): Boolean =
    '0' <= c && c <= '9' || 'A' <= c && c <= 'F' || 'a' <= c && c <= 'f'

  def is_ascii_quasi(c: Char): Boolean = c == '_' || c == '\''

  def is_ascii_blank(c: Char): Boolean = " \t\n\u000b\f\r".contains(c)

  def is_ascii_letdig(c: Char): Boolean =
    is_ascii_letter(c) || is_ascii_digit(c) || is_ascii_quasi(c)

  def is_ascii_identifier(s: String): Boolean =
    s.length > 0 && is_ascii_letter(s(0)) && s.forall(is_ascii_letdig)


  /* symbol matching */

  private val symbol_total = new Regex("""(?xs)
    [\ud800-\udbff][\udc00-\udfff] | \r\n |
    \\ < (?: \^raw: [\x20-\x7e\u0100-\uffff && [^.>]]* | \^? ([A-Za-z][A-Za-z0-9_']*)? ) >? |
    .""")

  private def is_plain(c: Char): Boolean =
    !(c == '\r' || c == '\\' || Character.isHighSurrogate(c))

  def is_malformed(s: Symbol): Boolean =
    s.length match {
      case 1 =>
        val c = s(0)
        Character.isHighSurrogate(c) || Character.isLowSurrogate(c) || c == '\ufffd'
      case 2 =>
        val c1 = s(0)
        val c2 = s(1)
        !(c1 == '\r' && c2 == '\n' || Character.isSurrogatePair(c1, c2))
      case _ => !s.endsWith(">") || s == "\\<>" || s == "\\<^>"
    }

  def is_newline(s: Symbol): Boolean =
    s == "\n" || s == "\r" || s == "\r\n"

  class Matcher(text: CharSequence)
  {
    private val matcher = symbol_total.pattern.matcher(text)
    def apply(start: Int, end: Int): Int =
    {
      require(0 <= start && start < end && end <= text.length)
      if (is_plain(text.charAt(start))) 1
      else {
        matcher.region(start, end).lookingAt
        matcher.group.length
      }
    }
  }


  /* iterator */

  private val char_symbols: Array[Symbol] =
    (0 until 256).iterator.map(i => new String(Array(i.toChar))).toArray

  def iterator(text: CharSequence): Iterator[Symbol] =
    new Iterator[Symbol]
    {
      private val matcher = new Matcher(text)
      private var i = 0
      def hasNext = i < text.length
      def next =
      {
        val n = matcher(i, text.length)
        val s =
          if (n == 0) ""
          else if (n == 1) {
            val c = text.charAt(i)
            if (c < char_symbols.length) char_symbols(c)
            else text.subSequence(i, i + n).toString
          }
          else text.subSequence(i, i + n).toString
        i += n
        s
      }
    }

  def explode(text: CharSequence): List[Symbol] = iterator(text).toList

  def advance_line_column(pos: (Int, Int), text: CharSequence): (Int, Int) =
  {
    var (line, column) = pos
    for (sym <- iterator(text)) {
      if (is_newline(sym)) { line += 1; column = 1 }
      else column += 1
    }
    (line, column)
  }


  /* decoding offsets */

  object Index
  {
    private sealed case class Entry(chr: Int, sym: Int)

    val empty: Index = new Index(Nil)

    def apply(text: CharSequence): Index =
    {
      val matcher = new Matcher(text)
      val buf = new mutable.ListBuffer[Entry]
      var chr = 0
      var sym = 0
      while (chr < text.length) {
        val n = matcher(chr, text.length)
        chr += n
        sym += 1
        if (n > 1) buf += Entry(chr, sym)
      }
      if (buf.isEmpty) empty else new Index(buf.toList)
    }
  }

  final class Index private(entries: List[Index.Entry])
  {
    private val hash: Int = entries.hashCode
    private val index: Array[Index.Entry] = entries.toArray

    def decode(symbol_offset: Offset): Text.Offset =
    {
      val sym = symbol_offset - 1
      val end = index.length
      @tailrec def bisect(a: Int, b: Int): Int =
      {
        if (a < b) {
          val c = (a + b) / 2
          if (sym < index(c).sym) bisect(a, c)
          else if (c + 1 == end || sym < index(c + 1).sym) c
          else bisect(c + 1, b)
        }
        else -1
      }
      val i = bisect(0, end)
      if (i < 0) sym
      else index(i).chr + sym - index(i).sym
    }
    def decode(symbol_range: Range): Text.Range = symbol_range.map(decode(_))

    override def hashCode: Int = hash
    override def equals(that: Any): Boolean =
      that match {
        case other: Index => index.sameElements(other.index)
        case _ => false
      }
  }


  /* text chunks */

  object Text_Chunk
  {
    sealed abstract class Name
    case object Default extends Name
    case class Id(id: Document_ID.Generic) extends Name
    case class File(name: String) extends Name

    def apply(text: CharSequence): Text_Chunk =
      new Text_Chunk(Text.Range(0, text.length), Index(text))
  }

  final class Text_Chunk private(val range: Text.Range, private val index: Index)
  {
    override def hashCode: Int = (range, index).hashCode
    override def equals(that: Any): Boolean =
      that match {
        case other: Text_Chunk =>
          range == other.range &&
          index == other.index
        case _ => false
      }

    override def toString: String = "Text_Chunk" + range.toString

    def decode(symbol_offset: Offset): Text.Offset = index.decode(symbol_offset)
    def decode(symbol_range: Range): Text.Range = index.decode(symbol_range)
    def incorporate(symbol_range: Range): Option[Text.Range] =
    {
      def in(r: Range): Option[Text.Range] =
        range.try_restrict(decode(r)) match {
          case Some(r1) if !r1.is_singularity => Some(r1)
          case _ => None
        }
     in(symbol_range) orElse in(symbol_range - 1)
    }
  }


  /* recoding text */

  private class Recoder(list: List[(String, String)])
  {
    private val (min, max) =
    {
      var min = '\uffff'
      var max = '\u0000'
      for ((x, _) <- list) {
        val c = x(0)
        if (c < min) min = c
        if (c > max) max = c
      }
      (min, max)
    }
    private val table =
    {
      var tab = Map[String, String]()
      for ((x, y) <- list) {
        tab.get(x) match {
          case None => tab += (x -> y)
          case Some(z) =>
            error("Duplicate mapping of " + quote(x) + " to " + quote(y) + " vs. " + quote(z))
        }
      }
      tab
    }
    def recode(text: String): String =
    {
      val len = text.length
      val matcher = symbol_total.pattern.matcher(text)
      val result = new StringBuilder(len)
      var i = 0
      while (i < len) {
        val c = text(i)
        if (min <= c && c <= max) {
          matcher.region(i, len).lookingAt
          val x = matcher.group
          result.append(table.getOrElse(x, x))
          i = matcher.end
        }
        else { result.append(c); i += 1 }
      }
      result.toString
    }
  }



  /** symbol interpretation **/

  private lazy val symbols =
    new Interpretation(File.try_read(Path.split(Isabelle_System.getenv("ISABELLE_SYMBOLS"))))

  private class Interpretation(symbols_spec: String)
  {
    /* read symbols */

    private val No_Decl = new Regex("""(?xs) ^\s* (?: \#.* )? $ """)
    private val Key = new Regex("""(?xs) (.+): """)

    private def read_decl(decl: String): (Symbol, Properties.T) =
    {
      def err() = error("Bad symbol declaration: " + decl)

      def read_props(props: List[String]): Properties.T =
      {
        props match {
          case Nil => Nil
          case _ :: Nil => err()
          case Key(x) :: y :: rest => (x -> y) :: read_props(rest)
          case _ => err()
        }
      }
      decl.split("\\s+").toList match {
        case sym :: props if sym.length > 1 && !is_malformed(sym) =>
          (sym, read_props(props))
        case _ => err()
      }
    }

    private val symbols: List[(Symbol, Properties.T)] =
      (((List.empty[(Symbol, Properties.T)], Set.empty[Symbol]) /:
          split_lines(symbols_spec).reverse)
        { case (res, No_Decl()) => res
          case ((list, known), decl) =>
            val (sym, props) = read_decl(decl)
            if (known(sym)) (list, known)
            else ((sym, props) :: list, known + sym)
        })._1


    /* basic properties */

    val properties: Map[Symbol, Properties.T] = Map(symbols: _*)

    val names: Map[Symbol, String] =
    {
      val name = new Regex("""\\<\^?([A-Za-z][A-Za-z0-9_']*)>""")
      Map((for ((sym @ name(a), _) <- symbols) yield (sym -> a)): _*)
    }

    val groups: List[(String, List[Symbol])] =
      symbols.map({ case (sym, props) =>
        val gs = for (("group", g) <- props) yield g
        if (gs.isEmpty) List(sym -> "unsorted") else gs.map(sym -> _)
      }).flatten
        .groupBy(_._2).toList.map({ case (group, list) => (group, list.map(_._1)) })
        .sortBy(_._1)

    val abbrevs: Multi_Map[Symbol, String] =
      Multi_Map((
        for {
          (sym, props) <- symbols
          ("abbrev", a) <- props.reverse
        } yield (sym -> a)): _*)


    /* recoding */

    private val Code = new Properties.String("code")
    private val (decoder, encoder) =
    {
      val mapping =
        for {
          (sym, props) <- symbols
          code =
            props match {
              case Code(s) =>
                try { Integer.decode(s).intValue }
                catch { case _: NumberFormatException => error("Bad code for symbol " + sym) }
              case _ => error("Missing code for symbol " + sym)
            }
          ch = new String(Character.toChars(code))
        } yield {
          if (code < 128) error("Illegal ASCII code for symbol " + sym)
          else (sym, ch)
        }
      (new Recoder(mapping),
       new Recoder(mapping map { case (x, y) => (y, x) }))
    }

    def decode(text: String): String = decoder.recode(text)
    def encode(text: String): String = encoder.recode(text)

    private def recode_set(elems: String*): Set[String] =
    {
      val content = elems.toList
      Set((content ::: content.map(decode)): _*)
    }

    private def recode_map[A](elems: (String, A)*): Map[String, A] =
    {
      val content = elems.toList
      Map((content ::: content.map({ case (sym, a) => (decode(sym), a) })): _*)
    }


    /* user fonts */

    private val Font = new Properties.String("font")
    val fonts: Map[Symbol, String] =
      recode_map((for ((sym, Font(font)) <- symbols) yield (sym -> font)): _*)

    val font_names: List[String] = Set(fonts.toList.map(_._2): _*).toList
    val font_index: Map[String, Int] = Map((font_names zip (0 until font_names.length).toList): _*)


    /* classification */

    val letters = recode_set(
      "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M",
      "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z",
      "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
      "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z",

      "\\<A>", "\\<B>", "\\<C>", "\\<D>", "\\<E>", "\\<F>", "\\<G>",
      "\\<H>", "\\<I>", "\\<J>", "\\<K>", "\\<L>", "\\<M>", "\\<N>",
      "\\<O>", "\\<P>", "\\<Q>", "\\<R>", "\\<S>", "\\<T>", "\\<U>",
      "\\<V>", "\\<W>", "\\<X>", "\\<Y>", "\\<Z>", "\\<a>", "\\<b>",
      "\\<c>", "\\<d>", "\\<e>", "\\<f>", "\\<g>", "\\<h>", "\\<i>",
      "\\<j>", "\\<k>", "\\<l>", "\\<m>", "\\<n>", "\\<o>", "\\<p>",
      "\\<q>", "\\<r>", "\\<s>", "\\<t>", "\\<u>", "\\<v>", "\\<w>",
      "\\<x>", "\\<y>", "\\<z>",

      "\\<AA>", "\\<BB>", "\\<CC>", "\\<DD>", "\\<EE>", "\\<FF>",
      "\\<GG>", "\\<HH>", "\\<II>", "\\<JJ>", "\\<KK>", "\\<LL>",
      "\\<MM>", "\\<NN>", "\\<OO>", "\\<PP>", "\\<QQ>", "\\<RR>",
      "\\<SS>", "\\<TT>", "\\<UU>", "\\<VV>", "\\<WW>", "\\<XX>",
      "\\<YY>", "\\<ZZ>", "\\<aa>", "\\<bb>", "\\<cc>", "\\<dd>",
      "\\<ee>", "\\<ff>", "\\<gg>", "\\<hh>", "\\<ii>", "\\<jj>",
      "\\<kk>", "\\<ll>", "\\<mm>", "\\<nn>", "\\<oo>", "\\<pp>",
      "\\<qq>", "\\<rr>", "\\<ss>", "\\<tt>", "\\<uu>", "\\<vv>",
      "\\<ww>", "\\<xx>", "\\<yy>", "\\<zz>",

      "\\<alpha>", "\\<beta>", "\\<gamma>", "\\<delta>", "\\<epsilon>",
      "\\<zeta>", "\\<eta>", "\\<theta>", "\\<iota>", "\\<kappa>",
      "\\<mu>", "\\<nu>", "\\<xi>", "\\<pi>", "\\<rho>", "\\<sigma>",
      "\\<tau>", "\\<upsilon>", "\\<phi>", "\\<chi>", "\\<psi>",
      "\\<omega>", "\\<Gamma>", "\\<Delta>", "\\<Theta>", "\\<Lambda>",
      "\\<Xi>", "\\<Pi>", "\\<Sigma>", "\\<Upsilon>", "\\<Phi>",
      "\\<Psi>", "\\<Omega>")

    val blanks = recode_set(" ", "\t", "\n", "\u000B", "\f", "\r", "\r\n")

    val sym_chars =
      Set("!", "#", "$", "%", "&", "*", "+", "-", "/", "<", "=", ">", "?", "@", "^", "_", "|", "~")

    val symbolic = recode_set((for { (sym, _) <- symbols; if raw_symbolic(sym) } yield sym): _*)


    /* cartouches */

    val open_decoded = decode(open)
    val close_decoded = decode(close)


    /* control symbols */

    val ctrl_decoded: Set[Symbol] =
      Set((for ((sym, _) <- symbols if sym.startsWith("\\<^")) yield decode(sym)): _*)

    val sub_decoded = decode("\\<^sub>")
    val sup_decoded = decode("\\<^sup>")
    val bsub_decoded = decode("\\<^bsub>")
    val esub_decoded = decode("\\<^esub>")
    val bsup_decoded = decode("\\<^bsup>")
    val esup_decoded = decode("\\<^esup>")
    val bold_decoded = decode("\\<^bold>")
  }


  /* tables */

  def properties: Map[Symbol, Properties.T] = symbols.properties
  def names: Map[Symbol, String] = symbols.names
  def groups: List[(String, List[Symbol])] = symbols.groups
  def abbrevs: Multi_Map[Symbol, String] = symbols.abbrevs

  def decode(text: String): String = symbols.decode(text)
  def encode(text: String): String = symbols.encode(text)

  def decode_string: XML.Decode.T[String] = (x => decode(XML.Decode.string(x)))
  def encode_string: XML.Encode.T[String] = (x => XML.Encode.string(encode(x)))

  def decode_strict(text: String): String =
  {
    val decoded = decode(text)
    if (encode(decoded) == text) decoded
    else {
      val bad = new mutable.ListBuffer[Symbol]
      for (s <- iterator(text) if encode(decode(s)) != s && !bad.contains(s))
        bad += s
      error("Bad Unicode symbols in text: " + commas_quote(bad))
    }
  }

  def fonts: Map[Symbol, String] = symbols.fonts
  def font_names: List[String] = symbols.font_names
  def font_index: Map[String, Int] = symbols.font_index
  def lookup_font(sym: Symbol): Option[Int] = symbols.fonts.get(sym).map(font_index(_))


  /* classification */

  def is_letter(sym: Symbol): Boolean = symbols.letters.contains(sym)
  def is_digit(sym: Symbol): Boolean = sym.length == 1 && '0' <= sym(0) && sym(0) <= '9'
  def is_quasi(sym: Symbol): Boolean = sym == "_" || sym == "'"
  def is_letdig(sym: Symbol): Boolean = is_letter(sym) || is_digit(sym) || is_quasi(sym)
  def is_blank(sym: Symbol): Boolean = symbols.blanks.contains(sym)


  /* cartouches */

  val open = "\\<open>"
  val close = "\\<close>"

  def open_decoded: Symbol = symbols.open_decoded
  def close_decoded: Symbol = symbols.close_decoded

  def is_open(sym: Symbol): Boolean = sym == open_decoded || sym == open
  def is_close(sym: Symbol): Boolean = sym == close_decoded || sym == close


  /* symbols for symbolic identifiers */

  private def raw_symbolic(sym: Symbol): Boolean =
    sym.startsWith("\\<") && sym.endsWith(">") && !sym.startsWith("\\<^")

  def is_symbolic(sym: Symbol): Boolean =
    !is_open(sym) && !is_close(sym) && (raw_symbolic(sym) || symbols.symbolic.contains(sym))

  def is_symbolic_char(sym: Symbol): Boolean = symbols.sym_chars.contains(sym)


  /* control symbols */

  def is_ctrl(sym: Symbol): Boolean =
    sym.startsWith("\\<^") || symbols.ctrl_decoded.contains(sym)

  def is_controllable(sym: Symbol): Boolean =
    !is_blank(sym) && !is_ctrl(sym) && !is_open(sym) && !is_close(sym) && !is_malformed(sym)

  def sub_decoded: Symbol = symbols.sub_decoded
  def sup_decoded: Symbol = symbols.sup_decoded
  def bsub_decoded: Symbol = symbols.bsub_decoded
  def esub_decoded: Symbol = symbols.esub_decoded
  def bsup_decoded: Symbol = symbols.bsup_decoded
  def esup_decoded: Symbol = symbols.esup_decoded
  def bold_decoded: Symbol = symbols.bold_decoded
}
