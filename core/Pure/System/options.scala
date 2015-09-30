/*  Title:      Pure/System/options.scala
    Author:     Makarius

System options with external string representation.
*/

package isabelle


import java.util.Calendar


object Options
{
  type Spec = (String, Option[String])

  val empty: Options = new Options()


  /* representation */

  sealed abstract class Type
  {
    def print: String = Word.lowercase(toString)
  }
  case object Bool extends Type
  case object Int extends Type
  case object Real extends Type
  case object String extends Type
  case object Unknown extends Type

  case class Opt(
    public: Boolean,
    pos: Position.T,
    name: String,
    typ: Type,
    value: String,
    default_value: String,
    description: String,
    section: String)
  {
    private def print(default: Boolean): String =
    {
      val x = if (default) default_value else value
      "option " + name + " : " + typ.print + " = " +
        (if (typ == Options.String) quote(x) else x) +
        (if (description == "") "" else "\n  -- " + quote(description))
    }

    def print: String = print(false)
    def print_default: String = print(true)

    def title(strip: String = ""): String =
    {
      val words = Word.explode('_', name)
      val words1 =
        words match {
          case word :: rest if word == strip => rest
          case _ => words
        }
      Word.implode(words1.map(Word.perhaps_capitalize(_)))
    }

    def unknown: Boolean = typ == Unknown
  }


  /* parsing */

  private val SECTION = "section"
  private val PUBLIC = "public"
  private val OPTION = "option"
  private val OPTIONS = Path.explode("etc/options")
  private val PREFS_DIR = Path.explode("$ISABELLE_HOME_USER/etc")
  private val PREFS = PREFS_DIR + Path.basic("preferences")

  lazy val options_syntax =
    Outer_Syntax.init() + ":" + "=" + "--" +
      (SECTION, Keyword.DOCUMENT_HEADING) + PUBLIC + (OPTION, Keyword.THY_DECL)

  lazy val prefs_syntax = Outer_Syntax.init() + "="

  object Parser extends Parse.Parser
  {
    val option_name = atom("option name", _.is_xname)
    val option_type = atom("option type", _.is_ident)
    val option_value =
      opt(token("-", tok => tok.is_sym_ident && tok.content == "-")) ~ atom("nat", _.is_nat) ^^
        { case s ~ n => if (s.isDefined) "-" + n else n } |
      atom("option value", tok => tok.is_name || tok.is_float)

    val option_entry: Parser[Options => Options] =
    {
      command(SECTION) ~! text ^^
        { case _ ~ a => (options: Options) => options.set_section(a) } |
      opt($$$(PUBLIC)) ~ command(OPTION) ~! (position(option_name) ~ $$$(":") ~ option_type ~
      $$$("=") ~ option_value ~ ($$$("--") ~! text ^^ { case _ ~ x => x } | success(""))) ^^
        { case a ~ _ ~ ((b, pos) ~ _ ~ c ~ _ ~ d ~ e) =>
            (options: Options) => options.declare(a.isDefined, pos, b, c, d, e) }
    }

    val prefs_entry: Parser[Options => Options] =
    {
      option_name ~ ($$$("=") ~! option_value) ^^
      { case a ~ (_ ~ b) => (options: Options) => options.add_permissive(a, b) }
    }

    def parse_file(syntax: Outer_Syntax, parser: Parser[Options => Options],
      options: Options, file: Path): Options =
    {
      val toks = Token.explode(syntax.keywords, File.read(file))
      val ops =
        parse_all(rep(parser), Token.reader(toks, Token.Pos.file(file.implode))) match {
          case Success(result, _) => result
          case bad => error(bad.toString)
        }
      try { (options.set_section("") /: ops) { case (opts, op) => op(opts) } }
      catch { case ERROR(msg) => error(msg + Position.here(file.position)) }
    }
  }

  def init_defaults(): Options =
  {
    var options = empty
    for {
      dir <- Isabelle_System.components()
      file = dir + OPTIONS if file.is_file
    } { options = Parser.parse_file(options_syntax, Parser.option_entry, options, file) }
    options
  }

  def init(): Options = init_defaults().load_prefs()


  /* encode */

  val encode: XML.Encode.T[Options] = (options => options.encode)


  /* command line entry point */

  def main(args: Array[String])
  {
    Command_Line.tool0 {
      args.toList match {
        case get_option :: export_file :: more_options =>
          val options = (Options.init() /: more_options)(_ + _)

          if (get_option != "")
            Console.println(options.check_name(get_option).value)

          if (export_file != "")
            File.write(Path.explode(export_file), YXML.string_of_body(options.encode))

          if (get_option == "" && export_file == "")
            Console.println(options.print)

        case _ => error("Bad arguments:\n" + cat_lines(args))
      }
    }
  }
}


final class Options private(
  val options: Map[String, Options.Opt] = Map.empty,
  val section: String = "")
{
  override def toString: String = options.iterator.mkString("Options (", ",", ")")

  private def print_opt(opt: Options.Opt): String =
    if (opt.public) "public " + opt.print else opt.print

  def print: String = cat_lines(options.toList.sortBy(_._1).map(p => print_opt(p._2)))

  def description(name: String): String = check_name(name).description


  /* check */

  def check_name(name: String): Options.Opt =
    options.get(name) match {
      case Some(opt) if !opt.unknown => opt
      case _ => error("Unknown option " + quote(name))
    }

  private def check_type(name: String, typ: Options.Type): Options.Opt =
  {
    val opt = check_name(name)
    if (opt.typ == typ) opt
    else error("Ill-typed option " + quote(name) + " : " + opt.typ.print + " vs. " + typ.print)
  }


  /* basic operations */

  private def put[A](name: String, typ: Options.Type, value: String): Options =
  {
    val opt = check_type(name, typ)
    new Options(options + (name -> opt.copy(value = value)), section)
  }

  private def get[A](name: String, typ: Options.Type, parse: String => Option[A]): A =
  {
    val opt = check_type(name, typ)
    parse(opt.value) match {
      case Some(x) => x
      case None =>
        error("Malformed value for option " + quote(name) +
          " : " + typ.print + " =\n" + quote(opt.value))
    }
  }


  /* internal lookup and update */

  class Bool_Access
  {
    def apply(name: String): Boolean = get(name, Options.Bool, Properties.Value.Boolean.unapply)
    def update(name: String, x: Boolean): Options =
      put(name, Options.Bool, Properties.Value.Boolean(x))
  }
  val bool = new Bool_Access

  class Int_Access
  {
    def apply(name: String): Int = get(name, Options.Int, Properties.Value.Int.unapply)
    def update(name: String, x: Int): Options =
      put(name, Options.Int, Properties.Value.Int(x))
  }
  val int = new Int_Access

  class Real_Access
  {
    def apply(name: String): Double = get(name, Options.Real, Properties.Value.Double.unapply)
    def update(name: String, x: Double): Options =
      put(name, Options.Real, Properties.Value.Double(x))
  }
  val real = new Real_Access

  class String_Access
  {
    def apply(name: String): String = get(name, Options.String, s => Some(s))
    def update(name: String, x: String): Options = put(name, Options.String, x)
  }
  val string = new String_Access

  class Seconds_Access
  {
    def apply(name: String): Time = Time.seconds(real(name))
  }
  val seconds = new Seconds_Access


  /* external updates */

  private def check_value(name: String): Options =
  {
    val opt = check_name(name)
    opt.typ match {
      case Options.Bool => bool(name); this
      case Options.Int => int(name); this
      case Options.Real => real(name); this
      case Options.String => string(name); this
      case Options.Unknown => this
    }
  }

  def declare(
    public: Boolean,
    pos: Position.T,
    name: String,
    typ_name: String,
    value: String,
    description: String): Options =
  {
    options.get(name) match {
      case Some(other) =>
        error("Duplicate declaration of option " + quote(name) + Position.here(pos) +
          Position.here(other.pos))
      case None =>
        val typ =
          typ_name match {
            case "bool" => Options.Bool
            case "int" => Options.Int
            case "real" => Options.Real
            case "string" => Options.String
            case _ =>
              error("Unknown type for option " + quote(name) + " : " + quote(typ_name) +
                Position.here(pos))
          }
        val opt = Options.Opt(public, pos, name, typ, value, value, description, section)
        (new Options(options + (name -> opt), section)).check_value(name)
    }
  }

  def add_permissive(name: String, value: String): Options =
  {
    if (options.isDefinedAt(name)) this + (name, value)
    else {
      val opt = Options.Opt(false, Position.none, name, Options.Unknown, value, value, "", "")
      new Options(options + (name -> opt), section)
    }
  }

  def + (name: String, value: String): Options =
  {
    val opt = check_name(name)
    (new Options(options + (name -> opt.copy(value = value)), section)).check_value(name)
  }

  def + (name: String, opt_value: Option[String]): Options =
  {
    val opt = check_name(name)
    opt_value match {
      case Some(value) => this + (name, value)
      case None if opt.typ == Options.Bool => this + (name, "true")
      case None => error("Missing value for option " + quote(name) + " : " + opt.typ.print)
    }
  }

  def + (str: String): Options =
  {
    str.indexOf('=') match {
      case -1 => this + (str, None)
      case i => this + (str.substring(0, i), str.substring(i + 1))
    }
  }

  def ++ (specs: List[Options.Spec]): Options =
    (this /: specs)({ case (x, (y, z)) => x + (y, z) })


  /* sections */

  def set_section(new_section: String): Options =
    new Options(options, new_section)

  def sections: List[(String, List[Options.Opt])] =
    options.groupBy(_._2.section).toList.map({ case (a, opts) => (a, opts.toList.map(_._2)) })


  /* encode */

  def encode: XML.Body =
  {
    val opts =
      for ((_, opt) <- options.toList; if !opt.unknown)
        yield (opt.pos, (opt.name, (opt.typ.print, opt.value)))

    import XML.Encode.{string => string_, _}
    list(pair(properties, pair(string_, pair(string_, string_))))(opts)
  }


  /* user preferences */

  def load_prefs(): Options =
    if (Options.PREFS.is_file)
      Options.Parser.parse_file(
        Options.prefs_syntax, Options.Parser.prefs_entry, this, Options.PREFS)
    else this

  def save_prefs()
  {
    val defaults = Options.init_defaults()
    val changed =
      (for {
        (name, opt2) <- options.iterator
        opt1 = defaults.options.get(name)
        if opt1.isEmpty || opt1.get.value != opt2.value
      } yield (name, opt2.value, if (opt1.isEmpty) "  (* unknown *)" else "")).toList

    val prefs =
      changed.sortBy(_._1)
        .map({ case (x, y, z) => x + " = " + Outer_Syntax.quote_string(y) + z + "\n" }).mkString

    Isabelle_System.mkdirs(Options.PREFS_DIR)
    File.write_backup(Options.PREFS,
      "(* generated by Isabelle " + Calendar.getInstance.getTime + " *)\n\n" + prefs)
  }
}


class Options_Variable
{
  private var options = Options.empty

  def value: Options = synchronized { options }
  def update(new_options: Options): Unit = synchronized { options = new_options }

  def + (name: String, x: String): Unit = synchronized { options = options + (name, x) }

  class Bool_Access
  {
    def apply(name: String): Boolean = synchronized { options.bool(name) }
    def update(name: String, x: Boolean): Unit =
      synchronized { options = options.bool.update(name, x) }
  }
  val bool = new Bool_Access

  class Int_Access
  {
    def apply(name: String): Int = synchronized { options.int(name) }
    def update(name: String, x: Int): Unit =
      synchronized { options = options.int.update(name, x) }
  }
  val int = new Int_Access

  class Real_Access
  {
    def apply(name: String): Double = synchronized { options.real(name) }
    def update(name: String, x: Double): Unit =
      synchronized { options = options.real.update(name, x) }
  }
  val real = new Real_Access

  class String_Access
  {
    def apply(name: String): String = synchronized { options.string(name) }
    def update(name: String, x: String): Unit =
      synchronized { options = options.string.update(name, x) }
  }
  val string = new String_Access

  class Seconds_Access
  {
    def apply(name: String): Time = synchronized { options.seconds(name) }
  }
  val seconds = new Seconds_Access
}
