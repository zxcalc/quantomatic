/*  Title:      Pure/System/getopts.scala
    Author:     Makarius

Support for command-line options as in GNU bash.
*/

package isabelle


object Getopts
{
  def apply(usage_text: String, option_specs: (String, String => Unit)*): Getopts =
  {
    val options =
      (Map.empty[Char, (Boolean, String => Unit)] /: option_specs) {
        case (m, (s, f)) =>
          val (a, entry) =
            if (s.size == 1) (s(0), (false, f))
            else if (s.size == 2 && s.endsWith(":")) (s(0), (true, f))
            else error("Bad option specification: " + quote(s))
          if (m.isDefinedAt(a)) error("Duplicate option specification: " + quote(a.toString))
          else m + (a -> entry)
      }
    new Getopts(usage_text, options)
  }
}

class Getopts private(usage_text: String, options: Map[Char, (Boolean, String => Unit)])
{
  def usage(): Nothing =
  {
    Console.println(usage_text)
    sys.exit(1)
  }

  private def is_option(opt: Char): Boolean = options.isDefinedAt(opt)
  private def is_option0(opt: Char): Boolean = is_option(opt) && !options(opt)._1
  private def is_option1(opt: Char): Boolean = is_option(opt) && options(opt)._1
  private def print_option(opt: Char): String = quote("-" + opt.toString)
  private def option(opt: Char, opt_arg: List[Char]): Unit =
    try { options(opt)._2.apply(opt_arg.mkString) }
    catch {
      case ERROR(msg) =>
        cat_error(msg, "The error(s) above occurred in command-line option " + print_option(opt))
    }

  private def getopts(args: List[List[Char]]): List[List[Char]] =
    args match {
      case List('-', '-') :: rest_args => rest_args
      case ('-' :: opt :: rest_opts) :: rest_args if is_option0(opt) && !rest_opts.isEmpty =>
        option(opt, Nil); getopts(('-' :: rest_opts) :: rest_args)
      case List('-', opt) :: rest_args if is_option0(opt) =>
        option(opt, Nil); getopts(rest_args)
      case ('-' :: opt :: opt_arg) :: rest_args if is_option1(opt) && !opt_arg.isEmpty =>
        option(opt, opt_arg); getopts(rest_args)
      case List('-', opt) :: opt_arg :: rest_args if is_option1(opt) =>
        option(opt, opt_arg); getopts(rest_args)
      case List(List('-', opt)) if is_option1(opt) =>
        Output.error_message("Command-line option " + print_option(opt) + " requires an argument")
        usage()
      case ('-' :: opt :: _) :: _ if !is_option(opt) =>
        if (opt != '?') Output.error_message("Illegal command-line option " + print_option(opt))
        usage()
      case _ => args
  }

  def apply(args: List[String]): List[String] = getopts(args.map(_.toList)).map(_.mkString)
  def apply(args: Array[String]): List[String] = apply(args.toList)
}
