/*  Title:      Pure/System/process_result.scala
    Author:     Makarius

Result of system process.
*/

package isabelle

final case class Process_Result(
  rc: Int,
  out_lines: List[String] = Nil,
  err_lines: List[String] = Nil,
  timeout: Boolean = false,
  timing: Timing = Timing.zero)
{
  def out: String = cat_lines(out_lines)
  def err: String = cat_lines(err_lines)
  def error(s: String): Process_Result = copy(err_lines = err_lines ::: List(s))

  def was_timeout: Process_Result = copy(rc = 1, timeout = true)

  def ok: Boolean = rc == 0
  def interrupted: Boolean = rc == Exn.Interrupt.return_code

  def check_rc(pred: Int => Boolean): Process_Result =
    if (pred(rc)) this
    else if (interrupted) throw Exn.Interrupt()
    else Exn.error(err)

  def check: Process_Result = check_rc(_ == 0)

  def print: Process_Result =
  {
    Output.warning(err)
    Output.writeln(out)
    copy(out_lines = Nil, err_lines = Nil)
  }

  def print_stdout: Process_Result =
  {
    Output.warning(err, stdout = true)
    Output.writeln(out, stdout = true)
    copy(out_lines = Nil, err_lines = Nil)
  }

  def print_if(b: Boolean): Process_Result = if (b) print else this
  def print_stdout_if(b: Boolean): Process_Result = if (b) print_stdout else this
}
