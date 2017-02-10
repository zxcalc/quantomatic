/*  Title:      Pure/System/isabelle_tool.scala
    Author:     Makarius
    Author:     Lars Hupel

Isabelle system tools: external executables or internal Scala functions.
*/

package isabelle

import java.net.URLClassLoader
import scala.reflect.runtime.universe
import scala.tools.reflect.{ToolBox,ToolBoxError}


object Isabelle_Tool
{
  /* Scala source tools */

  abstract class Body extends Function[List[String], Unit]

  private def compile(path: Path): Body =
  {
    def err(msg: String): Nothing =
      cat_error(msg, "The error(s) above occurred in Isabelle/Scala tool " + path)

    val source = File.read(path)

    val class_loader = new URLClassLoader(Array(), getClass.getClassLoader)
    val tool_box = universe.runtimeMirror(class_loader).mkToolBox()

    try {
      val symbol = tool_box.parse(source) match {
        case tree: universe.ModuleDef => tool_box.define(tree)
        case _ => err("Source does not describe a module (Scala object)")
      }
      tool_box.compile(universe.Ident(symbol))() match {
        case body: Body => body
        case _ => err("Ill-typed source: Isabelle_Tool.Body expected")
      }
    }
    catch {
      case e: ToolBoxError =>
        if (tool_box.frontEnd.hasErrors) {
          val infos = tool_box.frontEnd.infos.toList
          val msgs = infos.map(info => "Error in line " + info.pos.line + ":\n" + info.msg)
          err(msgs.mkString("\n"))
        }
        else
          err(e.toString)
    }
  }


  /* external tools */

  private def dirs(): List[Path] = Path.split(Isabelle_System.getenv_strict("ISABELLE_TOOLS"))

  private def is_external(dir: Path, file_name: String): Boolean =
  {
    val file = (dir + Path.basic(file_name)).file
    try {
      file.isFile && file.canRead &&
        (file_name.endsWith(".scala") || file.canExecute) &&
        !file_name.endsWith("~") && !file_name.endsWith(".orig")
    }
    catch { case _: SecurityException => false }
  }

  private def list_external(): List[(String, String)] =
  {
    val Description = """.*\bDESCRIPTION: *(.*)""".r
    for {
      dir <- dirs() if dir.is_dir
      file_name <- File.read_dir(dir) if is_external(dir, file_name)
    } yield {
      val source = File.read(dir + Path.basic(file_name))
      val name = Library.try_unsuffix(".scala", file_name) getOrElse file_name
      val description =
        split_lines(source).collectFirst({ case Description(s) => s }) getOrElse ""
      (name, description)
    }
  }

  private def find_external(name: String): Option[List[String] => Unit] =
    dirs.collectFirst({
      case dir if is_external(dir, name + ".scala") =>
        compile(dir + Path.basic(name + ".scala"))
      case dir if is_external(dir, name) =>
        (args: List[String]) =>
          {
            val tool = dir + Path.basic(name)
            val result = Isabelle_System.bash(File.bash_path(tool) + " " + Bash.strings(args))
            sys.exit(result.print_stdout.rc)
          }
    })


  /* internal tools */

  private val internal_tools: List[Isabelle_Tool] =
    List(
      Build.isabelle_tool,
      Build_Doc.isabelle_tool,
      Build_PolyML.isabelle_tool,
      Build_Stats.isabelle_tool,
      Check_Sources.isabelle_tool,
      Doc.isabelle_tool,
      ML_Process.isabelle_tool,
      NEWS.isabelle_tool,
      Options.isabelle_tool,
      Profiling_Report.isabelle_tool,
      Remote_DMG.isabelle_tool,
      Update_Cartouches.isabelle_tool,
      Update_Header.isabelle_tool,
      Update_Then.isabelle_tool,
      Update_Theorems.isabelle_tool)

  private def list_internal(): List[(String, String)] =
    for (tool <- internal_tools.toList if tool.accessible)
      yield (tool.name, tool.description)

  private def find_internal(name: String): Option[List[String] => Unit] =
    internal_tools.collectFirst({
      case tool if tool.name == name && tool.accessible =>
        args => Command_Line.tool0 { tool.body(args) }
      })


  /* command line entry point */

  def main(args: Array[String])
  {
    Command_Line.tool0 {
      args.toList match {
        case Nil | List("-?") =>
          val tool_descriptions =
            (list_external() ::: list_internal()).sortBy(_._1).
              map({ case (a, "") => a case (a, b) => a + " - " + b })
          Getopts("""
Usage: isabelle TOOL [ARGS ...]

  Start Isabelle TOOL with ARGS; pass "-?" for tool-specific help.

Available tools:""" + tool_descriptions.mkString("\n  ", "\n  ", "\n")).usage
        case tool_name :: tool_args =>
          find_external(tool_name) orElse find_internal(tool_name) match {
            case Some(tool) => tool(tool_args)
            case None => error("Unknown Isabelle tool: " + quote(tool_name))
          }
      }
    }
  }
}

sealed case class Isabelle_Tool(
  name: String, description: String, body: List[String] => Unit, admin: Boolean = false)
{
  def accessible: Boolean = !admin || Isabelle_System.admin()
}
