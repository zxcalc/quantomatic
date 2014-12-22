/*  Title:      Pure/System/invoke_scala.scala
    Author:     Makarius

JVM method invocation service via Isabelle/Scala.
*/

package isabelle


import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import java.util.concurrent.{Future => JFuture}

import scala.util.matching.Regex


object Invoke_Scala
{
  /* method reflection */

  private val Ext = new Regex("(.*)\\.([^.]*)")
  private val STRING = Class.forName("java.lang.String")

  private def get_method(name: String): String => String =
    name match {
      case Ext(class_name, method_name) =>
        val m =
          try { Class.forName(class_name).getMethod(method_name, STRING) }
          catch {
            case _: ClassNotFoundException | _: NoSuchMethodException =>
              error("No such method: " + quote(name))
          }
        if (!Modifier.isStatic(m.getModifiers)) error("Not at static method: " + m.toString)
        if (m.getReturnType != STRING) error("Bad method return type: " + m.toString)

        (arg: String) => {
          try { m.invoke(null, arg).asInstanceOf[String] }
          catch {
            case e: InvocationTargetException if e.getCause != null =>
              throw e.getCause
          }
        }
      case _ => error("Malformed method name: " + quote(name))
    }


  /* method invocation */

  object Tag extends Enumeration
  {
    val NULL = Value("0")
    val OK = Value("1")
    val ERROR = Value("2")
    val FAIL = Value("3")
    val INTERRUPT = Value("4")
  }

  def method(name: String, arg: String): (Tag.Value, String) =
    Exn.capture { get_method(name) } match {
      case Exn.Res(f) =>
        Exn.capture { f(arg) } match {
          case Exn.Res(null) => (Tag.NULL, "")
          case Exn.Res(res) => (Tag.OK, res)
          case Exn.Exn(Exn.Interrupt()) => (Tag.INTERRUPT, "")
          case Exn.Exn(e) => (Tag.ERROR, Exn.message(e))
        }
      case Exn.Exn(e) => (Tag.FAIL, Exn.message(e))
    }
}


/* protocol handler */

class Invoke_Scala extends Session.Protocol_Handler
{
  private var futures = Map.empty[String, JFuture[Unit]]

  private def fulfill(prover: Prover, id: String, tag: Invoke_Scala.Tag.Value, res: String): Unit =
    synchronized
    {
      if (futures.isDefinedAt(id)) {
        prover.protocol_command("Invoke_Scala.fulfill", id, tag.toString, res)
        futures -= id
      }
    }

  private def cancel(prover: Prover, id: String, future: JFuture[Unit])
  {
    future.cancel(true)
    fulfill(prover, id, Invoke_Scala.Tag.INTERRUPT, "")
  }

  private def invoke_scala(prover: Prover, msg: Prover.Protocol_Output): Boolean = synchronized
  {
    msg.properties match {
      case Markup.Invoke_Scala(name, id) =>
        futures += (id ->
          Simple_Thread.submit_task {
            val (tag, result) = Invoke_Scala.method(name, msg.text)
            fulfill(prover, id, tag, result)
          })
        true
      case _ => false
    }
  }

  private def cancel_scala(prover: Prover, msg: Prover.Protocol_Output): Boolean = synchronized
  {
    msg.properties match {
      case Markup.Cancel_Scala(id) =>
        futures.get(id) match {
          case Some(future) => cancel(prover, id, future)
          case None =>
        }
        true
      case _ => false
    }
  }

  override def stop(prover: Prover): Unit = synchronized
  {
    for ((id, future) <- futures) cancel(prover, id, future)
    futures = Map.empty
  }

  val functions = Map(
    Markup.INVOKE_SCALA -> invoke_scala _,
    Markup.CANCEL_SCALA -> cancel_scala _)
}

