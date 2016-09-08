package quanto.util

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

object Scratch extends App {
  println("test")
  val toolbox = currentMirror.mkToolBox()

  val scalaThread = new Thread(new Runnable {
    def run() {
      try {
        val tree = toolbox.parse("if ) s = y (object Foo { def bar() { println(\"eval test\") } }; Foo.barr()")
        toolbox.eval(tree)
      } catch {
        case e =>
          e.printStackTrace()
      }
    }
  })

  scalaThread.start()

  Thread.sleep(2000)
  println("I'm still alive!")
}
