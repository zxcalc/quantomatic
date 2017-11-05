package quanto.util

import org.python.util.PythonInterpreter

object Scratch extends App {
  println("test")

  val pi = new PythonInterpreter
  pi.exec("f = 4")
  pi.exec("print \"hello from python\"")

//  val toolbox = currentMirror.mkToolBox()


//  val scalaThread = new Thread(new Runnable {
//    def run() {
//      try {
//        val tree = toolbox.parse("if ) s = y (object Foo { def bar() { println(\"eval test\") } }; Foo.barr()")
//        toolbox.eval(tree)
//      } catch {
//        case e =>
//          e.printStackTrace()
//      }
//    }
//  })
//
//  scalaThread.start()
//
//  Thread.sleep(2000)
//  println("I'm still alive!")
}
