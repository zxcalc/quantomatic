package quanto.core

import java.io._
import java.util.logging._
import quanto.util.json.Json

import quanto.util._
import java.net.{InetAddress, Socket}
import quanto.gui.QuantoDerive
import scala.swing.Swing
import java.awt.Color
import java.util.Random
import java.util.concurrent.locks.ReentrantLock

class CoreProcess {
  private var backend: Process = _
  var stdin : Json.Output = _
  var stdout : Json.Input = _
  var socket : Socket = _
  var consoleOutput : SignallingStreamRedirector = _
  var consoleInput : OutputStream = _
  var polyPid : Option[Int] = None

  def startCore() { startCore(CoreProcess.quantoHome) }
  
  def startCore(quantoHome : String) {
    try {
      val pb = new ProcessBuilder(CoreProcess.polyExe, "--ideprotocol")
      println("use poly: " + new File(CoreProcess.polyExe).getCanonicalPath)

      if (!Globals.isMacBundle && !Globals.isLinuxBundle && !Globals.isWindowsBundle) {
        pb.directory(new File(quantoHome + "/core"))
      }


      pb.redirectErrorStream(true)
      //CoreProcess.logger.log(Level.FINEST, "Starting {0}...", CoreProcess.polyExe)
      backend = pb.start()

      println("core started")

      // wire up console I/O
      consoleInput = backend.getOutputStream
      consoleOutput = new SignallingStreamRedirector(backend.getInputStream,Some(System.out))
      consoleOutput.start()

      // synchronous ML compilation using a condition variable
      //val compileLock = new ReentrantLock
      //val compileDone = compileLock.newCondition()
      var bCompileDone = true
      def compileReset() { bCompileDone = false }
      def compileWait() { while(!bCompileDone) Thread.sleep(30); } //{ println(s); compileLock.lock(); compileDone.await(); compileLock.unlock(); Thread.sleep(100) }
      def compileSignal() { bCompileDone = true } //{ println(s); compileLock.lock(); compileDone.signal(); compileLock.unlock() }


      compileReset()
      val sm = StreamMessage.compileMessage(0, "init", "use \"run_protocol.ML\";\n")
      consoleOutput.addListener(0) { _ => compileSignal() }
      sm.writeTo(consoleInput)

      println("loading heap and protocol...")
      compileWait()
      println("loading heap and protocol...done")

      compileReset()
      val sm1 = StreamMessage.compileMessage(1, "init", "val p = 2;\n")
      consoleOutput.addListener(1) { _ => compileSignal() }
      sm1.writeTo(consoleInput)

      println("test...")
      compileWait()
      println("test...done")

      var msgId = 1
      var port = 4321
      var success = false
      val r = new Random

      // start with 4321, then try random ports until we get a sucessful connection
      while (!success && msgId < 10) {
        compileReset()
        val code = "poll_future (Future.fork (run_protocol " + port + "));\n"
        //val code = "val p = 4;\n"
        val sm1 = StreamMessage.compileMessage(msgId, "init", code)
        consoleOutput.addListener(msgId) { msg =>
          if (msg.stripCodes(2) == StringPart("S")) {
            println("got success at port " + port)
            success = true
          } else {
            port = 4321 + Math.abs(r.nextInt() % 10000)
            println("got error, trying port " + port)
          }

          compileSignal()
        }

        sm1.writeTo(consoleInput)

        println("starting protocol...")
        compileWait()
        println("starting protocol...done")


        msgId += 1
      }

      socket = new Socket(InetAddress.getByName("localhost"), port)
      stdin = new Json.Output(new BufferedWriter(new OutputStreamWriter(socket.getOutputStream)))
      stdout = new Json.Input(new BufferedReader(new InputStreamReader(socket.getInputStream)))
      
      println(CoreProcess.polyExe + " started successfully")
    } catch {
      case e : IOException =>
        CoreProcess.logger.log(Level.SEVERE,
          "Could not execute \"" + CoreProcess.polyExe + "\": "
            + e.getMessage, e)
        throw new CoreProtocolException(String.format(
                    "Could not execute \"%s\": %s", CoreProcess.polyExe,
                    e.getMessage), e)

    }
  }
  
  def killCore(waitForExit: Boolean = true) {
    if (backend != null) {
      CoreProcess.logger.log(Level.FINEST, "Shutting down the core process")
      stdin.close()
      stdout.close()
      socket.close()
      consoleOutput.interrupt()
      consoleInput.close()
      backend.destroy()
        // val killThread = new Thread() {
        //   override def run() {
        //     if (waitForExit) {
        //       try {
        //         CoreProcess.logger.log(Level.FINER, "Waiting for 5 seconds for the core to exit")
        //         Thread.sleep(5000)
        //       } catch {
        //         case e: InterruptedException =>
        //           CoreProcess.logger.log(Level.FINER, "Thread interrupted")
        //       }
        //     }
        //     CoreProcess.logger.log(Level.FINER, "Forcibly terminating the core process")
        //     backend.destroy()
        //   }
        // }
        
        // killThread.start()
    }
  }

  object CoreProcess {
    val logger = Logger.getLogger("quanto.core")
    val quantoHome = new File("../").getCanonicalPath
    println("quanto home is " + quantoHome)

    // try to find poly in common locations
    val polyExe = if (new File("bin/poly").exists)
                    "bin/poly"  // for mac/linux bundles
                  else if (new File("bin/poly.exe").exists)
                    "bin/poly.exe"  // for windows installation
                  else if (new File("/usr/local/bin/poly").exists)
                    "/usr/local/bin/poly" // installed from source
                  else if (new File("/usr/bin/poly").exists)
                    "/usr/bin/poly" // installed by e.g. package manager
                  else if (new File("C:/Program Files/PolyML/bin/poly.exe").exists)
                    "C:/Program Files/PolyML/bin/poly.exe" // windows devel setup
                  else "poly" // hope we get lucky
  }
  
  // def inputStream : InputStream =
  //   if (backend!=null) backend.getInputStream()
  //   else null
  // def outputStream : OutputStream =
  //   if (backend!=null) backend.getOutputStream()
  //   else null
}
