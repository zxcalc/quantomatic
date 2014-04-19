package quanto.core

import java.io._
import java.util.logging._
import quanto.util.json.Json

import quanto.util.{StreamMessage, SignallingStreamRedirector, StreamRedirector}
import java.net.{InetAddress, Socket}
import quanto.gui.QuantoDerive

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
//      val pb = new ProcessBuilder(
//        CoreProcess.polyExe, "--use", "run_protocol.ML")
      val pb = if (!new File("../Resources").exists()) { // check if running inside OS X app bundle
        //QuantoDerive.CoreStatus.text = "didnt find osx-dist in " + new File(".").getAbsolutePath
        val pb1 = new ProcessBuilder(CoreProcess.polyExe, "--ideprotocol")
        pb1.directory(new File(quantoHome + "/core"))

        pb1
      } else {
        //QuantoDerive.CoreStatus.text = "found osx-dist"
        val pb1 = new ProcessBuilder("bin/poly", "--ideprotocol")
        QuantoDerive.CoreStatus.text = new File(".").getAbsolutePath + "bin/poly"

        pb1
      }

      pb.redirectErrorStream(true)
      CoreProcess.logger.log(Level.FINEST, "Starting {0}...", CoreProcess.polyExe)
      backend = pb.start()

      // get a PID for sending interrupt to poly process. Will return None unless system is UNIX-like
      polyPid = None

//      try {
//        val pidField = backend.getClass.getDeclaredField("pid")
//        pidField.setAccessible(true)
//        val p = pidField.getInt(backend)
//        pidField.setAccessible(false)
//        Some(p)
//      } catch {
//        case e: Throwable =>
//          e.printStackTrace()
//          None
//      }

      // wire up console I/O
      consoleInput = backend.getOutputStream
      //consoleOutput = new SignallingStreamRedirector(backend.getInputStream, Some(System.out))
      consoleOutput = new SignallingStreamRedirector(backend.getInputStream)
      consoleOutput.start()

      var spinLock = true
      val sm = StreamMessage.compileMessage(0, "init", "use \"run_protocol.ML\";\n")
      consoleOutput.addListener(0) { _ => println("done"); spinLock = false }
      sm.writeTo(consoleInput)

      while (spinLock) Thread.sleep(50)
      //Thread.sleep(500)


      // wait for signal from run_protocol.ML before connecting to socket
//      var spinLock = true
//      consoleOutput.addListener(0) { _ => spinLock = false }
//      consoleOutput.start()
//      while (spinLock) Thread.sleep(500)
      
      //stdin = new Json.Output(new BufferedWriter(new OutputStreamWriter(backend.getOutputStream)))
      //stdout = new Json.Input(new BufferedReader(new InputStreamReader(backend.getInputStream)))
      socket = new Socket(InetAddress.getByName("localhost"), 4321)
      stdin = new Json.Output(new BufferedWriter(new OutputStreamWriter(socket.getOutputStream)))
      stdout = new Json.Input(new BufferedReader(new InputStreamReader(socket.getInputStream)))


      
      CoreProcess.logger.log(Level.FINEST, "{0} started successfully", CoreProcess.polyExe)

//      new StreamRedirector(backend.getErrorStream, System.err).start()
//      new StreamRedirector(backend.getInputStream, System.out).start()
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
        val killThread = new Thread() {
          override def run() {
            if (waitForExit) {
              try {
                CoreProcess.logger.log(Level.FINER, "Waiting for 5 seconds for the core to exit")
                Thread.sleep(5000)
              } catch {
                case e: InterruptedException =>
                  CoreProcess.logger.log(Level.FINER, "Thread interrupted")
              }
            }
            CoreProcess.logger.log(Level.FINER, "Forcibly terminating the core process")
            backend.destroy()
          }
        }
        
        killThread.start()
    }
  }

  object CoreProcess {
    val logger = Logger.getLogger("quanto.core")
    var quantoHome = new File("../").getCanonicalPath
    println("quanto home is " + quantoHome)
    var polyExe = "/usr/local/bin/poly"
  }
  
  // def inputStream : InputStream =
  //   if (backend!=null) backend.getInputStream()
  //   else null
  // def outputStream : OutputStream =
  //   if (backend!=null) backend.getOutputStream()
  //   else null
}
