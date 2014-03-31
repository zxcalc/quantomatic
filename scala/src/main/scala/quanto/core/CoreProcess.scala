package quanto.core

import java.io._
import java.util.logging._
import quanto.util.json.Json

import quanto.util.{SignallingStreamRedirector, StreamRedirector}
import java.net.{InetAddress, Socket}
import java.nio.file.Paths

class CoreProcess {
  private var backend: Process = _
  var stdin : Json.Output = _
  var stdout : Json.Input = _
  var socket : Socket = _
  var consoleOutput : SignallingStreamRedirector = _
  var consoleInput : BufferedWriter = _

  def startCore() { startCore(CoreProcess.quantoHome) }
  
  def startCore(quantoHome : String) {
    try {
      val pb = new ProcessBuilder(
        CoreProcess.polyExe, "--use", "run_protocol.ML")
      pb.directory(new File(quantoHome + "/core"))
      pb.redirectErrorStream(true)
      CoreProcess.logger.log(Level.FINEST, "Starting {0}...", CoreProcess.polyExe)
      backend = pb.start()

      // wire up console I/O
      consoleInput = new BufferedWriter(new OutputStreamWriter(backend.getOutputStream))
      consoleOutput = new SignallingStreamRedirector(backend.getInputStream, Some(System.out))

      // wait for signal from run_protocol.ML before connecting to socket
      var spinLock = true
      consoleOutput.addListener(0) { _ => spinLock = false }
      consoleOutput.start()
      while (spinLock) Thread.sleep(50)
      
      //stdin = new Json.Output(new BufferedWriter(new OutputStreamWriter(backend.getOutputStream)))
      //stdout = new Json.Input(new BufferedReader(new InputStreamReader(backend.getInputStream)))
      socket = new Socket(InetAddress.getByName("localhost"), 4321)
      stdin = new Json.Output(new BufferedWriter(new OutputStreamWriter(socket.getOutputStream)))
      stdout = new Json.Input(new BufferedReader(new InputStreamReader(socket.getInputStream)))


      
      CoreProcess.logger.log(Level.FINEST, "{0} started successfully", CoreProcess.polyExe)

//      new StreamRedirector(backend.getErrorStream, System.err).start()
//      new StreamRedirector(backend.getInputStream, System.out).start()
    } catch {
      case e : IOException => {
        CoreProcess.logger.log(Level.SEVERE,
          "Could not execute \"" + CoreProcess.polyExe + "\": "
            + e.getMessage, e)
        throw new CoreProtocolException(String.format(
                    "Could not execute \"%s\": %s", CoreProcess.polyExe,
                    e.getMessage), e)
      }
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
