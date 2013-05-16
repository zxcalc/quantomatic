package quanto.core

import java.io._
import java.util.logging._
import quanto.util.json.Json

import quanto.util.StreamRedirector

class CoreProcess(parallel: Boolean = false) {
  import CoreProcess._
  private var backend: Process = null
  var stdin : Json.Output = null
  var stdout : Json.Input = null

  def startCore() { startCore(quantoCoreExecutable) }
  
  def startCore(executable : String) {
    try {
      val pb = new ProcessBuilder(
        executable,
        if (parallel) "--par-json-protocol" else "--json-protocol")

      pb.redirectErrorStream(false)
      logger.log(Level.FINEST, "Starting {0}...", executable)
      backend = pb.start()
      
      stdin = new Json.Output(new BufferedWriter(new OutputStreamWriter(backend.getOutputStream)))
      stdout = new Json.Input(new BufferedReader(new InputStreamReader(backend.getInputStream)))
      
      logger.log(Level.FINEST, "{0} started successfully", executable)

      new StreamRedirector(backend.getErrorStream, System.err).start()
    } catch {
      case e : IOException => {
        logger.log(Level.SEVERE,
          "Could not execute \"" + executable + "\": "
            + e.getMessage(), e)
        throw new CoreProtocolException(String.format(
                    "Could not execute \"%1$\": %2$", executable,
                    e.getMessage()), e)
      }
    }
  }
  
  def killCore(waitForExit: Boolean = true) {
    if (backend != null) {
        logger.log(Level.FINEST, "Shutting down the core process")
        val killThread = new Thread() {
          override def run() {
            if (waitForExit) {
              try {
                logger.log(Level.FINER, "Waiting for 5 seconds for the core to exit")
                Thread.sleep(5000)
              } catch {
                case e: InterruptedException =>
                  logger.log(Level.FINER, "Thread interrupted")
              }
            }
            logger.log(Level.FINER, "Forcibly terminating the core process")
            backend.destroy()
          }
        }
        
        killThread.start()
    }
  }

  object CoreProcess {
    val logger = Logger.getLogger("quanto.core");
    var quantoCoreExecutable = "quanto-core";
  }
  
  // def inputStream : InputStream =
  //   if (backend!=null) backend.getInputStream()
  //   else null
  // def outputStream : OutputStream =
  //   if (backend!=null) backend.getOutputStream()
  //   else null
}
