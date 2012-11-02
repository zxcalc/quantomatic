package quanto.core

import java.io._
import java.util.logging._
import scala.actors.Actor._

import quanto.util.StreamRedirector

object CoreProcess {
  val logger = Logger.getLogger("quanto.core");
  var quantoCoreExecutable = "quanto-core";
}

class CoreProcess() {
  import CoreProcess._
  private var backend: Process = null

  def startCore() { startCore(quantoCoreExecutable) }
  
  def startCore(executable : String) {
    try {
      val pb = new ProcessBuilder(executable)

      pb.redirectErrorStream(false)
      logger.log(Level.FINEST, "Starting {0}...", executable)
      backend = pb.start()
      logger.log(Level.FINEST, "{0} started successfully", executable)

      new StreamRedirector(backend.getErrorStream(), System.err).start()
    } catch {
      case e : IOException => {
        logger.log(Level.SEVERE,
          "Could not execute \"" + executable + "\": "
            + e.getMessage(), e);
        throw new CoreProtocolException(String.format(
                    "Could not execute \"%1$\": %2$", executable,
                    e.getMessage()), e)
      }
    }
  }
  
  def killCore() {
    if (backend != null) {
        logger.log(Level.FINEST, "Shutting down the core process");
        actor {
          try {
            logger.log(Level.FINER, "Waiting for 5 seconds for the core to exit");
            Thread.sleep(5000);
          } catch {
            case e: InterruptedException =>
              logger.log(Level.FINER, "Thread interupted");
          }
          logger.log(Level.FINER, "Forcibly terminating the core process");
          backend.destroy();
        }
    }
  }
  
  def inputStream : InputStream =
    if (backend!=null) backend.getInputStream()
    else null
  def outputStream : OutputStream =
    if (backend!=null) backend.getOutputStream()
    else null
}
