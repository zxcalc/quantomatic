package quanto.util

import java.io.File
import java.util.{Calendar, Date, UUID}

import scala.swing.{Color, Dialog, Publisher}
import scala.swing.event.Event


// Universal system for alerting the user
// Messages appear in the bottom pane (Label and progress bar)
// SelfAlertingProcess is the easiest way to access this system
// Listen to events via AlertPublisher
object UserAlerts {

  case class UserAlertEvent(alert: Alert) extends Event

  case class UserProcessUpdate(ongoingProcess: UserStartedProcess) extends Event

  object AlertPublisher extends Publisher

  class SelfAlertingProcess(name: String) extends UserStartedProcess(name) {
    alert(name + ": Started")

    override def fail(): Unit = {
      super.fail()
      alert(name + ": Failed", Elevation.ERROR)
    }

    override def finish(): Unit = {
      super.finish()
      alert(name + ": Finished")
    }
  }

  class UserStartedProcess(val name: String) {
    //private val uuid : UUID = UUID.randomUUID() //Will need for log files
    private var _determinate: Boolean = false
    private var _value: Int = 0
    private var _failed: Boolean = false

    def failed: Boolean = _failed

    def determinate: Boolean = _determinate

    def value: Int = _value

    def value_=(newValue: Int): Unit = {
      _value = newValue
      _determinate = true
      AlertPublisher.publish(UserProcessUpdate(this))
    }

    def setIndeterminate(): Unit = {
      _value = 0
      _determinate = false
      AlertPublisher.publish(UserProcessUpdate(this))
    }

    def fail(): Unit = {
      _failed = true
      value = 100
    }

    def finish(): Unit = {
      value = 100
    }

    ongoingProcesses = this :: ongoingProcesses
    AlertPublisher.publish(UserProcessUpdate(this))
  }

  var ongoingProcesses: List[UserStartedProcess] = List()


  def leastCompleteProcess: Option[UserStartedProcess] = {
    if (ongoingProcesses.isEmpty) None else {
      val indeterminate = ongoingProcesses.find(op => !op.determinate)
      if (indeterminate.nonEmpty) indeterminate else {
        Some(ongoingProcesses.minBy(op => op.value))
      }
    }
  }


  case class Alert(time: Date, elevation: Elevation.Elevation, message: String) {
    override def toString: String = UserOptions.preferredTimeFormat.format(time) + ": " + message

    def color: Color = {
      elevation match {
        case Elevation.ERROR => new Color(150, 0, 0) // Something broke
        case Elevation.ALERT => new Color(150, 150, 0) // Something soon to break
        case Elevation.WARNING => new Color(0, 150, 0) // That would have caused something to break
        case Elevation.DEBUG => new Color(0, 150, 150) // I want to know how it would have broken
        case Elevation.NOTICE => new Color(0, 0, 150) // Nothing is broken
      }
    }

    def elevationText: String = {
      elevation match {
        case Elevation.ERROR => "ERROR" // Something broke
        case Elevation.ALERT => "ALERT" // Something soon to break
        case Elevation.WARNING => "WARNING" // That would have caused something to break
        case Elevation.DEBUG => "DEBUG" // I want to know how it would have broken
        case Elevation.NOTICE => "" // Nothing is broken
      }
    }
  }

  object Elevation extends Enumeration {
    type Elevation = Value
    val ALERT, ERROR, WARNING, NOTICE, DEBUG = Value
  }

  var alerts: List[Alert] = List()

  def latestMessage: Alert = {
    if (alerts.headOption.nonEmpty) alerts.head else {
      alert("Quantomatic starting up")
      latestMessage
    }
  }

  def alert(message: String): Unit = alert(message, Elevation.NOTICE)

  def errorbox(message: String): Unit = {
    alert(message, Elevation.ERROR)
    Dialog.showMessage(
      title = "Error",
      message = message,
      messageType = Dialog.Message.Error)
  }

  def alert(message: String, elevation: Elevation.Elevation): Unit = {
    val newAlert = Alert(Calendar.getInstance().getTime, elevation, message)
    println(newAlert.toString)
    alerts = newAlert :: alerts
    AlertPublisher.publish(UserAlertEvent(newAlert))
    writeToLogFile(newAlert)
  }

  def logFile: Option[File] = {
    // It's possible to get here before the GUI instantiates
    val project = quanto.gui.QuantoDerive.CurrentProject
    if (project != null && project.nonEmpty && UserOptions.logging) {
      Some(new File(quanto.gui.QuantoDerive.CurrentProject.get.rootFolder + "/log.txt"))
    }
    else {
      None
    }
  }

  def writeToLogFile(alert: Alert): Unit = {
    val elevation = alert.elevationText match {
      case "" => ""
      case e => s"[$e]"
    }
    if (logFile.nonEmpty) {
      FileHelper.printToFile(logFile.get)(
        p => p.println(UserOptions.preferredTimeFormat.format(alert.time) + ": " + elevation + alert.message)
      )

    }
  }
}
