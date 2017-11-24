package quanto.gui

import java.awt.Desktop
import java.awt.event.{MouseAdapter, MouseEvent}

import quanto.gui.QuantoDerive.{Split, listenTo, popup, CurrentProject}
import quanto.util.{UserAlerts, UserOptions}

import scala.swing.{Action, BorderPanel, Dimension, FlowPanel, GridPanel, Label, MenuItem, ProgressBar, Publisher, Separator}
import scala.swing.event.{Event, Key}

class StatusBar extends BorderPanel {


  def MessagesContextMenu() : PopupMenu = new PopupMenu {
    menu =>

    def showAlert(a : UserAlerts.Alert) : Unit = {
      menu.contents += new Label(" "+a.toString)
    }

    val numAlerts : Int = UserAlerts.alerts.count(_ => true)
    if(numAlerts < 6) {
      // Few alerts, so list all of them
      UserAlerts.alerts.reverse.foreach(showAlert)
    } else {
      menu.contents += new Label(" ...")
      // List only first 5
      UserAlerts.alerts.slice(0,5).reverse.foreach(showAlert)
    }

    val ShowLogAction: Action = new Action("Open log file") {
      menu.contents += new MenuItem(this) {
        mnemonic = Key.L
      }

      def apply() {
        if(UserAlerts.logFile.nonEmpty){
          Desktop.getDesktop.browse(UserAlerts.logFile.get.toURI)
        } else {
          if (CurrentProject.isEmpty) {
            UserAlerts.alert("Open a project before trying to access its log")
          } else if (!UserOptions.logging){
            UserAlerts.alert("Please enable persistent logging in the options menu")
          }
        }
      }
    }

    menu.contents += new Separator()

    val ClearMessagesAction: Action = new Action("Clear message") {
      menu.contents += new MenuItem(this) {
        mnemonic = Key.C
      }

      def apply() {
        clearStatusBar()
      }
    }

  }


  val UserMessage = new Label(UserAlerts.latestMessage.toString)
  val ConsoleProgress = new ProgressBar
  val ConsoleProgressLabel = new Label(" ")
  val Status : GridPanel = new GridPanel(1, 2) {
    contents += new FlowPanel(FlowPanel.Alignment.Left)(UserMessage)
    contents += new FlowPanel(FlowPanel.Alignment.Right)(ConsoleProgressLabel, ConsoleProgress)
  }

  ConsoleProgress.preferredSize = ConsoleProgressSize //Currently doesn't respond to UI scaling

  def ConsoleProgressSize: Dimension = new Dimension(UserOptions.scaleInt(100), UserOptions.scaleInt(15))

  Status.peer.addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent) {
      e.getButton match {
        case _ =>
          if (e.isPopupTrigger) {
            popup(MessagesContextMenu(), Some(e))
          }
      }
    }

  })

  def clearStatusBar() : Unit = {
    UserMessage.text = ""
    ConsoleProgressLabel.text = " "
    ConsoleProgress.value = 0
    ConsoleProgress.indeterminate = false
  }


  listenTo(UserAlerts.AlertPublisher)
  reactions += {
    case UserAlerts.UserAlertEvent(alert: UserAlerts.Alert) =>
      UserMessage.text = alert.toString
      UserMessage.foreground = alert.color
    case UserAlerts.UserProcessUpdate(_) =>
      UserAlerts.leastCompleteProcess match {
        case Some(process) => if (process.determinate) {
          ConsoleProgress.indeterminate = false
          ConsoleProgress.value = process.value
        } else {
          ConsoleProgress.indeterminate = true
        }
        case _ => ConsoleProgress.value = 100
      }
      val ongoing = UserAlerts.ongoingProcesses.filter(op => op.value < 100)
      ongoing.count(_ => true) match {
        case 0 => ConsoleProgressLabel.text = " " //keep non-empty so the progressbar stays in line with text
        case 1 => ConsoleProgressLabel.text = ongoing.head.name
        case n => ConsoleProgressLabel.text = n.toString + " processes ongoing"
      }
  }

}
