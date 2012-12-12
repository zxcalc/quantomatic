package quanto.gui

import swing.Publisher
import swing.event.Event

abstract class UndoEvent extends Event
case class UndoStackChanged() extends UndoEvent
case class UndoRegistered(name: String) extends UndoEvent
case class UndoPerformed(name: String) extends UndoEvent
case class RedoPerformed(name: String) extends UndoEvent

class UndoStackException(msg: String) extends Exception(msg)

class UndoStack extends Publisher {
  private var redoMode = false

  var commitDepth = 0
  var tempStack = List[()=>Any]()
  var actionName: String = _
  var undoStack = List[(String, List[()=>Any])]()
  var redoStack = List[(String, List[()=>Any])]()

  def start(aName: String) {
    if (commitDepth == 0) actionName = aName
    commitDepth += 1
  }

  def +=(f: =>Any) {
    if (commitDepth == 0) throw new UndoStackException("no active undo action")
    tempStack = (() => f) :: tempStack
  }

  def commit() {
    commitDepth -= 1

    if (commitDepth < 0) throw new UndoStackException("no active undo action")
    else if (commitDepth == 0) {

      if (redoMode) {
        redoStack = (actionName, tempStack) :: redoStack
      } else {
        undoStack = (actionName, tempStack) :: undoStack
        redoStack = List[(String, List[()=>Any])]()
      }

      publish(UndoRegistered(actionName))

      actionName = null
      tempStack = List[()=>Any]()
    }
  }

  def cancel() {
    commitDepth -= 1

    if (commitDepth < 0) throw new UndoStackException("no active undo action")
    else if (commitDepth == 0) {
      actionName = null
      tempStack = List[()=>Any]()
    }
  }

  def register(aName: String)(f: =>Any) {
    start(aName)
    this += f
    commit()
  }

  def undo() {
    undoStack match {
      case (n, fs) :: s =>
        redoMode = true

        // Any recursive undo registrations are nested under a single redo action, with the same name
        // as the undo.
        this.start(n)
        fs foreach (f => f())
        this.commit()

        redoMode = false
        undoStack = s
        publish(UndoPerformed(n))
      case _ =>
    }
  }

  def redo() {
    redoStack match {
      case (n, fs) :: s =>
        fs foreach (f => f())
        redoStack = s
        publish(RedoPerformed(n))
      case _ =>
    }
  }

  def undoActionName = undoStack match {
    case (n,_) :: s => Some(n)
    case _ => None
  }

  def redoActionName = redoStack match {
    case (n,_) :: s => Some(n)
    case _ => None
  }

  def canUndo = !undoStack.isEmpty
  def canRedo = !redoStack.isEmpty
}
