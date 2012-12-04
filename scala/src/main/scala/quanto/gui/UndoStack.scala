package quanto.gui

class UndoStackException(msg: String) extends Exception(msg)

class UndoStack {
  private var redoMode = false

  var commitDepth = 0
  var tempStack = List[()=>Any]()
  var actionName: String = _
  var undoStack = List[(String, List[()=>Any])]()
  var redoStack = List[(String, List[()=>Any])]()

  def start(aName: String) {
    if (commitDepth == 0) {
      if (redoMode)
        actionName =
          undoStack.headOption.getOrElse(
            throw new UndoStackException("starting a redo action with empty undo stack"))._1
      else actionName = aName
    }
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
      case (_, fs) :: s =>
        redoMode = true
        fs foreach (f => f())
        undoStack = s
        redoMode = false
      case _ =>
    }
  }

  def redo() {
    redoStack match {
      case (_, fs) :: s =>
        fs foreach (f => f())
        redoStack = s
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
