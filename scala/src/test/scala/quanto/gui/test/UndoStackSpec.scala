package quanto.gui.test

import quanto.gui._
import org.scalatest._

class UndoStackSpec extends FlatSpec {
  var undoStack = new UndoStack

  behavior of "an undo stack"

  it can "add an action" in {
    undoStack.start("Empty action")
    undoStack.commit()
  }

  it should "report the action name" in {
    assert(undoStack.undoActionName === Some("Empty action"))
  }

  it should "apply an undo and become empty" in {
    undoStack.undo()
    assert(undoStack.canUndo === false)
  }

  var flip1 = false
  var flip2 = false
  var flip3 = false

  it can "register a compound action" in {
    undoStack.start("Compound action")
    undoStack += { assert(flip1) }
    undoStack += { assert(flip2); flip1 = true }
    undoStack += { assert(flip3); flip2 = true }
    undoStack += { flip3 = true }
    undoStack.commit()
  }

  it should "revert the action in reverse order" in {
    undoStack.undo()
  }

  // some mutable state
  var stateVar = 3.0

  // Two actions that do not commute, for testing that actions get applied in the
  // correct order.
  private def multBy2() {
    stateVar *= 2.0
    undoStack.register("Multiply by 2") { divBy2() }
  }

  private def add5() {
    stateVar += 5.0
    undoStack.register("Add 5") { subtract5() }
  }

  // ...and their associated inverses
  private def divBy2() {
    stateVar /= 2.0
    undoStack.register("Divide by 2") { multBy2() }
  }

  private def subtract5() {
    stateVar -= 5.0
    undoStack.register("Subtract 5") { add5() }
  }

  it should "correctly revert the state" in {
    multBy2()
    assert(stateVar === 6.0)
    undoStack.undo()
    assert(stateVar === 3.0)
  }

  it should "have registered a redo with the same name" in {
    assert(undoStack.redoActionName === Some("Multiply by 2"))
  }

  it should "reapply the change on redo" in {
    undoStack.redo()
    assert(stateVar === 6.0)
  }

  it can "cope with multiple undos and redos" in {
    stateVar = 3
    undoStack = new UndoStack

    multBy2()
    assert(stateVar === 6.0)
    assert(undoStack.undoActionName === Some("Multiply by 2"))
    assert(undoStack.redoActionName === None)

    subtract5()
    assert(stateVar === 1.0)
    assert(undoStack.undoActionName === Some("Subtract 5"))
    assert(undoStack.redoActionName === None)

    undoStack.undo()
    assert(stateVar === 6.0)
    assert(undoStack.undoActionName === Some("Multiply by 2"))
    assert(undoStack.redoActionName === Some("Subtract 5"))

    undoStack.undo()
    assert(stateVar === 3.0)
    assert(undoStack.undoActionName === None)
    assert(undoStack.redoActionName === Some("Multiply by 2"))

    undoStack.redo()
    assert(stateVar === 6.0)
    assert(undoStack.undoActionName === Some("Multiply by 2"))
    assert(undoStack.redoActionName === Some("Subtract 5"))
  }

  it should "clear the redo stack when a change is made" in {
    add5()
    assert(undoStack.redoActionName === None)
  }

  it can "handle nested undo registration" in {
    stateVar = 3

    undoStack.start("Parent action")
    add5()
    multBy2()
    undoStack.commit()

    assert(stateVar === 16.0)
    assert(undoStack.undoActionName === Some("Parent action"))
    undoStack.undo()

    println(undoStack.redoStack)

    assert(stateVar === 3.0)
    undoStack.redo()
    assert(stateVar === 16.0)
  }
}
