package quanto.gui

import java.io.File
import quanto.data._
import quanto.util.json.Json
import quanto.layout._
import quanto.util.FileHelper.printToFile

class DerivationDocument(panel: DerivationPanel) extends Document {
  val description = "Derivation"
  val fileExtension = "qderive"

  protected def parent = panel
  private var storedDerivation: Derivation = Derivation(panel.theory, Graph(panel.theory))
  private var _derivation: Derivation = storedDerivation
  def derivation = _derivation
  def derivation_=(d: Derivation) = {
    _derivation = d
    publish(DocumentChanged(this))
  }

  def unsavedChanges = _derivation != storedDerivation

  object rootRef extends HasGraph {
    protected def gr = _derivation.root
    protected def gr_=(g: Graph) {
      _derivation = _derivation.copy(root = g)
    }
  }

  class StepRef(s: DSName) extends HasGraph {
    protected def gr = _derivation.steps(s).graph
    protected def gr_=(g: Graph) {
      _derivation = _derivation.updateGraphInStep(s, g)
    }
  }

  def stepRef(s: DSName) = new StepRef(s)

  protected def loadDocument(f: File) {
    val json = Json.parse(f)
    _derivation = Derivation.fromJson(json, panel.theory)
    storedDerivation = _derivation
  }

  protected def saveDocument(f: File)  {
    val json = Derivation.toJson(_derivation, panel.theory)
    json.writeTo(f)
    storedDerivation = _derivation
  }

  protected def clearDocument() = {
    _derivation = Derivation(panel.theory, root = Graph(panel.theory))
  }

  def root_=(g: Graph) {
    rootRef.graph = g
    // if the root is set manually, move to root (which causes a state refresh of the panel)
    panel.controller.state = HeadState(None)
    publish(DocumentChanged(this))
  }

  def root = rootRef.graph

  override protected def exportDocument(f: File) = {
    val old_state : DeriveState = parent.controller.state
    var state_opt : Option[DeriveState] = derivation.parent(old_state)

    /* sequence of states from root leading to current state's parent */
    var state_list : List[DeriveState] = List()
    while (state_opt.isDefined) {
      state_list = state_opt.get :: state_list
      state_opt = derivation.parent(state_opt.get)
    }

    /* drop the root state */
    state_list = state_list.tail

    /* enclose everything into a quote environment */
    printToFile(f, false)(p => {
      p.println("\\begin{quote}\\raggedright")
    })

    /* output derivation until current state parent */
    state_list.foreach { s =>
      parent.controller.state = s

      /* must compute view data to prevent race condition */
      parent.LhsView.computeDisplayData()
      parent.LhsView.exportView(f, true)

      printToFile(f, true)( p => {
        p.println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
        p.println("\\quad = \\quad")
        p.println("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%")
      })
    }

    /* finally output current state and close quote environment */
    parent.controller.state = old_state
    parent.LhsView.computeDisplayData()
    parent.LhsView.exportView(f, true)
    printToFile(f, true)( p => {
      p.println("\\end{quote}")
    })
  }
}
