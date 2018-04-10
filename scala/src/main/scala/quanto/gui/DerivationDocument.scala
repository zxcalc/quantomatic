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
  private var storedDerivation: Derivation = Derivation(Graph())
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

  protected def clearDocument() {
    _derivation = Derivation(root = Graph(panel.theory))
  }

  def root_=(g: Graph) {
    rootRef.graph = g
    // if the root is set manually, move to root (which causes a state refresh of the panel)
    panel.controller.state = HeadState(None)
    publish(DocumentChanged(this))
  }

  def root: Graph = rootRef.graph

  override protected def exportDocument(f: File) {
    previousDir = f
    val old_state : DeriveState = parent.controller.state
    var state_opt : Option[DeriveState] = Some(old_state) //derivation.parent(old_state)

    /* sequence of states from root leading to current state's parent */
    var state_list : List[DeriveState] = List()
    while (state_opt.isDefined) {
      state_list = state_opt.get :: state_list
      state_opt = derivation.parent(state_opt.get)
    }

    /* drop the root state */
    state_list = state_list.tail

    /* enclose everything into a quote environment */
    printToFile(f, append=false)(p => {
      p.println("\\begin{quote}\\raggedright")
    })

    val baseName = f.getName.substring(0, f.getName.lastIndexOf('.'))
    val fullBaseName = f.getAbsolutePath.substring(0, f.getAbsolutePath.lastIndexOf('.'))

    var i = 0

    /* output derivation until current state parent */
    state_list.foreach { s =>
      parent.controller.state = s
      val stepFileName = fullBaseName + "-" + i + ".tikz"
      println(stepFileName)
      val stepFile = new File(stepFileName)

      /* must compute view data to prevent race condition */
      parent.LhsView.computeDisplayData()
      parent.LhsView.exportView(stepFile, append=false)

      printToFile(f)( p => {
        p.print("\\tikzfig{" + baseName + "-" + i + "}")

        // print an equals sign, unless this is the last step
        if (i < state_list.length - 1) {
          p.println(" $=$")
        } else {
          p.println()
        }
      })

      i += 1
    }


    /* finally output current state and close quote environment */
//    parent.controller.state = old_state
//    parent.LhsView.computeDisplayData()
//    parent.LhsView.exportView(f, true)

    printToFile(f, true)( p => {
      p.println("\\end{quote}")
    })
  }
}
