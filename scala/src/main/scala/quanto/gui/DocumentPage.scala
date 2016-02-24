package quanto.gui

import scala.swing._
import quanto.data._

abstract class DocumentPage(component0: Component with HasDocument)
extends ClosablePage(
  component0.document.titleDescription,
  component0,
  closeAction = () => { component0.document.promptUnsaved() } )
with Reactor
{
  val document = component0.document
  listenTo(document)
  reactions += {
    case DocumentChanged(_)|DocumentSaved(_) =>
      title = document.titleDescription
  }

  def documentType: String
}

class GraphDocumentPage(val theory: Theory)
extends DocumentPage(new GraphEditPanel(theory)) {
  val documentType = "Graph"
}

class RuleDocumentPage(val theory: Theory)
extends DocumentPage(new RuleEditPanel(theory)) {
  val documentType = "Rule"
}

class MLDocumentPage
extends DocumentPage(new MLEditPanel) {
  val documentType = "ML Code"
}

class DerivationDocumentPage(val project: Project)
extends DocumentPage(new DerivationPanel(project)) {
  val documentType = "Derivation"
}

class SynthDocumentPage(val theory: Theory)
  extends DocumentPage(new SynthPanel(theory)) {
  val documentType = "Synthesis"
}
