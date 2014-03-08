package quanto.gui

import scala.swing._
import quanto.data._

class DocumentPage(component0: Component, val document: Document)
extends ClosablePage(document.titleDescription, component0, closeAction = () => { document.promptUnsaved() } )
with Reactor
{
  listenTo(document)
  reactions += {
    case DocumentChanged(_)|DocumentSaved(_) =>
      title = document.titleDescription
  }
}

class GraphDocumentPage(graphEditPanel: GraphEditPanel)
extends DocumentPage(graphEditPanel, graphEditPanel.graphDocument)


