package quanto.gui

import java.io.File

import quanto.data._
import quanto.util.UserAlerts
import quanto.util.json.Json

import scala.swing.Component
import scala.swing.event.Event
import scala.swing.Publisher
import scala.swing.event.Event


case class TheoryChanged() extends Event

// Will infer the theory from QuantoDerive!
// Otherwise can have multiple theories editing at once, but only one should be affecting the project
class TheoryDocument(val parent: Component) extends Document with Publisher {
  val description = "Theory"
  val fileExtension = "qtheory"
  private var _theory: Theory = getTheory()

  def getTheory(): Theory = {
    if (QuantoDerive.CurrentProject.isEmpty) {
      blankTheory()
    } else {
      QuantoDerive.CurrentProject.get.theory
    }
  }

  protected def clearDocument() {
    _theory = blankTheory()
    publish(TheoryChanged())
  }

  def blankTheory(): Theory = {
    new Theory(name = "unsaved",
      coreName = "unsaved",
      vertexTypes = Map(), edgeTypes = Map(),
      defaultVertexType = "")
  }

  override def loadDocument(f: File) {
    // Not convinced this should ever be called
    val json = Json.parse(f)
    val newTheory = Theory.fromJson(json)
    //theory = newTheory
    publish(TheoryChanged())
  }

  // There should never be unsaved changes
  override def unsavedChanges: Boolean = false

  protected def saveDocument(f: File) {
    val json = Theory.toJson(theory)
    json.writeTo(f)
    publish(TheoryChanged())
  }

  def theory: Theory = _theory

  def theory_=(th: Theory): Unit = {
    if (QuantoDerive.CurrentProject.isEmpty){
      UserAlerts.alert("Please open a project before altering theory files", UserAlerts.Elevation.WARNING)
    } else {
      val project = QuantoDerive.CurrentProject.get
      QuantoDerive.CurrentProject = Some(new Project(th, project.projectFile, project.name))
      QuantoDerive.updateProjectFile(project.projectFile)
    }
    _theory = getTheory()
    publish(TheoryChanged())
  }


  override protected def exportDocument(f: File) {
    showSaveAsDialog(None)
  }


}
