package quanto.data

import quanto.util.json._

class ProjectLoadException(message: String, cause: Throwable) extends Exception(message, cause)

case class Project(theoryFile: String, rootFolder: String = "") {
  val theory = Theory.fromJson(Json.parse(
    new Json.Input(Theory.getClass.getResourceAsStream(theoryFile + ".qtheory"))))
}

object Project {
  def fromJson(json: Json, rootFolder : String) = try {
    Project((json / "theory").stringValue, rootFolder)
  } catch {
    case e: Exception => throw new ProjectLoadException("Error loading project", e)
  }

  def toJson(project: Project) = {
    JsonObject("theory" -> project.theoryFile)
  }
}
