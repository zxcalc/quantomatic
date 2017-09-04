package quanto.data

import quanto.util.json._
import java.io.File

class ProjectLoadException(message: String, cause: Throwable) extends Exception(message, cause)

case class Project(theoryFile: String, rootFolder: String = "") {
  val theory : Theory = Theory.fromJson(
    try {
      Json.parse(new File(theoryFile))
    } catch {
      case e: Exception =>
        try {
          Json.parse(new Json.Input(Theory.getClass.getResourceAsStream(theoryFile + ".qtheory")))
        }
        catch {
          case e: Exception =>
            throw new ProjectLoadException("Error loading project", e)
        }
    }
  )

  def rules: Vector[String] = rulesInPath(rootFolder)

  private def rulesInPath(p: String): Vector[String] = {
    val f = new File(p)
    if (f.isDirectory) f.listFiles().toVector.flatMap(f => rulesInPath(f.getPath))
    else if (f.getPath.endsWith(".qrule")) {
      val fname = f.getPath
      Vector(fname.substring(rootFolder.length + 1, fname.length - 6))
    }
    else Vector()
  }
}

object Project {
  def fromJson(json: Json, rootFolder: String) = try {
    Project((json / "theory").stringValue, rootFolder)
  } catch {
    case e: Exception => throw new ProjectLoadException("Error loading project", e)
  }

  def toJson(project: Project) = {
    JsonObject("theory" -> project.theoryFile)
  }
}
