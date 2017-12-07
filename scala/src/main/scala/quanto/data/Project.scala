package quanto.data

import quanto.rewrite.Simproc
import quanto.util.json._
import java.io.File

class ProjectLoadException(message: String, cause: Throwable) extends Exception(message, cause)

case class Project(theory: Theory, rootFolder: String, name : String) {
  def rules: Vector[String] = rulesInPath(rootFolder)
  var simprocs: Map[String, Simproc] = Map()
  var simprocSource: Map[String, String] = Map() // name -> file

  def relativePath(f: File) : String = {
    new File(rootFolder).toURI.relativize(f.toURI).getPath
  }

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

  def fromTheoryOrProjectFile(theoryOrProjectFile: String, rootFolder: String = "", name : String = "") : Project = {
    println(s"Asked to load prjoect from: $theoryOrProjectFile")
    val theory: Theory = Theory.fromJson({
      val extension: String = theoryOrProjectFile.replaceAll(".*\\.", "")
      try {
        extension match {
          case "qtheory" =>
            Json.parse(new File(theoryOrProjectFile))
          case "qproject" =>
            Json.parse(new File(theoryOrProjectFile)) / "theory"
          case _ =>
            try{
              val theoryStream = Theory.getClass.getResourceAsStream(theoryOrProjectFile + ".qtheory")
              Json.parse(new Json.Input(theoryStream))
          } catch {
            case e: Exception =>
              throw new ProjectLoadException(s"Could not parse the resource $theoryOrProjectFile", e)
          }
        }
      } catch {
        case e: Exception =>
          throw new ProjectLoadException("Error loading project", e)
      }
    }
    )
    new Project(theory, rootFolder, name)
  }

  def fromJson(json: Json, rootFolder: String): Project = try {
    Project(
      Theory.fromJson(json / "theory"),
      rootFolder,
      (json / "name").stringValue
    )
  } catch {
    // First try loading as though old format
    case e: Exception =>
      try {
        println("Attempting to update project file from old format.")
        Project.fromTheoryOrProjectFile((json / "theory").stringValue, rootFolder)
      }
      catch {
        case e: Exception => throw new ProjectLoadException("Error loading project", e)
      }
  }

  def toJson(project: Project) : Json = {
    JsonObject(
      "name" -> project.name,
      "theory" -> Theory.toJson(project.theory)
    )
  }
}
