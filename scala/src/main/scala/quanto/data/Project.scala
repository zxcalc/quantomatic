package quanto.data

import quanto.rewrite.Simproc
import quanto.util.json._
import java.io.File

class ProjectLoadException(message: String, cause: Throwable) extends Exception(message, cause)

case class Project(theory: Theory, rootFolder: String, name : String) {
  var simprocs: Map[String, Simproc] = Map()

  def relativePath(f: File) : String = {
    new File(rootFolder).toURI.relativize(f.toURI).getPath
  }

  //Scans the given folder for filenames that end in the given extension
  def filesEndingIn(ext: String, path: String = rootFolder): List[String] = {
    val f = new File(path)
    if (f.isDirectory) f.listFiles().toList.flatMap(f => filesEndingIn(ext, f.getPath))
    else if (f.getPath.endsWith(ext)) {
      val fileName = relativePath(f)
      List(fileName.substring(0, fileName.length - ext.length))
    }
    else List()
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
