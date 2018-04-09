package quanto.data

import quanto.rewrite.Simproc
import quanto.util.json._
import java.io.File

import quanto.util.FileHelper

class ProjectLoadException(message: String, cause: Throwable) extends Exception(message, cause)

case class Project(theory: Theory, projectFile: File, name: String) {
  require(!projectFile.isDirectory)

  FileHelper.printJson(projectFile.getAbsolutePath, Project.toJson(this))

  val rootFolder: String = projectFile.getParent
  private val rootFolderFile = projectFile.getParentFile
  var simprocs: Map[String, Simproc] = Map()

  def relativePath(f: File): String = {
    rootFolderFile.toURI.relativize(f.toURI).getPath
  }

  //Scans the given folder for filenames that end in the given extension
  def filesEndingIn(ext: String, path: String = rootFolderFile.getAbsolutePath): List[String] = {
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

  def fromJson(json: Json, projectFile: File): Project = try {
    Project(
      Theory.fromJson(json / "theory"),
      projectFile,
      (json / "name").stringValue
    )
  } catch {
    // First try loading as though old format
    case e: Exception =>
      try {
        println("Attempting to update project file from old format.")
        Project(Theory.fromJson(json / "theory"), projectFile, projectFile.getName)
      }
      catch {
        case e: Exception => throw new ProjectLoadException("Error loading project", e)
      }
  }

  def fromTheoryOrProjectFile(theoryOrProjectFile: File, rootFolder: File = new File("."), name: String = ""): Project = {
    println(s"Asked to load project from: $theoryOrProjectFile")
    val theory: Theory = Theory.fromJson({
      val extension: String = theoryOrProjectFile.getAbsolutePath.replaceAll(".*\\.", "")
      try {
        extension match {
          case "qtheory" =>
            Json.parse(theoryOrProjectFile)
          case "qproject" =>
            Json.parse(theoryOrProjectFile) / "theory"
          case _ =>
            try {
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
    val suggestedFilename = rootFolder + "/" + name + ".qproject"
      new Project(theory, new File(suggestedFilename), name)
  }

  def toJson(project: Project): Json = {
    JsonObject(
      "name" -> project.name,
      "theory" -> Theory.toJson(project.theory)
    )
  }
}
