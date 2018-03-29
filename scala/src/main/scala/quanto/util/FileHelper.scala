package quanto.util

import java.io.File

import quanto.util.json.{Json, JsonObject}

import scala.util.matching.Regex

object FileHelper {
  /**
    * Helper method to print to a file.
    *
    * @param file_name The file to write into
    * @param append    true -- append to an existing file;
    *                  false -- overwrite the file
    */
  def printToFile(file_name: File, append: Boolean = true)
                 (op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(new java.io.FileWriter(file_name, append))
    try {
      op(p)
    } finally {
      p.close()
    }
  }

  def printJson(fileName : String, json: Json) : Unit = {
    val targetFile = new File(fileName)
    val parent = targetFile.getParentFile
    if (!parent.exists && !parent.mkdirs) throw new IllegalStateException("Couldn't create dir: " + parent)
    json.writeTo(new File(fileName))
  }

  def readFile[T](file: File, conversion: Json => T): T = conversion(Json.parse(file))

  def readAllOfType[T](directory: String, regexFilter: String, conversion: Json => T): List[T] = {
    readJSONFromDirectory(directory, regexFilter).map(conversion)
  }

  def readJSONFromDirectory(directory: String, regexFilter: String): List[Json] = {
    getListOfFiles(directory, regexFilter).map(Json.parse)
  }

  def getListOfFiles(directory: String, regexFilter: String): List[File] = {
    val f = new File(directory)
    if (f.exists && f.isDirectory) {
      f.listFiles.filter(f => f.isFile && f.getName.matches(regexFilter)).toList
    } else {
      List[File]()
    }
  }
}
