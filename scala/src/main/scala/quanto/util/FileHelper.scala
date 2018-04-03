package quanto.util

import java.io.File
import java.net.URI

import quanto.util.json.Json


object FileHelper {

  implicit def uriToFile(uri: URI): File = new File(uri)

  implicit def pathToFile(path: String): File = new File(path)

  val Home: URI = {
    val uri = System.getProperty("user.home").toURI
    if (!uri.exists) throw new IllegalStateException("Couldn't access dir: " + uri)
    uri
  }

  def printToFile(file_name: File, string: String, append: Boolean) {
    printToFile(file_name, append) { p => {
      p.println(string)
    }
    }
  }

  /**
    * Helper method to print to a file.
    *
    * @param file_name The file to write into
    * @param append    true -- append to an existing file;
    *                  false -- overwrite the file
    */
  def printToFile(file_name: File, append: Boolean = true)
                 (op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(new java.io.FileWriter(ensureParentFolderExists(file_name), append))
    try {
      op(p)
    } finally {
      p.close()
    }
  }

  def ensureParentFolderExists(file: File): File = {
    ensureFolderExists(file.getParentFile)
    file
  }

  def ensureFolderExists(file: File): File = {
    if (!file.exists && !file.mkdirs) throw new IllegalStateException("Couldn't create dir: " + file)
    file
  }

  def printJson(fileName: String, json: Json): Unit = {
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
