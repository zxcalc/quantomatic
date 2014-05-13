package quanto.util

object FileHelper {
  /**
   * Helper method to print to a file.
   * @param file_name The file to write into
   * @param append true -- append to an existing file;
   *               false -- overwrite the file
   */
  def printToFile(file_name: java.io.File, append : Boolean = true)
                 (op: java.io.PrintWriter => Unit) {
    val p = new java.io.PrintWriter(new java.io.FileWriter(file_name, append))
    try { op(p) } finally { p.close() }
  }
}
