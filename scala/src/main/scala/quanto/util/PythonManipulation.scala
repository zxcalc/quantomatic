package quanto.util

object PythonManipulation {

  val endOfMachineLine = "# User entered python:"

  def header(documentName: String) : String = {
    s"""
       |# Machine generated python:
       |from quanto.util.Scripting import *
       |script_file_name = "$documentName"
       |def register(name, simproc):
       |  print(script_file_name)
       |  register_simproc(name, simproc, script_file_name)
       |$endOfMachineLine
       |  """.stripMargin
  }

  def addHeader(code: String, documentName : String) : String = {
    header(documentName) + "\n" + code
  }
}
