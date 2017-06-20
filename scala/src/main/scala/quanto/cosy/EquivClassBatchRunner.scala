package quanto.cosy

import java.io.File
import java.nio.file.Paths

import quanto.data.Theory

/**
  * Created by hector on 20/06/2017.
  * A wrapper object for the most common qsynth run
  */
object EquivClassBatchRunner {

  var outputPath : String = "cosy_synth"

  def apply(numAngles: Int = 8, boundaries: Int = 3, vertices: Int, outputFileName: String = "default.qrun"): Unit = {
    val rg = Theory.fromFile("red_green")
    var results = EquivClassRunResults(
      numAngles = numAngles,
      tolerance = EquivClassRunResults.defaultTolerance,
      rulesList = List(),
      theory = rg)

    val diagramStream = ColbournReadEnum.enumerate(numAngles, numAngles, boundaries, vertices)
    results.findEquivalenceClasses(diagramStream, s"ColbournRead $numAngles $numAngles $boundaries $vertices")

    new File(outputPath).mkdirs()
    var testFile = new File(Paths.get(outputPath, outputFileName).toString)
    quanto.util.FileHelper.printToFile(testFile, append = false)(
      p => p.println(results.toJSON.toString())
    )
  }
}
