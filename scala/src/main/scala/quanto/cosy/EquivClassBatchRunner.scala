package quanto.cosy

import java.io.File

import quanto.data.Theory
import quanto.util.json.{JsonArray, JsonObject}

/**
  * Created by hector on 20/06/2017.
  * A wrapper object for the most common qsynth run
  * Superseded by the CoSyRun system
  */
object EquivClassBatchRunner {

  var outputPath: String = "cosy_synth"

  def apply(numAngles: Int = 8, boundaries: Int = 3, vertices: Int, outputFileName: String = "default.qrun"): Unit = {
    val rg = Theory.fromFile("red_green")
    val results = EquivClassRunAdjMat(
      numAngles = numAngles,
      tolerance = EquivClassRunAdjMat.defaultTolerance,
      rulesList = List(),
      theory = rg)

    val diagramStream = ColbournReadEnum.enumerate(numAngles, numAngles, boundaries, vertices)
    results.findEquivalenceClasses(diagramStream.map(_.hash),
      s"ColbournRead $numAngles $numAngles $boundaries $vertices")

    new File(outputPath).mkdirs()
    val testFile = new File(outputPath + "/" + outputFileName)
    quanto.util.FileHelper.printToFile(testFile, append = false)(
      p => p.println(results.toJSON.toString())
    )
  }
}

object TensorBatchRunner {

  var outputPath: String = "cosy_synth"

  def apply(numAngles: Int = 8,
            boundaries: Int = 3,
            vertices: Int): Unit = {
    val rg = Theory.fromFile("red_green")

    val diagramStream = ColbournReadEnum.enumerate(numAngles, numAngles, boundaries, vertices)
    val results = EquivClassRunAdjMat(
      numAngles = numAngles,
      tolerance = EquivClassRunAdjMat.defaultTolerance,
      rulesList = List(),
      theory = rg)


    results.findEquivalenceClasses(diagramStream.map(_.hash),
      s"ColbournRead $numAngles $numAngles $boundaries $vertices")

    new File(outputPath).mkdirs()
    val testFile = new File(
      outputPath + "/" + s"tensors-$numAngles-$boundaries-$vertices.qtensor"
    )
    val lines = diagramStream.map(d => JsonObject(
      "adjMatHash" -> d.hash,
      "tensor" -> results.interpret(d.hash).toJson
    ))

    val jsonHolder = JsonObject("results" -> JsonArray(lines))
    quanto.util.FileHelper.printToFile(testFile, append = false)(
      p => p.println(jsonHolder)
    )
  }
}