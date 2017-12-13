package quanto.cosy

import java.util.Calendar

import quanto.data._
import quanto.data.Names._
import quanto.gui.QuantoDerive
import quanto.rewrite.Simproc
import quanto.util.{FileHelper, UserOptions}
import quanto.util.json.{JsonArray, JsonObject}
import quanto.util.UserAlerts.{Elevation, alert}

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.Publisher
import scala.swing.event.Event


// Each simproc, graph pair generates a SimprocSingleRun result
case class SimprocSingleRun(simprocName: String, derivation: Derivation, derivationTimings: List[(String, Double)])


// Collect the single runs and the simproc definitions in one place
case class SimprocBatchResult(allSimprocs: Map[String, Simproc], singleResults: List[SimprocSingleRun]) {

  lazy val toJson: JsonObject = {
    JsonObject(
      "python" -> JsonArray(
        allSimprocs.map(
          ss => JsonObject(
            ss._1 -> ss._2.sourceCode
          )
        )
      ),
      "results" -> JsonArray(
        singleResults.map(
          ssr => JsonObject(
            "simproc" -> ssr.simprocName,
            "derivation" -> Derivation.toJson(ssr.derivation),
            "timings" -> JsonArray(
              ssr.derivationTimings.map(t =>
                JsonObject(
                  "step" -> t._1,
                  "time" -> t._2
                )
              )
            )
          )
        )
      )
    )
  }
}

object SimprocBatchResult {
  def collate(SBResult: SimprocBatchResult): Map[String, List[(Derivation, List[(String, Double)])]] = {
    SBResult.singleResults.
      groupBy { case SimprocSingleRun(a, b, c) => a }.
      mapValues(_.map { case SimprocSingleRun(a, b, c) => (b, c) })
  }
}


// Set up the run as a SimprocBatch
case class SimprocBatch(selectedSimprocs: List[String], selectedGraphs: List[Graph]) {
  def run(): Unit = {

    val simprocGraphPairs = for (simprocName <- selectedSimprocs; graph <- selectedGraphs) yield (simprocName, graph)
    val listFutureResults = simprocGraphPairs.map(sg => {
      Future {
        val derivationData = SimprocBatch.runSimprocGetTimings(sg._1, sg._2)
        SimprocSingleRun(sg._1, derivationData._1, derivationData._2)
      }
    })
    val futureListResults = Future.sequence(listFutureResults)

    futureListResults.onComplete {
      case Success(list) =>
        val result = SimprocBatchResult(SimprocBatch.loadedSimprocs, list)
        SimprocBatch.publish(SimprocBatchRunComplete(result))
      case Failure(e) =>
        alert("Simproc batch run failed!", Elevation.ERROR)
        e.printStackTrace()
    }
  }
}

case class SimprocNotLoaded(simprocName: String) extends Exception

case class SimprocBatchRunComplete(result: SimprocBatchResult) extends Event

object SimprocBatch extends Publisher {

  var timeout: Long = 60 * 1000 // timeout time in milliseconds

  // CONCURRENTLY run a simproc on a graph
  // Makes its own derivation with timing data
  // Runs until completion or timeout
  def runSimprocGetTimings(simprocName: String, graph: Graph): (Derivation, List[(String, Double)]) = {
    val simproc = simprocFromName(simprocName)
    val startTime: Long = now

    def timeElapsed = now - startTime

    var timings: List[(String, Double)] = List()
    var derivation = new Derivation(graph)
    var parentOpt: Option[DSName] = None
    for ((graph, rule) <- simproc.simp(graph)) {
      // Stop if taking too long
      if (timeElapsed < timeout) {
        val suggest = rule.name.replaceFirst("^.*\\/", "") + "-0"
        val step = DStep(
          name = derivation.steps.freshWithSuggestion(DSName(suggest)),
          rule = rule,
          graph = graph.minimise) // layout is already done by simproc now

        derivation = derivation.addStep(parentOpt, step)
        timings = (step.name.toString, timeElapsed.toDouble) :: timings
        parentOpt = Some(step.name)
      }
    }

    (derivation, timings)
  }

  listenTo(this)
  reactions += {
    case SimprocBatchRunComplete(result) =>
      val fileName = UserOptions.preferredTimeFormat.format(Calendar.getInstance().getTime) + ".qsbr"
      FileHelper.printJson("batch_results/" + fileName, result.toJson)
  }

  implicit def simprocFromName(name: String): Simproc = {
    try {
      //TODO: Some other method of providing simprocs for command line access
      loadedSimprocs(name)
    } catch {
      case e: Exception =>
        alert(s"Requested simproc $name was not found. Please load it first.", Elevation.ERROR)
        throw SimprocNotLoaded(name)
    }
  }

  def now: Long = Calendar.getInstance().getTimeInMillis

  def loadedSimprocs: Map[String, Simproc] = QuantoDerive.CurrentProject.map(p => p.simprocs).getOrElse(Map())

}