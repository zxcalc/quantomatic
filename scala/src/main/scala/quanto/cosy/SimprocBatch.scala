package quanto.cosy

import java.util.Calendar

import quanto.data._
import quanto.data.Names._
import quanto.gui.QuantoDerive
import quanto.gui.BatchDerivationCreatorPanel
import quanto.rewrite.Simproc
import quanto.util.{FileHelper, UserOptions}
import quanto.util.json.{Json, JsonArray, JsonObject}
import quanto.util.UserAlerts.{Elevation, alert}

import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.Publisher
import scala.swing.event.Event


// Each simproc, graph pair generates a SimprocSingleRun result
case class SimprocSingleRun(simprocName: String, derivation: Derivation, derivationTimings: List[(String, Double)])

object SimprocSingleRun {
  def toJson(ssr: SimprocSingleRun): Json = {
    JsonObject(
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
  }

  def fromJson(json: Json): SimprocSingleRun = {
    val name: String = (json / "simproc").stringValue
    val derivation: Derivation = Derivation.fromJson(json / "derivation")
    val timings: List[(String, Double)] = (json / "timings").asArray.map(j => (
      (j / "step").stringValue,
      (j / "time").doubleValue)).toList
    SimprocSingleRun(name, derivation, timings)
  }
}

// Collect the single runs and the simproc definitions in one place
case class SimprocBatchResult(selectedSimprocs: List[String],
                              allSimprocs: Map[String, String],
                              singleResults: List[SimprocSingleRun],
                              notes: String) {

  lazy val toJson: JsonObject = {
    JsonObject(
      "python" -> JsonArray(
        allSimprocs.map(
          ss => JsonObject(
            ss._1 -> ss._2
          )
        )
      ),
      "selected_simprocs" -> JsonArray(selectedSimprocs),
      "results" -> JsonArray(
        singleResults.map(sr => SimprocSingleRun.toJson(sr))
      ),
      "notes" -> notes
    )
  }
}

case class SimprocLazyBatchResult(notes: String, selectedSimprocs: List[String], resultCount: Int)

object SimprocBatchResult {
  def collate(SBResult: SimprocBatchResult): Map[String, List[(Derivation, List[(String, Double)])]] = {
    SBResult.singleResults.
      groupBy { case SimprocSingleRun(a, b, c) => a }.
      mapValues(_.map { case SimprocSingleRun(a, b, c) => (b, c) })
  }


  def fromJson(json: Json): SimprocBatchResult = {
    val notes: String = (json / "notes").stringValue
    val selectedSimprocs: List[String] = (json / "selected_simprocs").asArray.map(a => a.stringValue).toList
    val allSimprocs: Map[String, String] = (json / "python").asArray.flatMap(j => j.asObject.v.map(sj => sj._1 -> sj._2.stringValue)).toMap
    val results: List[SimprocSingleRun] = (json / "results").asArray.map(j => SimprocSingleRun.fromJson(j.asObject)).toList
    SimprocBatchResult(selectedSimprocs, allSimprocs, results, notes)
  }

  def lazyFromJson(json: Json): SimprocLazyBatchResult = {
    val notes: String = (json / "notes").stringValue
    val selectedSimprocs: List[String] = (json / "selected_simprocs").asArray.map(a => a.stringValue).toList
    val resultCount: Int = (json / "results").asArray.size
    SimprocLazyBatchResult(notes, selectedSimprocs, resultCount)
  }
}


// Set up the run as a SimprocBatch
case class SimprocBatch(selectedSimprocs: List[String], selectedGraphs: List[Graph], notes: String) {
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
        val result = SimprocBatchResult(selectedSimprocs,
          SimprocBatch.loadedSimprocs.map(ss => (ss._1, ss._2.sourceCode)),
          list,
          notes)
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
    val jobIDAtStart = BatchDerivationCreatorPanel.jobID

    def timeElapsed = now - startTime

    var timings: List[(String, Double)] = List()
    var derivation = new Derivation(graph)
    var parentOpt: Option[DSName] = None
    for ((graph, rule) <- simproc.simp(graph)) {
      // Stop if taking too long
      // Stop if the user has incremented the jobID (indicating they want the job to halt)
      if (timeElapsed < timeout || BatchDerivationCreatorPanel.jobID > jobIDAtStart) {
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
      val fileName = UserOptions.preferredDateTimeFormat.format(Calendar.getInstance().getTime) + ".qsbr"
      val projectRoot = QuantoDerive.CurrentProject.map(p => p.rootFolder + "/").getOrElse("")
      FileHelper.printJson(projectRoot + "batch_results/" + fileName, result.toJson)
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