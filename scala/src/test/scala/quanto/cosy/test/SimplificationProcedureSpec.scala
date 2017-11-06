package quanto.cosy.test

import java.io.File

import quanto.cosy._
import org.scalatest.FlatSpec
import quanto.data._
import quanto.data.Derivation.DerivationWithHead
import quanto.cosy.RuleSynthesis._
import quanto.data

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Random, Success}

/**
  * Created by hector on 28/06/17.
  */

class SimplificationProcedureSpec extends FlatSpec {
  implicit val rg: Theory = Theory.fromFile("red_green")

  val examplesDirectory = "../examples/"
  val ZXRules: List[Rule] = loadRuleDirectory(examplesDirectory + "ZX_CliffordT")
  val ZXErrorRules: List[Rule] = loadRuleDirectory(examplesDirectory + "ZX_errors")

  val waitTime = 6000 // seconds

  behavior of "Handler"

  it should "handle annealing" in {

    val ctRules = ZXRules
    val target = ctRules.filter(_.name.matches(raw"RED.*")).head.lhs
    //val remaining = ctRules.filter(_.name.matches(raw"S\d+.*"))
    import SimplificationProcedure.Annealing._
    val initialState: State = new State(
      ctRules,
      0,
      Some(20),
      new Random(),
      3,
      None)
    val simplificationProcedure = new SimplificationProcedure[State](
      (new Derivation(rg, target), None),
      initialState,
      step,
      progress,
      (_, state) => state.currentStep == state.maxSteps.get
    )
    var returningDerivation = simplificationProcedure.initialDerivation
    val backgroundDerivation: Future[DerivationWithHead] = Future[DerivationWithHead] {
      //println("future started")
      while (!simplificationProcedure.stopped) {
        //println("futured loop")
        simplificationProcedure.step()
        println(simplificationProcedure.state.currentStep)
        returningDerivation = simplificationProcedure.current
      }
      simplificationProcedure.current
    }
    backgroundDerivation onComplete {
      case Success(_) =>
        println(returningDerivation)
        assert(true)
      case Failure(_) =>
        assert(false)
    }
    Await.result(backgroundDerivation, Duration(10, "seconds"))
  }

  it should "evaluate a graph" in {

    val ctRules = ZXRules
    val target = ctRules.filter(_.name.matches(raw"S1.*")).head.lhs
    val t1 = raw"\beta"
    val targetString: String = t1.replaceAll(raw"\\", raw"\\\\")
    val replacementString: String = raw"\pi".replaceAll(raw"\\", raw"\\\\")
    val initialDerivation = graphToDerivation(target, rg)
    import SimplificationProcedure.Evaluation._
    if (targetString.length > 0) {

      val initialState: State = new State(
        List(),
        0,
        Some(initialDerivation.verts.size),
        new Random(),
        initialDerivation.verts.toList,
        targetString,
        replacementString,
        Some(initialDerivation.verts.size)
      )
      val simplificationProcedure = new SimplificationProcedure[State](
        initialDerivation,
        initialState,
        step,
        progress,
        (_, state) => state.currentStep == state.maxSteps.get
      )
      var returningDerivation = simplificationProcedure.initialDerivation
      val backgroundDerivation: Future[DerivationWithHead] = Future[DerivationWithHead] {
        //println("future started")
        while (!simplificationProcedure.stopped) {
          //println("futured loop")
          simplificationProcedure.step()
          returningDerivation = simplificationProcedure.current
        }
        simplificationProcedure.current
      }
      backgroundDerivation onComplete {
        case Success(_) =>
          println(data.Derivation.derivationHeadPairToGraph(returningDerivation).vdata)
          assert(true)
        case Failure(_) =>
          assert(false)
      }
      Await.result(backgroundDerivation, Duration(waitTime, "seconds"))
    }
  }

  it should "perform LTE simplifications" in {
    val allowedRules = ZXErrorRules
    val targetGraph = quanto.util.FileHelper.readFile[Graph](
      new File(examplesDirectory + "ZX_errors/ErrorGate.qgraph"),
      Graph.fromJson(_, rg)
    )
    val initialDerivation: DerivationWithHead = graphToDerivation(targetGraph, rg)
    val graph = Derivation.derivationHeadPairToGraph(initialDerivation)
    val boundaries = graph.verts.filter(v => graph.vdata(v).isBoundary)
    import SimplificationProcedure.LTEByWeight._
    val targets = boundaries.filter(t => t.toString.matches("b[345]")).toList
    if (targets.nonEmpty) {
      val initialState: State = State(
        allowedRules,
        0,
        Some(50),
        new Random(),
        quanto.cosy.GraphAnalysis.distanceOfErrorsFromEnds(targets),
        None,
        heldVertices = None,
        None
      )
      val simplificationProcedure = new SimplificationProcedure[State](
        initialDerivation,
        initialState,
        step,
        progress,
        (_, state) => state.currentStep == state.maxSteps.getOrElse(-1) || state.currentDistance.getOrElse(1) == 0
      )
      var returningDerivation = simplificationProcedure.initialDerivation
      val backgroundDerivation: Future[DerivationWithHead] = Future[DerivationWithHead] {
        //println("future started")
        while (!simplificationProcedure.stopped) {
          //println("futured loop")
          simplificationProcedure.step()
          returningDerivation = simplificationProcedure.current
        }
        simplificationProcedure.current
      }
      backgroundDerivation onComplete {
        case Success(_) =>
          println(data.Derivation.derivationHeadPairToGraph(returningDerivation).vdata)
          assert(true)
        case Failure(_) =>
          assert(false)
      }
      Await.result(backgroundDerivation, Duration(waitTime, "seconds"))
    }
  }

  it should "pull errors in the small example" in {
    val allowedRules = ZXErrorRules
    val targetGraph = quanto.util.FileHelper.readFile[Graph](
      new File(examplesDirectory + "ZX_errors/ErrorGate.qgraph"),
      Graph.fromJson(_, rg)
    )
    val initialDerivation: DerivationWithHead = graphToDerivation(targetGraph, rg)
    val graph = Derivation.derivationHeadPairToGraph(initialDerivation)
    val boundaries = graph.verts.filter(v => graph.vdata(v).isBoundary)
    import SimplificationProcedure.PullErrors._
    val targets = boundaries.filter(t => t.toString.matches(raw"b[3-5]")).toList
    if (targets.nonEmpty) {
      val initialState: State = State(
        allowedRules.filterNot(r => r.name.matches(raw".*g_ann")),
        0,
        None,
        new Random(),
        weightFunction = errorsDistance(targets.toSet),
        Some(ZXErrorRules.filter(r => r.name.matches(raw".*g_ann"))) ,
        None,
        heldVertices = None,
        None
      )
      val simplificationProcedure = new SimplificationProcedure[State](
        initialDerivation,
        initialState,
        step,
        progress,
        (_, state) => (state.currentStep == state.maxSteps.getOrElse(-1)) || (state.currentDistance.getOrElse(2.0) < 1)
      )
      var returningDerivation = simplificationProcedure.initialDerivation
      val backgroundDerivation: Future[DerivationWithHead] = Future[DerivationWithHead] {
        //println("future started")
        while (!simplificationProcedure.stopped) {
          //println("futured loop")
          simplificationProcedure.step()
          returningDerivation = simplificationProcedure.current
        }
        simplificationProcedure.current
      }
      backgroundDerivation onComplete {
        case Success(d) =>
          println("Success")
          println(data.Derivation.derivationHeadPairToGraph(d).vdata)
          assert(errorsDistance(targets.toSet)(d, GraphAnalysis.detectErrors(d)).get < 1)
        case Failure(_) =>
          assert(false)
      }
      Await.result(backgroundDerivation, Duration(waitTime, "seconds"))
    }
  }

  // AK: This one takes too long to run as a unit test.
  // TODO: find a better solution for one-off tests like this.

  ignore should "perform large error pull simplifications" in {
    val allowedRules = ZXErrorRules
    val targetGraph = quanto.util.FileHelper.readFile[Graph](
      new File(examplesDirectory + "ZX_errors/Huge_With_Error.qgraph"),
      Graph.fromJson(_, rg)
    )
    val initialDerivation: DerivationWithHead = graphToDerivation(targetGraph, rg)
    val graph = Derivation.derivationHeadPairToGraph(initialDerivation)
    val boundaries = graph.verts.filter(v => graph.vdata(v).isBoundary)
    import SimplificationProcedure.PullErrors._
    val targets = boundaries.filter(t => t.toString.matches(raw"b(1[1-9]|2\d)")).toList
    if (targets.nonEmpty) {
      val initialState: State = State(
        allowedRules.filterNot(r => r.name.matches(raw".*g_ann")),
        0,
        None,
        new Random(),
        weightFunction = errorsDistance(targets.toSet),
        Some(ZXErrorRules.filter(r => r.name.matches(raw".*g_ann"))),
        None,
        heldVertices = None,
        None
      )
      val simplificationProcedure = new SimplificationProcedure[State](
        initialDerivation,
        initialState,
        step,
        progress,
        (_, state) => state.currentStep == state.maxSteps.getOrElse(-1) || state.currentDistance.getOrElse(2.0) < 1
      )
      var returningDerivation = simplificationProcedure.initialDerivation
      val backgroundDerivation: Future[DerivationWithHead] = Future[DerivationWithHead] {
        //println("future started")
        while (!simplificationProcedure.stopped) {
          //println("futured loop")
          simplificationProcedure.step()
          returningDerivation = simplificationProcedure.current
        }
        simplificationProcedure.current
      }
      backgroundDerivation onComplete {
        case Success(_) =>
          println("Success")
          println(simplificationProcedure.state.currentDistance)
          println(data.Derivation.derivationHeadPairToGraph(returningDerivation).vdata)
          assert(true)
        case Failure(_) =>
          assert(false)
      }
      Await.result(backgroundDerivation, Duration(waitTime, "seconds"))
    }
  }


}