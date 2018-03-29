package quanto.cosy

import java.nio.file.Path
import java.util.Calendar
import java.util.concurrent.TimeUnit

import quanto.cosy.Interpreter.ZXAngleData
import quanto.data.Theory.ValueType
import quanto.util.FileHelper._
import quanto.data._
import quanto.rewrite.{Matcher, Rewriter}
import quanto.util.Rational
import quanto.util.json.JsonObject

import scala.concurrent.duration.Duration

/**
  * This class performs the actual batch conjecture synthesis
  */
abstract class CoSyRun[S, T](
                              startingRules: List[Rule],
                              duration: Duration,
                              outputDir: String
                            ) {

  val Generator: Iterator[S]
  var reductionRules: List[Rule] = List()

  def makeGraph(gen: S): Graph

  def makeTensor(gen: S): T

  def graphLeftBiggerRight(left: Graph, right: Graph): Boolean

  def begin(): Unit = {
    def now(): Long = Calendar.getInstance().getTimeInMillis

    val timeStart = now()
    var equivClasses: Map[T, Graph] = Map()
    while (Duration(now() - timeStart, "millis") < duration && Generator.hasNext) {
      // Get a graph
      val next: S = Generator.next()
      val graph = makeGraph(next)
      var matchesReductionRule: Boolean = false
      for (rule <- reductionRules) {
        if (Matcher.findMatches(rule.lhs, graph).nonEmpty) {
          matchesReductionRule = true
        }
      }

      if (!matchesReductionRule) {
        val interpretation = makeTensor(next)
        if (equivClasses.contains(interpretation)) {
          // Something with that tensor exists
          createRule(graph, equivClasses(interpretation))
        } else {
          equivClasses = equivClasses + (interpretation -> graph)
        }
      } else {
        // Nothing to do, since it can be reduced
      }
    }
  }


  def createRule(a: Graph, b: Graph): Unit = {
    val (lhs, rhs) = if (graphLeftBiggerRight(a, b)) {
      (a, b)
    } else {
      (b, a)
    }
    val r = new Rule(lhs, rhs)
    val name = s"${a.hashCode}-${b.hashCode}.qrule"
    printJson(outputDir + "/" + name, Rule.toJson(r))
    loadRule(r)
  }

  def loadRule(rule: Rule): Unit = {
    // Please don't put bbox rules into here unless you really mean them to be here and they reduce left->right
    if (rule.lhs.bboxes.nonEmpty) {
      reductionRules = rule :: reductionRules
    } else {
      // No bboxes, act normally
      if (graphLeftBiggerRight(rule.lhs, rule.rhs)) {
        reductionRules = rule :: reductionRules
      } else if (graphLeftBiggerRight(rule.rhs, rule.lhs)) {
        reductionRules = rule.inverse :: reductionRules
      } else {
        // Not a reduction rule, so leave it out
      }
    }
  }

  startingRules.foreach(loadRule)
}

object CoSyRuns {

  class CoSyZX(startingRules: List[Rule],
               duration: Duration,
               outputDir: String,
               numAngles: Int,
               numBoundaries: Int,
               numVertices: Int
              ) extends CoSyRun[AdjMat, Tensor](startingRules, duration, outputDir) {


    override val Generator: Iterator[AdjMat] =
      ColbournReadEnum.enumerate(numAngles, numAngles, numBoundaries, numVertices).iterator
    private val theory = Theory.fromFile("red_green")
    private val gdata = (for (i <- 0 until numAngles) yield {
      NodeV(data = JsonObject("type" -> "Z", "value" -> angleMap(i).toString), theory = theory)
    }).toVector
    private val rdata = (for (i <- 0 until numAngles) yield {
      NodeV(data = JsonObject("type" -> "X", "value" -> angleMap(i).toString), theory = theory)
    }).toVector


    private implicit def stringToPhase(s : String) : PhaseExpression = {
      PhaseExpression.parse(s, ValueType.AngleExpr)
    }


    override def makeTensor(gen: AdjMat): Tensor = Interpreter.interpretZXGraph(makeGraph(gen))

    override def makeGraph(gen: AdjMat): Graph = Graph.fromAdjMat(gen, rdata, gdata)

    override def graphLeftBiggerRight(left: Graph, right: Graph): Boolean = {
      // First count number of nodes
      def nodes(graph: Graph): Int = graph.vdata.size

      val node = nodes(left) - nodes(right)
      if (node > 0) {
        return true
      }
      if (node < 0) {
        return false
      }

      // Number of edges
      def edges(graph: Graph): Int = graph.edata.size

      val edge = edges(left) - edges(right)
      if (edge > 0) {
        return true
      }
      if (edge < 0) {
        return false
      }

      // Number of "Z" nodes
      def countZ(graph: Graph): Int = graph.vdata.count(nd => nd._2.typ == "Z")

      val zDiff = countZ(left) - countZ(right)
      if (zDiff > 0) {
        return true
      }
      if (zDiff < 0) {
        return false
      }

      // Sum of Z angles
      def toAngle(nv: NodeV): ZXAngleData = ZXAngleData(nv.typ == "Z", nv.value)

      val Pi = math.Pi

      def sumAngles(graph: Graph, filterType: String): PhaseExpression = graph.vdata.
        filter(nd => nd._2.typ == filterType).
        foldLeft(PhaseExpression.zero(ValueType.AngleExpr)) {
          (angle, nd) => angle + nd._2.asInstanceOf[NodeV].value
        }

      val ZAngles: Rational = sumAngles(left, "Z").constant - sumAngles(right, "Z").constant
      if (ZAngles > 0) {
        return true
      }
      if (ZAngles < 0) {
        return false
      }

      // Sum of X angles

      val XAngles: Rational = sumAngles(left, "X").constant - sumAngles(right, "X").constant
      if (XAngles % (2*Pi) > Pi) {
        return true
      }
      if (XAngles % (2*Pi) < Pi) {
        return false
      }


      false
    }

    private def angleMap = (x: Int) => PhaseExpression(new Rational(x,numAngles), ValueType.AngleExpr)


  }

}