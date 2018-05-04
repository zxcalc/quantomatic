package quanto.cosy

import java.io.File
import java.util.Calendar

import quanto.data.Theory.ValueType
import quanto.data._
import quanto.rewrite.{Match, Matcher}
import quanto.util.FileHelper._
import quanto.util.json.JsonObject
import quanto.util.{FileHelper, Rational}

import scala.concurrent.duration.Duration
import scala.util.matching.Regex

/**
  * This class performs the actual batch conjecture synthesis
  */
abstract class CoSyRun[S, T](
                              rulesDir: File,
                              theory: Theory,
                              duration: Duration,
                              outputDir: File,
                              makeValuesFile: Boolean
                            ) {

  val Generator: Iterator[S]
  var reductionRules: List[Rule] = List()
  var equivClasses: Map[T, Graph] = Map()

  def makeGraph(gen: S): Graph

  def makeTensor(gen: S): T

  val matchBorders : Option[Regex]

  def graphLeftBiggerRight(left: Graph, right: Graph): Boolean

  def compareTensor(a: T, b: T): Boolean

  def makeString(a: S, b: T): String

  def doWithUnmatched(a: S) : Unit

  def begin(): Unit = {
    def now(): Long = Calendar.getInstance().getTimeInMillis

    val timeStart = now()
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
        doWithUnmatched(next)
        val interpretation = makeTensor(next)
        // Need to check rough equivalence
        val similarTensors = equivClasses.keys.filter(t => compareTensor(t, interpretation))

        if (similarTensors.nonEmpty) {
          for (similar <- similarTensors) {
            // Something with that tensor exists
            val existing : Graph = equivClasses(similar)

            def borderNodes(g: Graph) : Set[VName] = g.verts.filter(vn => matchBorders.get.findFirstMatchIn(vn.s).nonEmpty)

            val overlappingNodes = borderNodes(graph).intersect(borderNodes(existing))

            val boundaryData = NodeV(JsonObject(
              "type" -> "dummyBoundary",
              "value" -> ""
            ),
              JsonObject(),
              theory)

            val constrainedMatches = if (matchBorders.nonEmpty) {
              def makeSolidBoundaries(graph: Graph): Graph = {
                graph.verts.filter(vn => matchBorders.get.findFirstIn(vn.s).nonEmpty).
                  foldLeft(graph) { (g, vn) =>
                    g.updateVData(vn) { _ => boundaryData }
                  }
              }

              // check that it isn't a duplicate of that graph
              Matcher.findMatches(makeSolidBoundaries(graph), makeSolidBoundaries(existing)).filter(
                m => overlappingNodes.forall(
                  vn => m.map.v.dom.toList.contains(vn) && m.map.v.domf(vn).contains(vn)
                )
              )
            } else {
              Stream[Match]()
            }
            if (constrainedMatches.isEmpty){
              createRule(graph, existing)
            }else{
              // Don't create a rule between isomorphic (constrained) graphs
            }
            if (graphLeftBiggerRight(existing, graph)) {
              equivClasses = equivClasses + (interpretation -> graph) // update with smaller graph
            }
          }
        } else {
          equivClasses = equivClasses + (interpretation -> graph)
          if (makeValuesFile) {
            FileHelper.printToFile(
              outputDir.toURI.resolve("./values.txt"),
              makeString(next, interpretation),
              append = true)
          }
        }
      } else {
        // Nothing to do, since it can be reduced
      }
    }
  }


  def createRule(suggested: Graph, existing: Graph): Rule = {
    val (lhs, rhs) = if (graphLeftBiggerRight(suggested, existing)) {
      (suggested, existing)
    } else {
      (existing, suggested)
    }
    val r = new Rule(lhs, rhs)
    val name = s"${suggested.hashCode}_${existing.hashCode}.qrule"
    printJson(outputDir.toURI.resolve("./" + name).getPath, Rule.toJson(r))
    loadRule(r)
    r
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

  FileHelper.readAllOfType(rulesDir.getAbsolutePath, ".*qrule", Rule.fromJson(_, theory)).foreach(loadRule)
}

object CoSyRuns {

  private val Angle = ValueType.AngleExpr

  class CoSyCircuit(rulesDir: File,
                    theory: Theory,
                    duration: Duration,
                    outputDir: File,
                    numBoundaries: Int
                   ) extends CoSyRun[BlockStack, Tensor](rulesDir, theory, duration, outputDir, makeValuesFile = true) {

    override val Generator: Iterator[BlockStack] = new Iterator[BlockStack] {

      override def hasNext: Boolean = true

      override def next(): BlockStack = {
        if (!unusedRows.hasNext) {
          if (!unusedStacks.hasNext) {
            unusedStacks = nextRoundOfStacks.toIterator
            nextRoundOfStacks = List()
          }
          unusedRows = rows.toIterator
          currentStack = unusedStacks.next()
        }
        currentStack.append(unusedRows.next())
      }
    }
    override val matchBorders = Some("(i|o)-".r)
    val blocks: List[Block] = BlockGenerators.ZXClifford
    val rows: List[BlockRow] = BlockRowMaker.makeRowsOfSize(numBoundaries, blocks, Some(numBoundaries))
    var unusedRows: Iterator[BlockRow] = rows.toIterator
    var unusedStacks: Iterator[BlockStack] = rows.map(r => BlockStack(List(r))).toIterator
    var nextRoundOfStacks: List[BlockStack] = List()
    var currentStack: BlockStack = unusedStacks.next()

    override def doWithUnmatched(a: BlockStack): Unit = {
      nextRoundOfStacks = a :: nextRoundOfStacks
    }

    override def compareTensor(a: Tensor, b: Tensor): Boolean = a.isRoughlyUpToScalar(b)

    override def graphLeftBiggerRight(left: Graph, right: Graph): Boolean = {

      def phase(vdata: VData): PhaseExpression =
        vdata match {
          case NodeV(d, a, t) => vdata.asInstanceOf[NodeV].phaseData.first[PhaseExpression](ValueType.AngleExpr) match {
            case Some(p) => p
            case None => PhaseExpression.zero(ValueType.AngleExpr)
          }
          case _ => PhaseExpression.zero(ValueType.AngleExpr)
        }


      // Number of T-gates
      def countT(graph: Graph): Int = graph.vdata.count(nd => phase(nd._2).constant == Rational(1, 4))

      val tDiff = countT(left) - countT(right)
      if (tDiff > 0) {
        return true
      }
      if (tDiff < 0) {
        return false
      }

      // Number of nodes
      def nodes(graph: Graph): Int = graph.vdata.size

      val nodeDiff = nodes(left) - nodes(right)
      if (nodeDiff > 0) {
        return true
      }
      if (nodeDiff < 0) {
        return false
      }

      // Number of edges
      def edges(graph: Graph): Int = graph.edata.size

      val edgeDiff = edges(left) - edges(right)
      if (edgeDiff > 0) {
        return true
      }
      if (edgeDiff < 0) {
        return false
      }

      // Number of "Z" nodes
      // We favour these!
      // Purely for aesthetic reasons
      def countZ(graph: Graph): Int = graph.vdata.count(nd => nd._2.typ == "Z")

      val zDiff = countZ(left) - countZ(right)
      if (zDiff < 0) {
        return true
      }
      if (zDiff > 0) {
        return false
      }

      // sum of the phases
      def phaseSum(graph: Graph): PhaseExpression = graph.vdata.map(nd => phase(nd._2)).
        foldLeft(PhaseExpression.zero(Angle)) { (s, a) => s + a }

      val phaseDiff = (phaseSum(left) - phaseSum(right)).constant
      if (phaseDiff > 0) {
        return true
      }
      if (phaseDiff < 0) {
        return false
      }

      false
    }

    override def makeTensor(gen: BlockStack): Tensor = gen.tensor

    override def makeString(a: BlockStack, b: Tensor): String = s"$a: ${b.toJson},"

    override def makeGraph(gen: BlockStack): Graph = {
      val g = gen.graph.minimise
      val IOPattern = raw"r-\d+-(i|o)-(\d+)".r
      val renameMap : Map[VName, VName] = g.verts.map(vn => vn -> (vn.s match {
        case IOPattern(io, n) => VName(io + "-" + n)
        case _ => vn
      })).toMap
      g.rename(renameMap)
    }

  }

  class CoSyZX(rulesDir: File,
               theory: Theory,
               duration: Duration,
               outputDir: File,
               numAngles: Int,
               numBoundaries: List[Int],
               numVertices: Int,
               scalars: Boolean
              ) extends CoSyRun[AdjMat, Tensor](rulesDir, theory, duration, outputDir, makeValuesFile = true) {


    override val Generator: Iterator[AdjMat] =
      (ColbournReadEnum.enumerate(1, 1, numBoundaries.max, 0).iterator ++
        ColbournReadEnum.enumerate(numAngles, numAngles, numBoundaries.max, numVertices).iterator).
        filter(a => numBoundaries.contains(a.numBoundaries))
    private val gdata = (for (i <- 0 until numAngles) yield {
      NodeV(data = JsonObject("type" -> "Z", "value" -> angleMap(i).toString), theory = theory)
    }).toVector
    private val rdata = (for (i <- 0 until numAngles) yield {
      NodeV(data = JsonObject("type" -> "X", "value" -> angleMap(i).toString), theory = theory)
    }).toVector

    override def compareTensor(a: Tensor, b: Tensor): Boolean = if (!scalars) {
      a.isRoughlyUpToScalar(b)
    } else {
      a.isRoughly(b)
    }

    override val matchBorders = None

    private implicit def stringToPhase(s: String): PhaseExpression = {
      PhaseExpression.parse(s, ValueType.AngleExpr)
    }

    override def doWithUnmatched(a: AdjMat): Unit = {
      // Don't need to do anything, since Colbourn-Read handles generating adj-mats
    }

    override def makeTensor(gen: AdjMat): Tensor = Interpreter.interpretZXGraph(makeGraph(gen))

    override def makeGraph(gen: AdjMat): Graph = Graph.fromAdjMat(gen, rdata, gdata)

    override def makeString(a: AdjMat, b: Tensor): String = {
      s"adj${a.hash}: ${b.toJson},"
    }

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
      if (XAngles % (2 * Pi) > Pi) {
        return true
      }
      if (XAngles % (2 * Pi) < Pi) {
        return false
      }


      false
    }

    private def angleMap = (x: Int) => PhaseExpression(new Rational(x, numAngles), ValueType.AngleExpr)


  }

}