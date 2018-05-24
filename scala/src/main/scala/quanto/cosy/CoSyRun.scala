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
                              outputDir: Option[File]
                            ) {

  val Generator: Iterator[S]
  var reductionRules: List[Rule] = List()
  var equivClasses: Map[T, Graph] = Map()

  // Turn your generator into a graph
  def makeGraph(gen: S): Graph

  // Turn your generator into a tensor
  def makeTensor(gen: S): T

  // If you want to check for isomorphisms specify a regex to match the boundaries here
  val matchBorders: Option[Regex]

  // Compare two graphs. Strictly bigger than.
  def graphLeftBiggerRight(left: Graph, right: Graph): Boolean

  // See if two tensors should be considered equivalent
  // e.g. isRoughly or isRoughlyUpToScalar
  def compareTensor(a: T, b: T): Boolean

  def findClassesCloseTo(tensor: T): Map[T, Graph] = equivClasses.filter(tv => compareTensor(tv._1, tensor))

  // How to store the values in values.txt
  def makeString(a: S, b: T): String

  // what to do with graphs that aren't hit by reduction rules
  // e.g. for circuits put that circuit into the pile to be added to for next iteration
  def doWithUnmatched(a: S): Unit

  // Core loop.
  // Comes with option of time restriction.
  def begin(): List[Rule] = {
    def now(): Long = Calendar.getInstance().getTimeInMillis

    val timeStart = now()
    while (Duration(now() - timeStart, "millis") < duration && Generator.hasNext) {
      // Get a graph
      val next: S = Generator.next()
      val nextGraph = makeGraph(next)

/*
// Print out each graph made
      if (outputDir.nonEmpty) {
        FileHelper.printToFile(
          outputDir.get.toURI.resolve("./" + nextGraph.hashCode + ".qgraph"),
          Graph.toJson(nextGraph).toString,
          append = true)
      }
      */

      val matchesReductionRule = reductionRules.exists(rule => Matcher.findMatches(rule.lhs, nextGraph).nonEmpty)

      // Check if it can be reduced by known rules
      if (!matchesReductionRule) {
        val interpretation = makeTensor(next)
        val nearbyClasses = findClassesCloseTo(interpretation)

        nearbyClasses.size match {
          case 0 =>
            // doesn't fit into any existing class
            doWithUnmatched(next)
            equivClasses = equivClasses + (interpretation -> nextGraph)
            updateValuesFile(next, interpretation)
          case 1 =>
            // Something with that tensor exists
            val equivClass = nearbyClasses.head
            val existing: Graph = equivClass._2


            // Don't create a rule between isomorphic (constrained at the boundary) graphs
            val isomorphic = if (matchBorders.nonEmpty) {
              checkIsomorphic(nextGraph, existing)
            } else {
              false
            }


            if (!isomorphic) {
              doWithUnmatched(next)
              if (graphLeftBiggerRight(existing, nextGraph)) {
                // update class with smaller graph
                equivClasses = equivClasses + (equivClass._1 -> nextGraph)
                createRule(existing, nextGraph)
              } else {
                // new graph is at least as big as current
                // keep status quo, create rule
                createRule(nextGraph, existing)
              }
            }

          case n =>
            // Somehow in the approximation radius of two classes
            // For now, add to both.
            for (equivClass <- nearbyClasses) {
              // Something with that tensor exists
              val existing: Graph = equivClass._2


              val isomorphic = if (matchBorders.nonEmpty) {
                checkIsomorphic(nextGraph, existing)
              } else {
                false
              }


              if (!isomorphic) {
                doWithUnmatched(next)
                createRule(nextGraph, existing)
                // update class with smaller graph
                if (graphLeftBiggerRight(existing, nextGraph)) {
                  equivClasses = equivClasses + (equivClass._1 -> nextGraph)
                }
              } else {
                // Don't create a rule between isomorphic (constrained) graphs
              }
            }
        }
      }
    }
    reductionRules
  }

  private def updateValuesFile(next: S, interpretation: T): Unit = {
    if (outputDir.nonEmpty) {
      FileHelper.printToFile(
        outputDir.get.toURI.resolve("./values.txt"),
        makeString(next, interpretation),
        append = true)
    }
  }

  private def checkIsomorphic(g1: Graph, g2: Graph): Boolean = {
    // Check whether graphs are isomorphic, after constraining their boundaries

    def borderNodes(g: Graph): Set[VName] = g.verts.filter(vn => vn.s.matches(matchBorders.get.regex))

    val overlappingNodes = borderNodes(g1).intersect(borderNodes(g2))

    // TODO:
    // This is a hack, because it requires the theory to have "dummyBoundary" nodes
    val boundaryData = NodeV(JsonObject(
      "type" -> "dummyBoundary",
      "value" -> ""
    ),
      JsonObject(),
      theory)

    def makeSolidBoundaries(graph: Graph): Graph = {
      graph.verts.filter(vn => matchBorders.get.findFirstIn(vn.s).nonEmpty).
        foldLeft(graph) { (g, vn) =>
          g.updateVData(vn) { _ => boundaryData }
        }
    }

    val solid1 = makeSolidBoundaries(g1)
    val solid2 = makeSolidBoundaries(g2)

    // check that it isn't a duplicate of that graph
    val matches12 = Matcher.findMatches(solid1, solid2).filter(
      m => overlappingNodes.forall(
        vn => m.map.v.dom.toList.contains(vn) && m.map.v.domf(vn).contains(vn)
      )
    )

    val matches21 = Matcher.findMatches(solid2, solid1).filter(
      m => overlappingNodes.forall(
        vn => m.map.v.dom.toList.contains(vn) && m.map.v.domf(vn).contains(vn)
      )
    )

    matches21.nonEmpty && matches12.nonEmpty
  }

  def createRule(lhs: Graph, rhs: Graph): Rule = {
    val name = s"${lhs.hashCode}_${rhs.hashCode}.qrule"
    val r = new Rule(lhs, rhs, derivation = Some("CoSy"), description = RuleDesc(name))
    if (outputDir.nonEmpty) {
      printJson(outputDir.get.toURI.resolve("./" + name).getPath, Rule.toJson(r))
    }
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
                    outputDir: Option[File],
                    numBoundaries: Int
                   ) extends CoSyRun[BlockStack, Tensor](rulesDir, theory, duration, outputDir) {


    // Include the empty diagram
    // equivClasses = equivClasses + (Tensor(Array(Array(1))) -> new Graph())

    override val Generator: Iterator[BlockStack] = new Iterator[BlockStack] {

      override def hasNext: Boolean = unusedStacks.hasNext || unusedRows.hasNext || nextRoundOfStacks.nonEmpty

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
    override val matchBorders = Some(raw"""(i|o)-(\d+)""".r)
    val blocks: List[Block] = BlockGenerators.ZXCNOT
    val rows: List[BlockRow] = BlockRowMaker.makeRowsUpToSize(numBoundaries, blocks, Some(numBoundaries))
      .filter(br => br.inputs.size == numBoundaries && br.outputs.size == numBoundaries)
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

      // position

      val positionPattern: Regex = raw"r-(\d+)-bl-(\d+)-\w+-\d+".r

      def weightByName(vName: VName): Int = {
        def int(string: String): Int = string.toInt

        vName.s match {
          case positionPattern(a, b) => (113 * int(a)) + (11 * int(b))
          case _ => 0
        }
      }

      def weightByNameOfGraph(graph: Graph): Int = graph.verts.map(weightByName).sum


      val weightDiff = weightByNameOfGraph(left) - weightByNameOfGraph(right)
      if (weightDiff > 0) {
        return true
      }
      if (weightDiff < 0) {
        return false
      }


      false
    }

    override def makeTensor(gen: BlockStack): Tensor = gen.tensor

    override def makeString(a: BlockStack, b: Tensor): String = s"$a: ${b.toJson},"

    override def makeGraph(gen: BlockStack): Graph = {
      val g = gen.graph.minimise
      val IOPattern = raw"r-\d+-(i|o)-(\d+)".r
      val renameMap: Map[VName, VName] = g.verts.map(vn => vn -> (vn.s match {
        case IOPattern(io, n) => VName(io + "-" + n)
        case _ => vn
      })).toMap
      g.rename(renameMap)
    }

  }

  class CoSyZX(rulesDir: File,
               theory: Theory,
               duration: Duration,
               outputDir: Option[File],
               numAngles: Int,
               numBoundaries: List[Int],
               numVertices: Int,
               scalars: Boolean
              ) extends CoSyRun[AdjMat, Tensor](rulesDir, theory, duration, outputDir) {


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

    override def makeTensor(gen: AdjMat): Tensor = {
      val asGraph = makeGraph(gen)
      Interpreter.interpretZXGraph(asGraph, asGraph.verts.filter(asGraph.isTerminalWire).toList.sortBy(_.s), List())
    }

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

    private def angleMap = (x: Int) => PhaseExpression(new Rational(2*x, numAngles), ValueType.AngleExpr)


  }

}