package quanto.cosy

import java.io.File
import java.util.Calendar

import quanto.cosy.Interpreter.{ZXAngleData, interpretZXSpider}
import quanto.data.Theory.{ValueType, VertexDesc}
import quanto.data._
import quanto.rewrite.{Match, Matcher}
import quanto.util.FileHelper._
import quanto.util.json.{Json, JsonObject}
import quanto.util.{FileHelper, Rational, UserAlerts}

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

  def checkIsomorphic(graph1: Graph, graph2: Graph): Boolean =
    GraphAnalysis.checkIsomorphic(theory, Some(matchBorders.getOrElse("".r)))(graph1,graph2)

  // Positive if left bigger than right:
  def compareGraph(left: Graph, right: Graph): Int

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
              if (compareGraph(existing, nextGraph) > 0) {
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
                if (compareGraph(existing, nextGraph) > 0) {
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

  def createRule(lhs: Graph, rhs: Graph): Rule = {
    val name = s"${lhs.hashCode}_${rhs.hashCode}.qrule"
    val r = new Rule(lhs, rhs, derivation = Some("CoSy"), description = RuleDesc(name))
    if (outputDir.nonEmpty) {
      printJson(outputDir.get.toURI.resolve("./" + name).getPath, Rule.toJson(r, lhs.data.theory))
    }
    loadRule(r)
    r
  }

  def loadRule(rule: Rule): Unit = {
    def reduceRules(rules: List[Rule]) : List[Rule] = {
      RuleSynthesis.greedyReduceRules(compareGraph, Some((theory, matchBorders)))(rules).filter(
        rule => !checkIsomorphic(rule.lhs, rule.rhs)
      )
    }
    // Please don't put bbox rules into here unless you really mean them to be here and they reduce left->right
    if (rule.lhs.bboxes.nonEmpty) {
      reductionRules = rule :: reductionRules
    } else {
      // No bboxes, act normally
      if (compareGraph(rule.lhs, rule.rhs) > 0) {
        reductionRules = rule :: reductionRules
        reductionRules = reduceRules(reductionRules)
      } else if (compareGraph(rule.rhs, rule.lhs) > 0) {
        reductionRules = rule.inverse :: reductionRules
        reductionRules = reduceRules(reductionRules)
      } else {
        // Not a reduction rule, so leave it out
      }
    }
  }

  FileHelper.readAllOfType(rulesDir.getAbsolutePath, ".*qrule", Rule.fromJson(_, theory)).foreach(loadRule)
}

object CoSyRuns {


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
            UserAlerts.alert("Finished next row")
          }
          unusedRows = rows.toIterator
          currentStack = unusedStacks.next()
        }
        currentStack.append(unusedRows.next())
      }
    }
    override val matchBorders = Some(raw"""(i|o)-(\d+)""".r)
    val blocks: List[Block] = BlockGenerators.ZXGates(4, numBoundaries)
    UserAlerts.alert(s"Created ${blocks.length} blocks")
    val rows: List[BlockRow] = BlockRowMaker.makeRowsUpToSize(numBoundaries, blocks, Some(numBoundaries))
      .filter(br => br.inputs.size == numBoundaries && br.outputs.size == numBoundaries)
    UserAlerts.alert(s"Created ${rows.length} rows")
    var unusedRows: Iterator[BlockRow] = rows.toIterator
    var unusedStacks: Iterator[BlockStack] = rows.map(r => BlockStack(List(r))).toIterator
    var nextRoundOfStacks: List[BlockStack] = List()
    var currentStack: BlockStack = unusedStacks.next()

    override def doWithUnmatched(a: BlockStack): Unit = {
      nextRoundOfStacks = a :: nextRoundOfStacks
    }


    override def compareTensor(a: Tensor, b: Tensor): Boolean = a.isRoughlyUpToScalar(b)

    override def compareGraph(left: Graph, right: Graph) : Int = GraphAnalysis.zxCircuitCompare(left, right)

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


    override val Generator: Iterator[AdjMat] = {
      val identitiesFirst  = ColbournReadEnum.enumerate(1, 1, numBoundaries.max, 0).
        filter(a => numBoundaries.contains(a.numBoundaries))

      val CR = ColbournReadEnum.enumerate(numAngles, numAngles, numBoundaries.max, numVertices)

      UserAlerts.alert(s"CoSy: Finished Colbourn-Read (${CR.size})")

      val CRScalars = if(scalars) CR else CR.filter(adj => !GraphAnalysis.containsScalars(adj))

      UserAlerts.alert(s"CoSy: Filtered out scalars (${CRScalars.size})")

      val CRScalarsSorted = CRScalars.sortBy(_.size)

      UserAlerts.alert("CoSy: Sorted AdjMats")

      val combined = identitiesFirst.iterator ++
        CRScalarsSorted.iterator.filter(a => numBoundaries.contains(a.numBoundaries))

      combined
    }

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

    override def compareGraph(left: Graph, right: Graph) : Int = GraphAnalysis.zxGraphCompare(left, right)

    private def angleMap = (x: Int) => PhaseExpression(new Rational(2 * x, numAngles), ValueType.AngleExpr)


  }

  class CoSyZXBool(rulesDir: File,
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
        ColbournReadEnum.enumerate(2*numAngles, 2*numAngles, numBoundaries.max, numVertices).iterator).
        filter(a => numBoundaries.contains(a.numBoundaries))



    private val gdata = (for (i <- 0 until 2*numAngles) yield {
      NodeV(data = JsonObject("type" -> "Z", "value" -> angleMap(i).toString), theory = theory)
    }).toVector
    private val rdata = (for (i <- 0 until 2*numAngles) yield {
      NodeV(data = JsonObject("type" -> "X", "value" -> angleMap(i).toString), theory = theory)
    }).toVector

    override def compareTensor(a: Tensor, b: Tensor): Boolean = if (!scalars) {
      a.isRoughlyUpToScalar(b)
    } else {
      a.isRoughly(b)
    }

    override val matchBorders = None

    private implicit def stringToPhase(s: String): PhaseExpression = {
      CompositeExpression.parseKnowingTypes(s, Vector(ValueType.AngleExpr, ValueType.Boolean))(1)
    }

    override def doWithUnmatched(a: AdjMat): Unit = {
      // Don't need to do anything, since Colbourn-Read handles generating adj-mats
    }

    override def makeTensor(gen: AdjMat): Tensor = {
      val asGraph = makeGraph(gen)

      def spiderInterpreter(vdata: NodeV, inputs: Int, outputs: Int): Tensor = {

        val zxData: ZXAngleData = {
          val isGreen = vdata.typ == "Z"
          val angle = CompositeExpression.parseKnowingTypes(vdata.value, Vector(ValueType.AngleExpr, ValueType.Boolean))
          if (angle(1).constant == Rational(1, 1)) {
            ZXAngleData(isGreen, angle(0))
          } else {
            ZXAngleData(isGreen, PhaseExpression(new Rational(0, 1), ValueType.AngleExpr))
          }
        }

        interpretZXSpider(zxData, inputs, outputs)
      }


      Interpreter.interpretSpiderGraph(spiderInterpreter)(asGraph, asGraph.verts.filter(asGraph.isTerminalWire).toList.sortBy(_.s), List())
    }

    override def makeGraph(gen: AdjMat): Graph = Graph.fromAdjMat(gen, rdata, gdata)

    override def makeString(a: AdjMat, b: Tensor): String = {
      s"adj${a.hash}: ${b.toJson},"
    }


    //TODO: Graph comparison for ZXBool
    override def compareGraph(left: Graph, right: Graph) : Int = RuleSynthesis.basicGraphComparison(left, right)

    private def angleMap(x: Int): CompositeExpression =
      if (x < numAngles) {
        CompositeExpression(Vector(ValueType.AngleExpr, ValueType.Boolean),

          Vector(PhaseExpression(new Rational(2 * x, numAngles), ValueType.AngleExpr),
            PhaseExpression(new Rational(1, 1), ValueType.Boolean)))

      } else {
        CompositeExpression(Vector(ValueType.AngleExpr, ValueType.Boolean),

          Vector(PhaseExpression(new Rational(2 * (x-numAngles), numAngles), ValueType.AngleExpr),
            PhaseExpression(new Rational(0, 1), ValueType.Boolean)))

      }


  }
}

