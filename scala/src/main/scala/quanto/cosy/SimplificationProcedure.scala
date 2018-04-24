package quanto.cosy

import quanto.data.Derivation.{DerivationWithHead, derivationHeadPairToGraph}
import quanto.data._

import scala.util.Random


class SimplificationProcedure[T <: SimplificationProcedure.SimplificationState]
(val initialDerivation: DerivationWithHead,
 val initialState: T,
 stepAction: (DerivationWithHead, T) => (DerivationWithHead, T),
 measureProgress: (DerivationWithHead, T) => SimplificationProcedure.ProgressUpdate,
 haltCondition: (DerivationWithHead, T) => Boolean
) {

  var state: T = initialState
  var current: DerivationWithHead = initialDerivation
  var progress: SimplificationProcedure.ProgressUpdate = (Some("Initialising"), None)
  var stopped: Boolean = false

  def step(): SimplificationProcedure.ProgressUpdate = {
    if (!stopped) {
      val returned = stepAction(current, state)
      current = returned._1
      state = returned._2
      updateProgress()
    }
    if (haltCondition(current, state)) stop()
    progress
  }

  def stop(): Unit = {
    stopped = true
  }

  def updateProgress(): Unit = {
    progress = measureProgress(current, state)
  }
}


object SimplificationProcedure {


  type ProgressUpdate = (Option[String], Option[Double])

  trait SimplificationState {
    val currentStep: Int
    val seed: Random
    val rules: List[Rule]
  }

  object Evaluation {

    def progress(derivationWithHead: DerivationWithHead, internalState: State):
    ProgressUpdate = {
      val progressDouble = internalState.currentStep.toDouble / internalState.vertices.size.toDouble
      (progressDouble match {
        case 0 => Some("Initialising")
        case 1 => Some("Complete")
        case _ => Some("Running")
      }, Some(progressDouble))
    }

    def step(derivation: DerivationWithHead, state: State):
    (DerivationWithHead, State) = {
      val vertexName = state.vertices(state.currentStep)
      val graph = derivationHeadPairToGraph(derivation)
      val vertexData = graph.vdata(vertexName)
      //println("Acting on vertex "+state.currentStep)
      vertexData match {
        case node: NodeV =>
          //println("Data " + vertexData.asInstanceOf[NodeV].value)
          if (node.value.matches(state.targetString)) {
            val newNode = node.withValue(node.value.replaceAll(state.targetString, state.replacementString))
            val nextGraph = graph.copy(vdata = graph.vdata + (vertexName -> newNode))

            val nextStepName = quanto.data.Names.mapToNameMap(derivation._1.steps).
              freshWithSuggestion(DSName(state.targetString + "->" + state.replacementString + "--0"))
            val nextDerivation = (derivation._1.addStep(
              derivation._2,
              DStep(nextStepName,
                Rule(new Graph(), new Graph(), None, RuleDesc("evaluation")),
                nextGraph)
            ), Some(nextStepName))

            (nextDerivation, state.next())
          } else {
            (derivation, state.next())
          }
        case _ => (derivation, state.next())
      }
    }

    class State(val rules: List[Rule],
                val currentStep: Int,
                val maxSteps: Option[Int],
                val seed: Random,
                val vertices: List[VName],
                val targetString: String,
                val replacementString: String,
                val vertexLimit: Option[Int]) extends SimplificationState {
      def next(): State = {
        new State(rules,
          currentStep + 1,
          maxSteps,
          seed,
          vertices,
          targetString,
          replacementString,
          vertexLimit)
      }
    }

  }

  object PullErrors {


    def errorsDistance(targets: Set[VName])(graph: Graph, vNames: Set[VName]): Option[Double] =
      GraphAnalysis.distanceOfSingleErrorFromEnd(targets)(graph, vNames)

    def progress(derivation: DerivationWithHead, state: State): ProgressUpdate = {
      val currentDistance = state.currentDistance
      if (currentDistance.nonEmpty) {
        (Some(f"Distance: ${currentDistance.get}%2.2f"), None)
      } else {
        (None, None)
      }
    }

    def step(derivation: DerivationWithHead, state: State):
    (DerivationWithHead, State) = {
      import state._
      val d = derivation
      val randRule = rules(seed.nextInt(rules.size))
      val adjacencyMatrix = GraphAnalysis.adjacencyMatrix(d)
      val errors = GraphAnalysis.detectPiNodes(d)
      val specialsDistances = errors.map(eName => (eName, state.weightFunction(d, Set(eName))))
      val maxDistance = specialsDistances.maxBy[Double](ed => ed._2.getOrElse(0))._2.getOrElse(0)

      val furthestErrors = specialsDistances.filter(ed => ed._2.getOrElse(0) == maxDistance).map(ed => ed._1)

      val farNeighbours = furthestErrors.flatMap(GraphAnalysis.neighbours(adjacencyMatrix, _)) ++ furthestErrors

      val suggestedNextStep = AutoReduce.randomSingleApply(d,
        randRule,
        seed,
        restrictToVertices = Some(farNeighbours),
        mustIncludeOneOf = Some(furthestErrors),
        blockedVertices = None)

      val changed = suggestedNextStep._1.steps.size > derivation._1.steps.size

      val shrunkNextStep = AutoReduce.greedyReduce(suggestedNextStep, greedyRules.getOrElse(Set()).toList)
      val newErrors = GraphAnalysis.detectPiNodes(shrunkNextStep)
      val suggestedNewSize: Double = state.weightFunction(shrunkNextStep, newErrors).getOrElse(0)

      // Bias towards strict reduction
      if (currentDistance.isEmpty ||
        changed && (
          suggestedNewSize < currentDistance.get ||
            ((suggestedNewSize < currentDistance.get.ceil || suggestedNewSize == currentDistance.get.floor)
              && seed.nextBoolean()) || seed.nextDouble() < 0.1)
      ) {
        println("accepting " + suggestedNewSize)
        println(randRule.description)
        (shrunkNextStep, state.next(Some(suggestedNewSize)))
      } else {
        println("rej " + suggestedNewSize)
        (d, state.next(currentDistance))
      }
    }

    case class State(rules: List[Rule],
                     currentStep: Int,
                     maxSteps: Option[Int],
                     seed: Random,
                     weightFunction: (Graph, Set[VName]) => Option[Double],
                     greedyRules: Option[List[Rule]],
                     currentDistance: Option[Double],
                     heldVertices: Option[Set[VName]],
                     vertexLimit: Option[Int]) extends SimplificationState {
      def next(distance: Option[Double]): State = {
        State(
          rules,
          currentStep + 1,
          maxSteps,
          seed,
          weightFunction,
          greedyRules,
          distance,
          heldVertices,
          vertexLimit
        )
      }
    }


  }

  object Annealing {


    def progress(derivationWithHead: DerivationWithHead, state: State):
    ProgressUpdate = {
      val progressDouble = state.currentStep.toDouble / state.maxSteps.getOrElse(1).toDouble
      (progressDouble match {
        case 0 => Some("Initialising")
        case 1 => Some("Complete")
        case _ => Some("Running")
      }, Some(progressDouble))
    }


    def step(derivationWithHead: DerivationWithHead, state: State):
    (DerivationWithHead, State) = {
      require(state.maxSteps.nonEmpty)
      import state._
      val d = derivationWithHead

      val allowIncrease = seed.nextDouble() < math.exp(-timeDilation * currentStep / maxSteps.get)
      if (rules.nonEmpty) {
        val randRule = rules(seed.nextInt(rules.length))
        val suggestedNextStep = AutoReduce.randomSingleApply(
          d,
          randRule,
          seed,
          None,
          None,
          None)
        val head = Derivation.derivationHeadPairToGraph(d)
        val smallEnough = vertexLimit.isEmpty || (head.verts.size < vertexLimit.get)
        if ((allowIncrease && smallEnough) || suggestedNextStep < head) {
          (suggestedNextStep, state.next())
        } else
          (d, state.next())
      } else
        (d, state.next())
    }

    class State(val rules: List[Rule],
                val currentStep: Int,
                val maxSteps: Option[Int],
                val seed: Random,
                val timeDilation: Double,
                val vertexLimit: Option[Int]
               ) extends SimplificationState {
      def next(): State = {
        new State(rules, currentStep + 1, maxSteps, seed, timeDilation, vertexLimit)
      }
    }

  }

  object LTEByWeight {

    def step(derivation: DerivationWithHead, state: State):
    (DerivationWithHead, State) = {
      println("In LTE function")
      println(state)
      import state._
      val d = derivation
      val randRule = rules(seed.nextInt(rules.length))
      val suggestedNextStep = AutoReduce.randomSingleApply(d, randRule, seed,
        restrictToVertices = None,
        mustIncludeOneOf = None,
        blockedVertices = heldVertices)
      val suggestedNewSize = weightFunction(suggestedNextStep)
      if (suggestedNewSize.nonEmpty && (currentDistance.isEmpty || suggestedNewSize.get <= currentDistance.get)) {
        println("accepting")
        println(randRule)
        (suggestedNextStep, state.next(suggestedNewSize.get))
      } else {
        (d, state.next(currentDistance.get))
      }
    }


    def progress(derivation: DerivationWithHead, state: State): ProgressUpdate = {
      val currentDistance = state.currentDistance
      if (currentDistance.nonEmpty) {
        (Some(f"Distance: ${currentDistance.get}%2.2f"), None)
      } else {
        (None, None)
      }
    }

    case class State(rules: List[Rule],
                     currentStep: Int,
                     maxSteps: Option[Int],
                     seed: Random,
                     weightFunction: Graph => Option[Double],
                     currentDistance: Option[Double],
                     heldVertices: Option[Set[VName]],
                     vertexLimit: Option[Int]) extends SimplificationState {

      def next(distance: Double): State = {
        State(
          rules,
          currentStep + 1,
          maxSteps,
          seed,
          weightFunction,
          Some(distance),
          heldVertices,
          vertexLimit
        )
      }


    }

  }

  object Greedy {

    def progress(derivation: DerivationWithHead, state: State): ProgressUpdate = {
      val currentRule = state.remainingRules.headOption
      if (currentRule.nonEmpty) {
        (Some(currentRule.toString), None)
      } else {
        (None, None)
      }
    }

    def step(derivation: DerivationWithHead, state: State):
    (DerivationWithHead, State) = {
      val seed = state.seed
      val nextRule = state.remainingRules.headOption
      val remainingRules = state.remainingRules
      if (nextRule.nonEmpty) {
        val suggestedDerivation = AutoReduce.randomSingleApply(derivation, nextRule.get, seed, None, None, None)
        if (suggestedDerivation < derivation) {
          (suggestedDerivation, state.next(remainingRules))
        } else {
          (derivation, state.next(remainingRules.tail))
        }
      } else {
        (derivation, state)
      }
    }

    class State(val rules: List[Rule],
                val currentStep: Int,
                val maxSteps: Option[Int],
                val seed: Random,
                val remainingRules: List[Rule],
                val vertexLimit: Option[Int]) extends SimplificationState {
      def next(updatedRemainingRules: List[Rule]): State = {
        new State(
          rules,
          currentStep + 1,
          maxSteps,
          seed,
          updatedRemainingRules,
          vertexLimit
        )
      }
    }

  }

}
