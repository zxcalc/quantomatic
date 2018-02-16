package quanto.cosy

import java.nio.file.Path
import java.util.Calendar
import java.util.concurrent.TimeUnit

import quanto.util.FileHelper._
import quanto.data.{Graph, Rule}
import quanto.rewrite.{Matcher, Rewriter}

import scala.concurrent.duration.Duration

/**
  * This class performs the actual batch conjecture synthesis
  */
class CoSyRun(
               graphGenerator: Iterator[Graph],
               graphInterpreter: Graph => Tensor,
               graphLeftBiggerRight: (Graph, Graph) => Boolean,
               startingRules: List[Rule],
               duration: Duration,
               outputDir: String,
               notes: String,
             ) {

  var rules: List[Rule] = List()

  def begin(): Unit = {
    def now(): Long = Calendar.getInstance().getTimeInMillis

    val timeStart = now()
    var equivClasses: Map[Tensor, Graph] = Map()
    while (Duration(now() - timeStart, "millis") > duration) {
      // Get a graph
      val graph : Graph = graphGenerator.next()
      var matchesReductionRule : Boolean = true
      // TODO: Check if it matches rules?
      for(rule <- rules){
        for(m <- Matcher.findMatches(rule.lhs, graph)){
          if (graphLeftBiggerRight(Rewriter.rewrite(m, rule.rhs), graph)){
            matchesReductionRule = true
          }
        }
      }

      if(!matchesReductionRule) {
        val interpretation = graphInterpreter(graph)
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

  def loadRule(rule: Rule): Unit = {
    // Please don't put bbox rules into here unless you really mean them to be here and they reduce left->right
    if(rule.lhs.bboxes.nonEmpty){
      rules = rule :: rules
    } else {
      // No bboxes, act normally
      if (graphLeftBiggerRight(rule.lhs, rule.rhs)) {
        rules = rule :: rules
      } else if (graphLeftBiggerRight(rule.rhs, rule.lhs)) {
        rules = rule.inverse :: rules
      } else {
        // Not a reduction rule, so leave it out
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

  startingRules.foreach(loadRule)
}