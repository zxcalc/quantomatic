package quanto.util

import java.io.File

import akka.util.Timeout
import org.python.core.{PyList, PyArray}
import quanto.core._
import quanto.data._
import quanto.data.Names._
import quanto.gui.QuantoDerive
import quanto.rewrite.Simproc
import quanto.util.json._
import akka.pattern.ask
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import ExecutionContext.Implicits.global


// object providing functions specifically for python scripting

object Scripting {
  implicit val timeout = Timeout(1.day)

  class NoProject extends Exception
  class RewriteError extends Exception

  def project: Project = QuantoDerive.CurrentProject match {
    case Some(p) => p
    case None => throw new NoProject
  }
  def theory: Theory = project.theory

  def load_graph(s: String) = {
    val path = project.rootFolder + "/" + s + ".qgraph"
    Graph.fromJson(Json.parse(new File(path)), theory)
  }

  def save_graph(g: Graph, s: String) = {
    val path = project.rootFolder + "/" + s + ".qgraph"
    val json = Graph.toJson(g, theory)
    json.writeTo(new File(path))
  }

  def load_rule(s: String) = {
    val path = project.rootFolder + "/" + s + ".qrule"
    (s, Rule.fromJson(Json.parse(new File(path)), theory))
  }

  def plug(g1: Graph, g2: Graph, b1: String, b2: String) =
    g1.plugGraph(g2, VName(b1), VName(b2))

  // TODO: implement with scala matcher
  def find_rewrites(g: Graph, r: (String,Rule)) = {
    null
//    val resp = QuantoDerive.core ? Call(theory.coreName, "rewrite", "find_rewrites",
//      JsonObject(
//        "rule" -> Rule.toJson(r._2, theory),
//        "graph" -> Graph.toJson(g, theory),
//        "vertices" -> JsonArray(g.verts.map(v => JsonString(v.toString)))
//      ))
//    Await.result(resp, 100.seconds) match {
//      case Success(stack) => (r._1, stack.stringValue)
//      case _ => throw new RewriteError
//    }
  }

  // TODO: implement with scala matcher
  def pull_rewrite(stack: (String, String)) = {
    null
//    val resp = QuantoDerive.core ? Call(theory.coreName, "rewrite", "pull_rewrite",
//      JsonObject("stack" -> JsonString(stack._2)))
//    Await.result(resp, 100.seconds) match {
//      case Success(obj : JsonObject) =>
//        // layout the graph before acquiring the lock, so many can be done in parallel
//        val step = DStep(
//          name = DSName("s"),
//          ruleName = stack._1,
//          rule = Rule.fromJson(obj / "rule", theory),
//          variant = RuleNormal,
//          graph = Graph.fromJson(obj / "graph", theory)).layout
//        step
//      case Success(JsonNull) => null
//      case _ => null
//    }
  }

  def delete_rewrite_stack(stack: (String,String)) = null
//    QuantoDerive.core ! Call(theory.coreName, "rewrite", "delete_rewrite_stack",
//      JsonObject("stack" -> JsonString(stack._2)))

  class derivation(start : Graph) {
    var d = Derivation(theory, start)

    def rewrite(r: (String, Rule)) = {
      false
//      val g = d.heads.headOption match { case Some(h) => d.steps(h).graph; case None => d.root }
//      val stack = find_rewrites(g, r)
//      val step = pull_rewrite(stack)
//      delete_rewrite_stack(stack)
//      if (step != null) {
//        val stepFr = step.copy(name = d.steps.freshWithSuggestion(DSName(r._1.replaceFirst("^.*\\/", "") + "-0")))
//        d = d.addStep(d.heads.headOption, stepFr)
//        true
//      } else {
//        false
//      }
    }

    def start_graph = d.root
    def current_graph = d.firstHead.map(d.steps(_).graph).getOrElse(d.root)

    def normalise(rules: PyList) {
      val n = rules.__len__()
      var i = 0
      while (i < n) {
        val r = rules.get(i).asInstanceOf[(String,Rule)]
        if (rewrite(r)) i = 0
        else i += 1
      }
    }

    def save(s: String) {
      val path = project.rootFolder + "/" + s + ".qderive"
      val json = Derivation.toJson(d, theory)
      json.writeTo(new File(path))
    }

    def copy() : derivation = { val d1 = new derivation(start); d1.d = d; d1 }
  }

  val EMPTY: Simproc = Simproc.empty

  def register_simproc(s: String, sp: Simproc) { project.simprocs += s -> sp }
}
