package quanto.util

import java.io.File

import akka.util.Timeout
import org.python.core._
import quanto.data._
import quanto.gui.QuantoDerive
import quanto.rewrite.Simproc
import quanto.util.json._

import scala.collection.JavaConverters._
import scala.concurrent.duration._


// object providing functions specifically for python scripting

object Scripting {
  implicit val timeout = Timeout(1.day)

  class NoProject extends Exception
  class RewriteError extends Exception

  def listToPyList[A](list: List[A]): PyList = new PyList(list.asJava)
  def pyListToList[A](pyList: PyList): List[A] = pyList.asInstanceOf[java.lang.Iterable[A]].asScala.toList

  def each[A](i: Iterable[A]) = i.asJava
  def verts(g: Graph) = each(g.verts)

  def project: Project = QuantoDerive.CurrentProject match {
    case Some(p) => p
    case None => throw new NoProject
  }
  def theory: Theory = project.theory

  def load_graph(s: String): Graph = {
    val path = project.rootFolder + "/" + s + ".qgraph"
    Graph.fromJson(Json.parse(new File(path)), theory)
  }

  def save_graph(g: Graph, s: String) {
    val path = project.rootFolder + "/" + s + ".qgraph"
    val json = Graph.toJson(g, theory)
    json.writeTo(new File(path))
  }

  def load_rule(s: String): Rule = {
    val path = project.rootFolder + "/" + s + ".qrule"
    Rule.fromJson(
      Json.parse(new File(path)),
      theory,
      Some(RuleDesc(s, inverse = false)))
  }

  def load_rules(ss: PyList): PyList = {
    listToPyList(pyListToList[String](ss).map(load_rule))
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

  def vertex_angle_is(g: Graph, v: VName, a: String) = g.vdata(v) match {
    case nv: NodeV => nv.angle == AngleExpression.parse(a)
    case _ => false
  }

//  def test(f: PyFunction) = {
//    val g = Graph.fromJson("{\"node_vertices\": [\"n0\"]}")
//    val i = Py.py2int(f.__call__(Py.java2py(g)))
//    2 * i
//  }


  // python wrappers for simproc combinators
  val EMPTY = Simproc.EMPTY
  def REWRITE(o: Object) = o match {
    case list: PyList => Simproc.REWRITE(pyListToList(list))
    case _ => Simproc.REWRITE(List(o.asInstanceOf[Rule]))
  }

  def REWRITE_METRIC(o: Object, metric: PyFunction) = {
    val rules = o match {
      case list: PyList => pyListToList(list)
      case _ => List(o.asInstanceOf[Rule])
    }

    Simproc.REWRITE_METRIC(rules, {g => Py.py2int(metric.__call__(Py.java2py(g)))})
  }

  def REWRITE_WEAK_METRIC(o: Object, metric: PyFunction) = {
    val rules = o match {
      case list: PyList => pyListToList(list)
      case _ => List(o.asInstanceOf[Rule])
    }

    Simproc.REWRITE_WEAK_METRIC(rules, {g => Py.py2int(metric.__call__(Py.java2py(g)))})
  }

  def REWRITE_TARGETED(rule: Rule, v: String, targ: PyFunction) = {
    Simproc.REWRITE_TARGETED(rule, VName(v),
      { g =>
        val v = targ.__call__(Py.java2py(g))
        if (v.isInstanceOf[PyNone]) None
        else Some(VName(v.toString))
      })
  }

  def REPEAT(s: Simproc) = Simproc.REPEAT(s)

  // REDUCE_XXX(-) := REPEAT(REWRITE_XXX(-))
  def REDUCE(o: Object) = REPEAT(REWRITE(o))
  def REDUCE_TARGETED(rule: Rule, v: String, targ: PyFunction) = REPEAT(REWRITE_TARGETED(rule, v, targ))
  def REDUCE_METRIC(o: Object, metric: PyFunction) = REPEAT(REWRITE_METRIC(o, metric))
  def REDUCE_WEAK_METRIC(o: Object, metric: PyFunction) = REPEAT(REWRITE_WEAK_METRIC(o, metric))

  def register_simproc(simprocName: String, simproc: Simproc, source: String): Unit = {
    project.simprocSource += simprocName -> source
    project.simprocs += simprocName -> simproc
  }

  // Ideally use the overloaded method with a source,
  // Or use the injected "register" python function
  def register_simproc(s: String, sp: Simproc): Unit = {
    register_simproc(s, sp, "")
  }

  // THERE ARE ALSO FUNCTIONS AND VARIABLES INJECTED DIRECTLY INTO PYTHON CODE
  // PLEASE SEE quanto/gui/PythonEditPanel.scala
}
