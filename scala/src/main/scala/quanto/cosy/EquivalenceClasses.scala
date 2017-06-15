package quanto.cosy

import quanto.cosy.Interpreter._
import quanto.data._
import quanto.util.json.{JsonArray, JsonObject}
import quanto.rewrite._

import scala.util.parsing.json.JSONObject

/**
  * Synthesises diagrams, holds the data and generates equivalence classes
  */

class EquivalenceClass(val centre: (AdjMat, Tensor)) {
  var members: List[(AdjMat, Tensor)] = List(centre)

  def addMember(adj: AdjMat, tensor: Tensor): EquivalenceClass = {
    members = (adj, tensor) :: members
    this
  }

  override def toString: String = {
    var s = "Equivalence class" +
      "\nSize: " + this.members.length +
      "\nCentre: " + this.centre._2.toString()
    //this.members.toString()
    s
  }

  def toJSON(adjMatToJSON : AdjMat => JsonObject): JsonObject = {
    JsonObject(
      "centre" -> centre._2.toString,
      "size" -> this.members.length,
      "members" -> JsonArray(
        members.map(x => JsonObject(
          "adjMat" -> adjMatToJSON(x._1), "tensor" -> x._2.toString)
        ))
    )
  }
}

class EquivClassRunResults(val normalised: Boolean,
                           rdata: Vector[NodeV],
                           gdata: Vector[NodeV],
                           val theory: Theory,
                           val tolerance: Double = 1e-14,
                           val rulesList: List[Rule]) {
  var equivalenceClasses: List[EquivalenceClass] = List()

  /** Adds the diagrams to classes, does not delete current classes first */
  def findEquivalenceClasses(adjMats: Stream[AdjMat]): List[EquivalenceClass] = {
    adjMats.foreach(
      x => if (
        rulesList.forall(r =>
          if (r.lhs > r.rhs) {
            Matcher.findMatches(r.lhs, adjMatToGraph(x)).isEmpty
          } else if (r.rhs > r.lhs) {
            Matcher.findMatches(r.rhs, adjMatToGraph(x)).isEmpty
          } else {
            true
          }
        )) {
        compareAndAddToClass(x)
      }
    )
    equivalenceClasses
  }

  def adjMatToGraph(adj: AdjMat): Graph = {
    Graph.fromAdjMat(adj, rdata, gdata)
  }

  // Returns (new Class, -1) if no EquivalenceClasses here
  def closestClassTo(that: AdjMat): (EquivalenceClass, Double) = {
    closestClassTo(interpret(that))
  }

  def closestClassTo(that: Tensor): (EquivalenceClass, Double) = {
    var closestClass = equivalenceClasses.head
    var closestDistance: Double = -1.0
    for (eqc <- equivalenceClasses) {
      val d = eqc.centre._2.distance(that)
      if (closestDistance < 0 || d < closestDistance) {
        closestDistance = d
        closestClass = eqc
      }
    }
    (closestClass, closestDistance)
  }

  def add(adjMat: AdjMat): EquivalenceClass = {
    compareAndAddToClass(adjMat)
  }

  // Finds the closest class and adds it, or creates a new class if outside tolerance
  def compareAndAddToClass(adj: AdjMat): EquivalenceClass = {
    val adjTensor = interpret(adj)
    var closest = new EquivalenceClass((adj, interpret(adj)))
    var closestDist = tolerance
    for (eqClass <- equivalenceClasses) {

      // Need to ensure that the tensor is of the right size first!
      if (adjTensor.isSameShapeAs(eqClass.centre._2)) {
        var dist = tolerance
        if (normalised) {
          val rep = eqClass.centre._2.normalised
          val dn = adjTensor.normalised
          dist = math.min(rep.distance(dn), rep.distance(dn.scaled(factor = -1.0)))
        } else {
          dist = eqClass.centre._2.distance(adjTensor)
        }
        if (dist < tolerance) {
          closest = eqClass
          closestDist = dist
        }
      }
    }

    if (closest.centre == (adj, adjTensor)) equivalenceClasses =
      closest :: equivalenceClasses else closest.addMember(adj, adjTensor)
    closest
  }

  def interpret(adjMat: AdjMat): Tensor = {
    interpretAdjMat(adjMat, rdata, gdata)
  }

  def toJSON: JsonObject = {
    JsonObject(
      "runData" -> JsonObject(
        "normalised" -> normalised,
        "redNodeData" -> JsonArray(rdata.toList.map(nv => nv.toJson)),
        "greenNodeData" -> JsonArray(gdata.toList.map(nv => nv.toJson)),
        "tolerance" -> tolerance,
        "theory" -> theory.toString
      ),
      "equivalenceClasses" -> JsonArray(equivalenceClasses.map(e => e.toJSON(
        a => Graph.toJson(adjMatToGraph(a),theory).asObject
      )
      )
      )
    )
  }
}

object EquivClassRunResults {
  val defaultTolerance = 1e-14

  def apply(normalised: Boolean,
            numAngles: Int,
            tolerance: Double,
            theory: Theory,
            rulesList: List[Rule]): EquivClassRunResults = {
    def angleMap = (x: Int) => x * math.Pi * 2.0 / numAngles

    val gdata = (for (i <- 0 until numAngles) yield {
      NodeV(data = JsonObject("type" -> "Z", "value" -> angleMap(i).toString), theory = theory)
    }).toVector
    val rdata = (for (i <- 0 until numAngles) yield {
      NodeV(data = JsonObject("type" -> "X", "value" -> angleMap(i).toString), theory = theory)
    }).toVector

    new EquivClassRunResults(normalised, rdata, gdata, theory, tolerance, rulesList)
  }

  def apply(normalised: Boolean,
            redAngles: Vector[NodeV],
            greenAngles: Vector[NodeV],
            tolerance: Double,
            theory: Theory,
            rulesList: List[Rule]): EquivClassRunResults = {
    new EquivClassRunResults(normalised = normalised,
      rdata = redAngles,
      gdata = greenAngles,
      tolerance = tolerance,
      rulesList = rulesList,
      theory = theory)
  }
}