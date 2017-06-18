package quanto.cosy

import quanto.cosy.Interpreter._
import quanto.data
import quanto.data._
import quanto.util.json.{Json, JsonAccessException, JsonArray, JsonObject}
import quanto.rewrite._

/**
  * Synthesises diagrams, holds the data and generates equivalence classes
  */

class EquivalenceClass() {
  var members: List[(Graph, Tensor)] = List()

  def addMember(graph: Graph, tensor: Tensor): EquivalenceClass = {
    members = (graph, tensor) :: members
    this
  }

  override def toString: String = {
    "Equivalence class" +
      "\nSize: " + this.members.length +
      "\nCentre: " + this.centre.toString()
  }

  def centre: Tensor = if (members.nonEmpty) {
    members.head._2
  } else {
    Tensor.zero(1, 1)
  }

  def toJSON(theory: Theory): JsonObject = {
    JsonObject(
      "centre" -> centre.toJson,
      "size" -> this.members.length,
      "members" -> JsonArray(
        members.map(x => JsonObject(
          "graph" -> Graph.toJson(x._1, theory), "tensor" -> x._2.toJson)
        ))
    )
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: EquivalenceClass =>
        this.members.toSet == that.members.toSet
      case _ => false
    }

  override def hashCode(): Int = 1 // HashCode overridden so it will compare set-like
}

object EquivalenceClass {
  def fromJson(json: JsonObject, theory: Theory): EquivalenceClass = {
    def toMemberFromJson(x: JsonObject) =
      (Graph.fromJson((x / "graph").asObject, theory), Tensor.fromJson((x / "tensor").asObject))

    val members: List[(Graph, Tensor)] = (json / "members").asArray.map(x => toMemberFromJson(x.asObject)).toList
    val eqc = new EquivalenceClass()
    for (member <- members) {
      eqc.addMember(member._1, member._2)
    }
    eqc
  }
}

class EquivClassRunResults(val normalised: Boolean,
                           rdata: Vector[NodeV],
                           gdata: Vector[NodeV],
                           val theory: Theory,
                           val tolerance: Double = 1e-14,
                           val rulesList: List[Rule]) {
  var equivalenceClasses: List[EquivalenceClass] = List()
  var messageList: List[String] = List()

  /** Adds the diagrams to classes, does not delete current classes first */
  def findEquivalenceClasses(adjMats: Stream[AdjMat], message: String = "Stream generation method"): List[EquivalenceClass] = {
    val startTime = System.nanoTime()
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
    val endTime = System.nanoTime()
    val timeTaken = endTime - startTime
    val timeTakenString = "which took " + (timeTaken * 1e-9).toString + "s"
    messageList = timeTakenString :: message :: messageList
    equivalenceClasses
  }

  // Returns (new Class, -1) if no EquivalenceClasses here
  def closestClassTo(that: AdjMat): (EquivalenceClass, Double) = {
    closestClassTo(interpret(that))
  }

  def closestClassTo(that: Tensor): (EquivalenceClass, Double) = {
    var closestClass = equivalenceClasses.head
    var closestDistance: Double = -1.0
    for (eqc <- equivalenceClasses) {
      val d = eqc.centre.distance(that)
      if (closestDistance < 0 || (d < closestDistance && d >= 0)) {
        closestDistance = d
        closestClass = eqc
      }
    }
    (closestClass, closestDistance)
  }

  def interpret(adjMat: AdjMat): Tensor = {
    interpretAdjMat(adjMat, rdata, gdata)
  }

  def add(adjMat: AdjMat): EquivalenceClass = {
    compareAndAddToClass(adjMat)
  }

  // Finds the closest class and adds it, or creates a new class if outside tolerance
  def compareAndAddToClass(adj: AdjMat): EquivalenceClass = {
    val adjTensor = interpret(adj)
    var closest = new EquivalenceClass()
    var closestDist = tolerance
    for (eqClass <- equivalenceClasses) {

      // Need to ensure that the tensor is of the right size first!
      if (adjTensor.isSameShapeAs(eqClass.centre)) {
        var dist = tolerance
        if (normalised) {
          val rep = eqClass.centre.normalised
          val dn = adjTensor.normalised
          dist = math.min(rep.distance(dn), rep.distance(dn.scaled(factor = -1.0)))
        } else {
          dist = eqClass.centre.distance(adjTensor)
        }
        if (dist < tolerance) {
          closest = eqClass
          closestDist = dist
        }
      }
    }
    closest.addMember(adjMatToGraph(adj), adjTensor)
    if (closest.members.length == 1) equivalenceClasses = closest :: equivalenceClasses
    closest
  }

  def adjMatToGraph(adj: AdjMat): Graph = {
    Graph.fromAdjMat(adj, rdata, gdata)
  }

  def toJSON: JsonObject = {
    val totalDiagrams = equivalenceClasses.foldLeft(0) { (a, b) => a + b.members.length }
    val numberOfClasses = equivalenceClasses.length
    JsonObject(
      "runData" -> JsonObject(
        "messages" -> JsonArray(messageList.reverse),
        "normalised" -> normalised,
        "tolerance" -> tolerance,
        "theory" -> Theory.toJson(theory),
        "totalDiagrams" -> totalDiagrams,
        "numberOfClasses" -> numberOfClasses,
        "redNodeData" -> JsonArray(rdata.toList.map(nv => nv.toJson)),
        "greenNodeData" -> JsonArray(gdata.toList.map(nv => nv.toJson)),
        "rules" -> JsonArray(rulesList.map(r => Rule.toJson(r, theory)))
      ),
      "equivalenceClasses" -> JsonArray(equivalenceClasses.map(e => e.toJSON(
        theory).asObject
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

  def loadFromFile(file: java.io.File): EquivClassRunResults = {
    fromJSON(Json.parse(
      new Json.Input(file)).asObject)
  }

  def fromJSON(json: JsonObject): EquivClassRunResults = {
    try {
      val runData = (json / "runData").asObject
      val messages: List[String] = (runData / "messages").asArray.map(_.stringValue).toList.reverse
      val normalised: Boolean = (runData / "normalised").boolValue
      val tolerance: Double = (runData / "tolerance").doubleValue
      val theory: Theory = Theory.fromJson(runData / "theory")
      val redData: Vector[NodeV] = (runData / "redNodeData").asArray.map(
        NodeV.fromJson(_, theory)
      ).toVector
      val greenData: Vector[NodeV] = (runData / "greenNodeData").asArray.map(
        NodeV.fromJson(_, theory)
      ).toVector
      val rules = (runData / "rules").asArray.map(Rule.fromJson(_, theory)).toList

      val equivalenceClasses: List[EquivalenceClass] = (json / "equivalenceClasses").asArray.map(
        eqc => EquivalenceClass.fromJson(eqc.asObject, theory)
      ).toList

      val eqrr = EquivClassRunResults(normalised = normalised,
        redAngles = redData,
        greenAngles = greenData,
        theory = theory,
        tolerance = tolerance,
        rulesList = rules
      )
      eqrr.messageList = messages
      eqrr.equivalenceClasses = equivalenceClasses
      eqrr
    } catch {
      case e: JsonAccessException => throw new TheoryLoadException("Error reading JSON", e)
    }
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