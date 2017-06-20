package quanto.cosy

import quanto.cosy.Interpreter._
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

class EquivClassRunResults(
                           rdata: Vector[NodeV],
                           gdata: Vector[NodeV],
                           val theory: Theory,
                           val tolerance: Double = 1e-14,
                           val rulesList: List[Rule]) {
  var equivalenceClasses: List[EquivalenceClass] = List()
  var equivalenceClassesNormalised: List[EquivalenceClass] = List()
  var messageList: List[String] = List()

  /** Adds the diagrams to classes, does not delete current classes first */
  def findEquivalenceClasses(adjMats: Stream[AdjMat], message: String = "Stream generation method"): EquivClassRunResults = {
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
        val tensor = interpret(x)
        compareAndAddToClass(x, tensor)
        compareAndAddToClassNormalised(x, tensor)
      }
    )
    val endTime = System.nanoTime()
    val timeTaken = endTime - startTime
    val timeTakenString = "which took " + (timeTaken * 1e-9).toString + "s"
    messageList = timeTakenString :: message :: messageList
    this
  }

  // Returns (new Class, -1) if no EquivalenceClasses here
  def closestClassTo(that: AdjMat): (EquivalenceClass, Double) = {
    closestClassTo(interpret(that))
  }

  def closestClassToNormalised(that: AdjMat): (EquivalenceClass, Double) = {
    closestClassToNormalised(interpret(that))
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

  def closestClassToNormalised(that: Tensor): (EquivalenceClass, Double) = {
    var closestClass = equivalenceClasses.head
    var closestDistance: Double = -1.0
    for (eqc <- equivalenceClasses) {
      val rep = eqc.centre.normalised
      val dn = that.normalised
      val d = math.min(rep.distance(dn), rep.distance(dn.scaled(factor = -1.0)))
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
    val tensor = interpret(adjMat)
    compareAndAddToClass(adjMat, tensor)
    compareAndAddToClassNormalised(adjMat, tensor)
  }

  // Finds the closest class and adds it, or creates a new class if outside tolerance
  def compareAndAddToClass(adj: AdjMat, tensor : Tensor = Tensor.zero(1,1)): EquivalenceClass = {
    val adjTensor = if (tensor == Tensor.zero(1,1)) interpret(adj) else tensor
    var closest = new EquivalenceClass()
    var closestDist = tolerance
    for (eqClass <- equivalenceClasses) {

      // Need to ensure that the tensor is of the right size first!
      if (adjTensor.isSameShapeAs(eqClass.centre)) {
        val dist = eqClass.centre.distance(adjTensor)
        if (dist < tolerance && dist > -1) {
          closest = eqClass
          closestDist = dist
        }
      }
    }
    closest.addMember(adjMatToGraph(adj), adjTensor)
    if (closest.members.length == 1) equivalenceClasses = closest :: equivalenceClasses
    closest
  }

  def compareAndAddToClassNormalised(adj: AdjMat, tensor : Tensor = Tensor.zero(1,1)): EquivalenceClass = {
    val adjTensor = if (tensor == Tensor.zero(1,1)) interpret(adj).normalised else tensor.normalised
    var closest = new EquivalenceClass()
    var closestDist = tolerance
    for (eqClass <- equivalenceClassesNormalised) {

      // Need to ensure that the tensor is of the right size first!
      if (adjTensor.isSameShapeAs(eqClass.centre)) {
        val rep = eqClass.centre // already normalised
        val dist = math.min(rep.distance(adjTensor), rep.distance(adjTensor.scaled(factor = -1.0)))
        if (dist < tolerance && dist > -1) {
          closest = eqClass
          closestDist = dist
        }
      }
    }
    closest.addMember(adjMatToGraph(adj), adjTensor)
    if (closest.members.length == 1) equivalenceClassesNormalised = closest :: equivalenceClassesNormalised
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
        "tolerance" -> tolerance,
        "theory" -> Theory.toJson(theory),
        "totalDiagrams" -> totalDiagrams,
        "numberOfClasses" -> numberOfClasses,
        "redNodeData" -> JsonArray(rdata.toList.map(nv => nv.toJson)),
        "greenNodeData" -> JsonArray(gdata.toList.map(nv => nv.toJson)),
        "rules" -> JsonArray(rulesList.map(r => Rule.toJson(r, theory)))
      ),
      "equivalenceClasses" -> JsonArray(equivalenceClasses.map(e => e.toJSON(
        theory).asObject)),
      "equivalenceClassesNormalised" -> JsonArray(equivalenceClassesNormalised.map(e => e.toJSON(
        theory).asObject)
      )
    )
  }
}

object EquivClassRunResults {
  val defaultTolerance = 1e-14

  def apply(            numAngles: Int,
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

    new EquivClassRunResults(rdata, gdata, theory, tolerance, rulesList)
  }

  def loadFromFile(file: java.io.File): EquivClassRunResults = {
    fromJSON(Json.parse(
      new Json.Input(file)).asObject)
  }

  def fromJSON(json: JsonObject): EquivClassRunResults = {
    try {
      val runData = (json / "runData").asObject
      val messages: List[String] = (runData / "messages").asArray.map(_.stringValue).toList.reverse
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

      val equivalenceClassesNormalised: List[EquivalenceClass] = (json / "equivalenceClassesNormalised").asArray.map(
        eqc => EquivalenceClass.fromJson(eqc.asObject, theory)
      ).toList

      val eqrr = EquivClassRunResults(
        redAngles = redData,
        greenAngles = greenData,
        theory = theory,
        tolerance = tolerance,
        rulesList = rules
      )
      eqrr.messageList = messages
      eqrr.equivalenceClasses = equivalenceClasses
      eqrr.equivalenceClassesNormalised = equivalenceClassesNormalised
      eqrr
    } catch {
      case e: JsonAccessException => throw new TheoryLoadException("Error reading JSON", e)
    }
  }

  def apply(redAngles: Vector[NodeV],
            greenAngles: Vector[NodeV],
            tolerance: Double,
            theory: Theory,
            rulesList: List[Rule]): EquivClassRunResults = {
    new EquivClassRunResults(rdata = redAngles,
      gdata = greenAngles,
      tolerance = tolerance,
      rulesList = rulesList,
      theory = theory)
  }
}