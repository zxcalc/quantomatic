package quanto.cosy

import quanto.cosy.Interpreter._
import quanto.data._
import quanto.util.json.{Json, JsonAccessException, JsonArray, JsonObject}

/**
  * Synthesises diagrams, holds the data and generates equivalence classes
  */


trait JSONable {
  def toJson: JsonObject
}

// EQUIVALENCE CLASSES

abstract class EquivalenceClass[T] {
  var members: List[(T, Tensor)] = List()

  def addMember(newMember: T, tensor: Tensor): EquivalenceClass[T] = {
    this.members = (newMember, tensor) :: members
    this
  }

  def toJSON: JsonObject

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

  override def equals(other: Any): Boolean =
    other match {
      case that: EquivalenceClass[T] =>
        this.members.toSet == that.members.toSet
      case _ => false
    }

  override def hashCode(): Int = 1 // HashCode overridden so it will compare set-like
}

class EquivalenceClassByAdjMat(val theory: Theory) extends EquivalenceClass[AdjMat] {


  override def toJSON: JsonObject = {
    JsonObject(
      "centre" -> centre.toJson,
      "size" -> this.members.length,
      "members" -> JsonArray(
        members.map(x => JsonObject(
          "adjMat" -> x._1.hash,
          "tensor" -> x._2.toJson)
        ))
    )
  }

}

object EquivalenceClassByAdjMat {
  def fromJson(json: JsonObject, theory: Theory): EquivalenceClassByAdjMat = {
    def toMemberFromJson(x: JsonObject) =
      (AdjMat.fromHash((x / "adjMat").stringValue), Tensor.fromJson((x / "tensor").asObject))

    val members: List[(AdjMat, Tensor)] = (json / "members").asArray.map(x => toMemberFromJson(x.asObject)).toList
    val eqc = new EquivalenceClassByAdjMat(theory)
    for (member <- members) {
      eqc.addMember(member._1, member._2)
    }
    eqc
  }

  def fromEC(ec: EquivalenceClass[AdjMat], theory: Theory): EquivalenceClassByAdjMat = {
    val eqc = new EquivalenceClassByAdjMat(theory)
    eqc.members = ec.members
    eqc
  }
}


class EquivalenceClassByBlockStack extends EquivalenceClass[BlockStack] {

  override def toJSON: JsonObject = {
    JsonObject(
      "centre" -> centre.toJson,
      "size" -> this.members.length,
      "members" -> JsonArray(
        members.map(x => JsonObject(
          "stack" -> x._1.toJson,
          "tensor" -> x._2.toJson)
        ))
    )
  }

}

object EquivalenceClassByBlockStack {
  def fromJson(json: JsonObject): EquivalenceClassByBlockStack = {
    def toMemberFromJson(x: JsonObject) =
      (BlockStack.fromJson((x / "stack").asObject), Tensor.fromJson((x / "tensor").asObject))

    val members: List[(BlockStack, Tensor)] = (json / "members").asArray.map(x => toMemberFromJson(x.asObject)).toList
    val eqc = new EquivalenceClassByBlockStack()
    for (member <- members) {
      eqc.addMember(member._1, member._2)
    }
    eqc
  }
}


class EquivalenceClassByGraph(val theory: Theory) extends EquivalenceClass[Graph] {

  override def toJSON: JsonObject = {
    JsonObject(
      "centre" -> centre.toJson,
      "size" -> this.members.length,
      "members" -> JsonArray(
        members.map(x => JsonObject(
          "graph" -> Graph.toJson(x._1, theory), "tensor" -> x._2.toJson)
        ))
    )
  }
}

object EquivalenceClassByGraph {
  def fromJson(json: JsonObject, theory: Theory): EquivalenceClassByGraph = {
    def toMemberFromJson(x: JsonObject) =
      (Graph.fromJson((x / "graph").asObject, theory), Tensor.fromJson((x / "tensor").asObject))

    val members: List[(Graph, Tensor)] = (json / "members").asArray.map(x => toMemberFromJson(x.asObject)).toList
    val eqc = new EquivalenceClassByGraph(theory)
    for (member <- members) {
      eqc.addMember(member._1, member._2)
    }
    eqc
  }
}

// RUN RESULTS


abstract class EquivClassRun[T](val tolerance: Double = 1e-14) {
  var equivalenceClasses: List[EquivalenceClass[T]]
  var equivalenceClassesNormalised: List[EquivalenceClass[T]]
  var messageList: List[String] = List()

  /** Adds the diagrams to classes, does not delete current classes first */
  def findEquivalenceClasses(candidates: Stream[T], message: String = "Stream generation method"): EquivClassRun[T] = {
    val startTime = System.nanoTime()
    candidates.foreach(x => {
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

  // Finds the closest class and adds it, or creates a new class if outside tolerance
  def compareAndAddToClass(candidate: T, tensor: Tensor = Tensor.zero(1, 1)): EquivalenceClass[T] = {
    val adjTensor = if (tensor == Tensor.zero(1, 1)) interpret(candidate) else tensor
    var closest = emptyClass()
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
    closest.addMember(candidate, adjTensor)
    if (closest.members.length == 1) equivalenceClasses = closest :: equivalenceClasses
    closest
  }

  def compareAndAddToClassNormalised(that: T, tensor: Tensor = Tensor.zero(1, 1)): EquivalenceClass[T] = {
    val adjTensor = if (tensor == Tensor.zero(1, 1)) interpret(that).normalised else tensor.normalised
    var closest = emptyClass()
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
    closest.addMember(that, adjTensor)
    if (closest.members.length == 1) equivalenceClassesNormalised = closest :: equivalenceClassesNormalised
    closest
  }

  // Returns (new Class, -1) if no EquivalenceClasses here
  def closestClassTo(that: T): (EquivalenceClass[T], Double) = {
    closestClassTo(interpret(that))
  }

  def closestClassTo(that: Tensor): (EquivalenceClass[T], Double) = {
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

  def closestClassToNormalised(that: T): (EquivalenceClass[T], Double) = {
    closestClassToNormalised(interpret(that))
  }

  def closestClassToNormalised(that: Tensor): (EquivalenceClass[T], Double) = {
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

  def interpret(that: T): Tensor

  def add(that: T): EquivalenceClass[T] = {
    val tensor = interpret(that)
    add(that, tensor)
  }

  def add(that: T, tensor: Tensor): EquivalenceClass[T] = {
    compareAndAddToClass(that, tensor)
    compareAndAddToClassNormalised(that, tensor)
  }

  def emptyClass(): EquivalenceClass[T]
}

class EquivClassRunBlockStack(tolerance: Double = 1e-14) extends EquivClassRun[BlockStack] {

  var equivalenceClasses: List[EquivalenceClass[BlockStack]] = List()
  var equivalenceClassesNormalised: List[EquivalenceClass[BlockStack]] = List()

  override def emptyClass(): EquivalenceClass[BlockStack] = {
    new EquivalenceClassByBlockStack()
  }

  override def interpret(that: BlockStack): Tensor = {
    that.tensor
  }

  def toJSON: JsonObject = {
    val totalDiagrams = equivalenceClasses.foldLeft(0) { (a, b) => a + b.members.length }
    val numberOfClasses = equivalenceClasses.length
    JsonObject(
      "runData" -> JsonObject(
        "messages" -> JsonArray(messageList.reverse),
        "tolerance" -> tolerance,
        "totalDiagrams" -> totalDiagrams,
        "numberOfClasses" -> numberOfClasses
      ),
      "equivalenceClasses" -> JsonArray(equivalenceClasses.map(e => e.toJSON)),
      "equivalenceClassesNormalised" -> JsonArray(equivalenceClassesNormalised.map(e => e.toJSON)
      )
    )
  }
}

object EquivClassRunBlockStack {


  def fromJSON(file: java.io.File): EquivClassRunBlockStack = {
    fromJSON(Json.parse(
      new Json.Input(file)).asObject)
  }

  def fromJSON(json: JsonObject): EquivClassRunBlockStack = {
    try {
      val runData = (json / "runData").asObject
      val messages: List[String] = (runData / "messages").asArray.map(_.stringValue).toList.reverse
      val tolerance: Double = (runData / "tolerance").doubleValue

      val equivalenceClasses: List[EquivalenceClassByBlockStack] = (json / "equivalenceClasses").asArray.map(
        eqc => EquivalenceClassByBlockStack.fromJson(eqc.asObject)
      ).toList

      val equivalenceClassesNormalised: List[EquivalenceClassByBlockStack] = (json / "equivalenceClassesNormalised").asArray.map(
        eqc => EquivalenceClassByBlockStack.fromJson(eqc.asObject)
      ).toList

      val eqrr = new EquivClassRunBlockStack(tolerance)
      eqrr.messageList = messages
      eqrr.equivalenceClasses = equivalenceClasses
      eqrr.equivalenceClassesNormalised = equivalenceClassesNormalised
      eqrr
    } catch {
      case e: JsonAccessException => throw new TheoryLoadException("Error reading JSON", e)
    }
  }

  def apply(tolerance: Double): EquivClassRunBlockStack = {
    new EquivClassRunBlockStack(
      tolerance = tolerance)
  }

  def fromTensorList(tensorList: JsonObject,
                     tolerance: Double): EquivClassRunBlockStack = {
    val results = (tensorList / "results").asArray.map(js =>
      (BlockStack.fromJson((js / "stack").asObject),
        Tensor.fromJson((js / "tensor").asObject))
    )
    val ecrr = new EquivClassRunBlockStack(tolerance)
    for ((adj, ten) <- results) {
      ecrr.add(adj)
    }
    ecrr
  }
}


class EquivClassRunAdjMat(
                           rdata: Vector[NodeV],
                           gdata: Vector[NodeV],
                           val theory: Theory,
                           override val tolerance: Double = 1e-14,
                           val rulesList: List[Rule]) extends EquivClassRun[AdjMat] {

  var equivalenceClasses: List[EquivalenceClass[AdjMat]] = List()
  var equivalenceClassesNormalised: List[EquivalenceClass[AdjMat]] = List()

  def adjMatToGraph(adj: AdjMat): Graph = {
    Graph.fromAdjMat(adj, rdata, gdata)
  }

  override def emptyClass(): EquivalenceClass[AdjMat] = {
    new EquivalenceClassByAdjMat(theory)
  }

  override def interpret(that: AdjMat): Tensor = {
    interpretAdjMat(that, gdata, rdata)
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
      "equivalenceClasses" -> JsonArray(equivalenceClasses.map(e => e.toJSON)),
      "equivalenceClassesNormalised" -> JsonArray(equivalenceClassesNormalised.map(e => e.toJSON)
      )
    )
  }
}

object EquivClassRunAdjMat {
  val defaultTolerance = 1e-14

  def apply(numAngles: Int,
            tolerance: Double,
            theory: Theory,
            rulesList: List[Rule]): EquivClassRunAdjMat = {
    def angleMap = (x: Int) => x * math.Pi * 2.0 / numAngles

    val gdata = (for (i <- 0 until numAngles) yield {
      NodeV(data = JsonObject("type" -> "Z", "value" -> angleMap(i).toString), theory = theory)
    }).toVector
    val rdata = (for (i <- 0 until numAngles) yield {
      NodeV(data = JsonObject("type" -> "X", "value" -> angleMap(i).toString), theory = theory)
    }).toVector

    new EquivClassRunAdjMat(rdata, gdata, theory, tolerance, rulesList)
  }

  def fromJSON(file: java.io.File): EquivClassRunAdjMat = {
    fromJSON(Json.parse(
      new Json.Input(file)).asObject)
  }

  def fromJSON(json: JsonObject): EquivClassRunAdjMat = {
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

      val equivalenceClasses: List[EquivalenceClassByAdjMat] = (json / "equivalenceClasses").asArray.map(
        eqc => EquivalenceClassByAdjMat.fromJson(eqc.asObject, theory)
      ).toList

      val equivalenceClassesNormalised: List[EquivalenceClassByAdjMat] = (json / "equivalenceClassesNormalised").asArray.map(
        eqc => EquivalenceClassByAdjMat.fromJson(eqc.asObject, theory)
      ).toList

      val eqrr = EquivClassRunAdjMat(
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
            rulesList: List[Rule]): EquivClassRunAdjMat = {
    new EquivClassRunAdjMat(rdata = redAngles,
      gdata = greenAngles,
      tolerance = tolerance,
      rulesList = rulesList,
      theory = theory)
  }

  def fromTensorList(tensorList: JsonObject,
                     numAngles: Int,
                     theory: Theory,
                     tolerance: Double = defaultTolerance,
                     rulesList: List[Rule]): EquivClassRunAdjMat = {
    val results = (tensorList / "results").asArray.map(js =>
      (AdjMat.fromHash((js / "adjMatHash").stringValue),
        Tensor.fromJson((js / "tensor").asObject))
    )

    def angleMap = (x: Int) => x * math.Pi * 2.0 / numAngles

    val gdata = (for (i <- 0 until numAngles) yield {
      NodeV(data = JsonObject("type" -> "Z", "value" -> angleMap(i).toString), theory = theory)
    }).toVector
    val rdata = (for (i <- 0 until numAngles) yield {
      NodeV(data = JsonObject("type" -> "X", "value" -> angleMap(i).toString), theory = theory)
    }).toVector

    val ecrr = new EquivClassRunAdjMat(rdata, gdata, theory, tolerance, rulesList)
    for ((adj, ten) <- results) {
      ecrr.add(adj)
    }
    ecrr
  }
}