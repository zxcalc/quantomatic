package quanto.cosy

import quanto.cosy.Interpreter._
import quanto.data._
import quanto.util.json.{Json, JsonAccessException, JsonArray, JsonObject}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

/**
  * Synthesises diagrams, holds the data and generates equivalence classes
  */


trait JSONable {
  def toJson: JsonObject
}

// EQUIVALENCE CLASSES

abstract class EquivalenceClass[T] {
  val centre: Tensor
  var members: List[T]

  def addMember(newMember: T): EquivalenceClass[T] = {
    this.members = newMember :: members
    this
  }

  def toJSON: JsonObject

  override def toString: String = {
    "Equivalence class" +
      "\nSize: " + this.members.length +
      "\nCentre: " + this.centre.toString()
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: EquivalenceClass[T] =>
        this.members.toSet == that.members.toSet
      case _ => false
    }

  override def hashCode(): Int = 1 // HashCode overridden so it will compare set-like
}

class EquivalenceClassByAdjMat(val theory: Theory,
                               val centre: Tensor,
                               var members: List[String]) extends EquivalenceClass[String] {

  override def toJSON: JsonObject = {
    JsonObject(
      "centre" -> centre.toJson,
      "size" -> this.members.length,
      "members" -> JsonArray(members)
    )
  }

}

object EquivalenceClassByAdjMat {
  def fromJson(json: JsonObject, theory: Theory): EquivalenceClassByAdjMat = {

    val members: List[String] = (json / "members").asArray.map(_.stringValue).toList
    new EquivalenceClassByAdjMat(theory,
      Tensor.fromJson((json / "centre").asObject),
      members
    )
  }

  def fromEC(ec: EquivalenceClass[String], theory: Theory): EquivalenceClassByAdjMat = {
    new EquivalenceClassByAdjMat(theory, ec.centre, ec.members)
  }
}

class EquivalenceClassByAdjString(val centre: Tensor,
                                  var members: List[String]) extends EquivalenceClass[String] {

  override def toJSON: JsonObject = {
    JsonObject(
      "centre" -> centre.toJson,
      "size" -> this.members.length,
      "members" -> JsonArray(members)
    )
  }

}

object EquivalenceClassByAdjString {
  def fromJson(json: JsonObject): EquivalenceClassByAdjString = {
    val members: List[String] = (json / "members").asArray.map(x => x.toString).toList
    new EquivalenceClassByAdjString((json / "centre").asObject, members)
  }
}

class EquivalenceClassByBlockStack(val centre: Tensor,
                                   var members: List[BlockStack]) extends EquivalenceClass[BlockStack] {

  override def toJSON: JsonObject = {
    JsonObject(
      "centre" -> centre.toJson,
      "size" -> this.members.length,
      "members" -> JsonArray(
        members.map(x => JsonObject(
          "stack" -> x.toJson)
        ))
    )
  }

}


object EquivalenceClassByBlockStack {
  def fromJson(json: JsonObject): EquivalenceClassByBlockStack = {
    def toMemberFromJson(x: JsonObject) = BlockStack.fromJson((x / "stack").asObject)

    val members: List[BlockStack] = (json / "members").asArray.map(x => toMemberFromJson(x.asObject)).toList
    new EquivalenceClassByBlockStack((json / "centre").asObject, members)
  }
}


class EquivalenceClassByGraph(val theory: Theory,
                              val centre: Tensor,
                              var members: List[Graph]) extends EquivalenceClass[Graph] {

  override def toJSON: JsonObject = {
    JsonObject(
      "centre" -> centre.toJson,
      "size" -> this.members.length,
      "members" -> JsonArray(
        members.map(x => JsonObject(
          "graph" -> Graph.toJson(x, theory))
        ))
    )
  }
}

object EquivalenceClassByGraph {
  def fromJson(json: JsonObject, theory: Theory): EquivalenceClassByGraph = {
    def toMemberFromJson(x: JsonObject) = Graph.fromJson((x / "graph").asObject, theory)

    val members: List[Graph] = (json / "members").asArray.map(x => toMemberFromJson(x.asObject)).toList
    new EquivalenceClassByGraph(theory, (json / "centre").asObject, members)
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
    val withInterpretations = candidates.map(c => (c, interpret(c)))
    withInterpretations.foreach(ct => add(ct._1, ct._2))
    val endTime = System.nanoTime()
    val timeTaken = endTime - startTime
    val timeTakenString = "which took " + (timeTaken * 1e-9).toString + "s"
    messageList = timeTakenString :: message :: messageList
    this
  }

  // Finds the closest class and adds it, or creates a new class if outside tolerance
  def compareAndAddToClass(candidate: T, tensor: Tensor, normalised: Boolean = false): EquivalenceClass[T] = {

    val matchingSizedTensors = (if (normalised) equivalenceClassesNormalised else equivalenceClasses).
      filter(_.centre.isSameShapeAs(tensor))

    if (matchingSizedTensors.isEmpty) {
      newClass(candidate, tensor, normalised)
    } else {

      val (closestExisting, distance) =
        matchingSizedTensors.
          map(eq => (eq, if (normalised) eq.centre.distanceAfterScaling(tensor) else eq.centre.distance(tensor))).
          minBy(_._2)

      if (distance > tolerance) {
        newClass(candidate, tensor, normalised)
      } else {
        closestExisting.addMember(candidate)
      }
    }
  }


  // Returns (new Class, -1) if no EquivalenceClasses here
  def closestClassTo(that: Tensor, normalised: Boolean = false): Option[(EquivalenceClass[T], Double)] = {
    val classesOfRightSize = (if (normalised) equivalenceClassesNormalised else equivalenceClasses).
      filter(_.centre.isSameShapeAs(that))

    if (classesOfRightSize.nonEmpty) {
      Some(classesOfRightSize.map(eq =>
        (eq, if (normalised) eq.centre.distanceAfterScaling(that) else eq.centre.distance(that))
      ).minBy(_._2))
    } else None

  }

  implicit def interpret(that: T): Tensor

  def add(that: T): EquivalenceClass[T] = {
    val tensor = interpret(that)
    add(that, tensor)
  }

  def add(that: T, tensor: Tensor): EquivalenceClass[T] = {
    compareAndAddToClass(that, tensor)
    compareAndAddToClass(that, tensor, normalised = true)
  }

  def newClass(t: T, tensor: Tensor, normalised: Boolean): EquivalenceClass[T]
}

class EquivClassRunBlockStack(tolerance: Double = 1e-14) extends EquivClassRun[BlockStack] {

  var equivalenceClasses: List[EquivalenceClass[BlockStack]] = List()
  var equivalenceClassesNormalised: List[EquivalenceClass[BlockStack]] = List()

  override def newClass(t: BlockStack, tensor: Tensor, normalised: Boolean): EquivalenceClass[BlockStack] = {
    val eqc = new EquivalenceClassByBlockStack(tensor, List(t))
    if (normalised) {
      equivalenceClassesNormalised = eqc :: equivalenceClassesNormalised
    } else {
      equivalenceClasses = eqc :: equivalenceClasses
    }
    eqc
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
                           override val tolerance: Double = 1e-14) extends EquivClassRun[String] {

  var equivalenceClasses: List[EquivalenceClass[String]] = List()
  var equivalenceClassesNormalised: List[EquivalenceClass[String]] = List()

  implicit def hashToAdjMat(hash: String): AdjMat = AdjMat.fromHash(hash)

  implicit def adjMatToHash(adjmat: AdjMat): String = adjmat.hash

  def adjMatToGraph(adj: AdjMat): Graph = {
    Graph.fromAdjMat(adj, rdata, gdata)
  }

  override def newClass(t: String, tensor: Tensor, normalised: Boolean): EquivalenceClass[String] = {
    val eqc = new EquivalenceClassByAdjMat(theory, tensor, List(t))
    if (normalised) {
      equivalenceClassesNormalised = eqc :: equivalenceClassesNormalised
    } else {
      equivalenceClasses = eqc :: equivalenceClasses
    }
    eqc
  }

  override def interpret(that: String): Tensor = {
    interpretZXAdjMat(that, gdata, rdata)
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
        "greenNodeData" -> JsonArray(gdata.toList.map(nv => nv.toJson))
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

    new EquivClassRunAdjMat(rdata, gdata, theory, tolerance)
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
        tolerance = tolerance
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
            theory: Theory): EquivClassRunAdjMat = {
    new EquivClassRunAdjMat(rdata = redAngles,
      gdata = greenAngles,
      tolerance = tolerance,
      theory = theory)
  }
}