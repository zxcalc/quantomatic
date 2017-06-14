package quanto.cosy

import quanto.cosy.Interpreter._
import quanto.data._
import quanto.util.json.{JsonArray, JsonObject}

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

  def toJSON: JsonObject = {
    JsonObject(
      "centre" -> centre._2.toString,
      "size" -> this.members.length,
      "members" -> JsonArray(
        members.map(x => JsonObject(
          "adjMat" -> x._1.toString, "tensor" -> x._2.toString)
        ))
    )
  }
}

class EquivClassRunResults(val normalised: Boolean, angleMap: AngleMap, val tolerance: Double = 1e-14) {
  var equivalenceClasses: List[EquivalenceClass] = List()

  /** Adds the diagrams to classes, does not delete current classes first */
  def findEquivalenceClasses(adjMats: Stream[AdjMat]): List[EquivalenceClass] = {
    adjMats.foreach(
      x => compareAndAddToClass(x)
    )
    equivalenceClasses
  }

  // Returns (new Class, -1) if no EquivalenceClasses here
  def closestClassTo(that: AdjMat): (EquivalenceClass, Double) = {
    closestClassTo(interpret(that))
  }

  def interpret(adjMat: AdjMat): Tensor = {
    interpretAdjMat(adjMat, angleMap, angleMap)
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

  def adjMatToGraph(adj: AdjMat): Graph = {

    def vxToNodeV(v: Int): NodeV = {
      val (c, t) = adj.vertexColoursAndTypes(v)
      val JSON = c match {
        case VertexColour.Boundary => JsonObject(
          "annotation" -> JsonObject(
            "boundary" -> "true",
            "coord" -> JsonArray(v, 0)
          )
        )
        case VertexColour.Green => JsonObject(
          "data" -> JsonObject(
            "type" -> "Z",
            "value" -> angleMap(t)
          ),
          "annotation" -> JsonObject(
            "boundary" -> false,
            "coord" -> JsonArray(v, 1)
          )
        )
        case VertexColour.Red => JsonObject(
          "data" -> JsonObject(
            "type" -> "X",
            "value" -> angleMap(t)
          ),
          "annotation" -> JsonObject(
            "boundary" -> false,
            "coord" -> JsonArray(v, 2)
          )
        )
      }
      new NodeV(annotation = JSON)
    }

    var vxNames: List[VName] = List()

    var g = new Graph()
    for (v <- adj.mat.indices) {
      val nodeVersion = vxToNodeV(v)
      var (g2, name) = g.newVertex(nodeVersion)
      g = g2
      // g.addVertex(name, nodeVersion)
      vxNames = name :: vxNames
    }

    vxNames = vxNames.reverse

    for (i <- adj.mat.indices; j <- adj.mat.indices) {
      if (i < j && adj.mat(i)(j)) {
        g.newEdge(UndirEdge(), vxNames(i) -> vxNames(j))
      }
    }
    g
  }

  def toJSON: JsonObject = {
    JsonObject(
      "runData" -> JsonObject(
        "normalised" -> normalised,
        "smallestAngle" -> angleMap(1),
        "tolerance" -> tolerance
      ),
      "equivalenceClasses" -> JsonArray(equivalenceClasses.map(e => e.toJSON))
    )
  }
}

object EquivClassRunResults {
  def apply(normalised: Boolean, numAngles: Int, tolerance: Double = 1e-14): EquivClassRunResults = {
    def angleMap = (x: Int) => x * math.Pi * 2.0 / numAngles

    new EquivClassRunResults(normalised, angleMap, tolerance)
  }
}