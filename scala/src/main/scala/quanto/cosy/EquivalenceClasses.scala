package quanto.cosy

import quanto.cosy.Interpreter._

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

  def interpret(adjMat: AdjMat): Tensor = {
    def redAM = (x: Int) => angleMap(x)

    def greenAM = (x: Int) => angleMap(x - adjMat.red.sum)

    interpretAdjMat(adjMat, greenAM, redAM)
  }

  def add(adjMat: AdjMat): EquivalenceClass = {
    compareAndAddToClass(adjMat)
  }
}

object EquivClassRunResults {
  def apply(normalised: Boolean, numAngles: Int, tolerance: Double = 1e-14): EquivClassRunResults = {
    def angleMap = (x: Int) => x * math.Pi * 2.0 / numAngles

    new EquivClassRunResults(normalised, angleMap, tolerance)
  }
}