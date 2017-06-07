package quanto.cosy

import quanto.cosy.Interpreter._

/**
  * Synthesises diagrams, holds the data and generates equivalence classes
  */

case class Diagram(graph: Graph, angleMap: AngleMap, tensor: Tensor) extends Ordering[Diagram] {
  // A Diagram is:
  // A graph, with given angle values, a tensor interpretation
  override def toString: String = "Diagram:\n Graph: \n" + graph.toString +
    "\n---- With angles:\n" +
    graph.redVertices.map(x => x.angleType).mkString(",") + ";" +
    graph.greenVertices.map(x => x.angleType).mkString(",") +
    "\n---- Tensor:\n" +
    tensor.toString

  // Compares the adjMat hashes
  def compare(x: Diagram, y: Diagram): Int = {
    x.graph.adjMat.hash.compareTo(y.graph.adjMat.hash)
  }
}

object Diagram {

  def apply(graph: Graph, numberOfAngles: Int): Diagram = {
    // TODO: Limit the number of angles checked
    def angleMap(x: Int): Double = x * 2.0 * math.Pi / numberOfAngles

    new Diagram(graph, angleMap, interpretGraph(graph, angleMap, angleMap))
  }
}

class EquivalenceClass(val centre: Diagram) {
  var members: List[Diagram] = List(centre)

  def addMember(diag: Diagram): EquivalenceClass = {
    members = diag :: members
    this
  }

  override def toString: String = {
    var s = "Equivalence class" +
      "\nSize: " + this.members.length +
      "\nCentre: " + this.centre.tensor.toString()
    //this.members.toString()
    s
  }
}

class EquivClassRunResults(val normalised: Boolean, val tolerance: Double = 1e-14) {
  var equivalenceClasses: List[EquivalenceClass] = List()

  /** Adds the diagrams to classes, does not delete current classes first */
  def findEquivalenceClasses(diagrams: Stream[Diagram]): List[EquivalenceClass] = {
    diagrams.foreach(
      x => compareAndAddToClass(x)
    )
    equivalenceClasses
  }

  // Finds the closest class and adds it, or creates a new class if outside tolerance
  def compareAndAddToClass(d: Diagram): EquivalenceClass = {
    var closest = new EquivalenceClass(d)
    var closestDist = tolerance
    for (eqClass <- equivalenceClasses) {

      // Need to ensure that the tensor is of the right size first!
      if (d.tensor.isSameShapeAs(eqClass.centre.tensor)) {
        var dist = tolerance
        if (normalised) {
          val rep = eqClass.centre.tensor.normalised
          val dn = d.tensor.normalised
          dist = math.min(rep.distance(dn), rep.distance(dn.scaled(that = -1.0)))
        } else {
          dist = eqClass.centre.tensor.distance(d.tensor)
        }
        if (dist < tolerance) {
          closest = eqClass
          closestDist = dist
        }
      }
    }

    if(closest.centre == d) equivalenceClasses = closest :: equivalenceClasses else closest.addMember(d)
    closest
  }

  // Returns (new Class, -1) if no EquivalenceClasses here
  def closestClassTo(that: Diagram): (EquivalenceClass, Double) = {
    var closestClass = new EquivalenceClass(that)
    var closestDistance: Double = -1
    for (eqc <- equivalenceClasses) {
      val d = eqc.centre.tensor.distance(that.tensor)
      if (closestDistance < 0 || d < closestDistance) {
        closestDistance = d
        closestClass = eqc
      }
    }
    (closestClass, closestDistance)
  }
}

object DiagramGenerator {

  def allDiagrams(boundaries: Int, vertices: Int, numberOfAngles: Int): Stream[Diagram] =
    ColbournReadEnum.enumerate(numberOfAngles, numberOfAngles, boundaries, vertices).
      map(x => Diagram(Graph(x), numberOfAngles))

}