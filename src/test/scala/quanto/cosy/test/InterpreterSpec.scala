package quanto.cosy.test

import org.scalatest.FlatSpec
import quanto.cosy
import quanto.cosy.BlockGenerators.QuickGraph
import quanto.cosy.Interpreter._
import quanto.cosy._
import quanto.data.Theory.{ValueDesc, ValueType, VertexDesc, VertexShape, VertexStyleDesc}
import quanto.data._
import quanto.util.json.JsonObject

/**
  * Created by hector on 24/05/17.
  */
class InterpreterSpec extends FlatSpec {

  behavior of "Connecting graphs"


  implicit def vname(str: String): VName = VName(str)

  implicit def vname2(strs: (String, String)): (VName, VName) = (vname(strs._1), vname(strs._2))

  implicit def ename(str: String): EName = EName(str)


  val cap: Tensor = Tensor(Array(Array[Complex](1, 0, 0, 1)))

  it should "make two id wires" in {
    var g = new Graph().
      addVertex(vname("v0"), WireV()).
      addVertex(vname("v1"), WireV()).
      addVertex(vname("v2"), WireV()).
      addVertex(vname("v3"), WireV()).
      addEdge("e0", UndirEdge(), "v0" -> "v1").
      addEdge("e1", UndirEdge(), "v2" -> "v3")
    val tensor = stringGraph(g, cap, List("v0", "v2"), List("v1", "v3"))
    assert(tensor == Tensor.idWires(2))
  }

  it should "make two caps" in {
    var g = new Graph().
      addVertex(vname("v0"), WireV()).
      addVertex(vname("v1"), WireV()).
      addVertex(vname("v2"), WireV()).
      addVertex(vname("v3"), WireV()).
      addEdge("e0", UndirEdge(), "v0" -> "v2").
      addEdge("e1", UndirEdge(), "v1" -> "v3")
    val tensor = stringGraph(g, cap, List("v0", "v2", "v1", "v3"), List())
    assert(tensor == (cap x cap))
  }


  it should "make cap and wire" in {
    var g = new Graph().
      addVertex(vname("v0"), WireV()).
      addVertex(vname("v1"), WireV()).
      addVertex(vname("v2"), WireV()).
      addVertex(vname("v3"), WireV()).
      addEdge("e0", UndirEdge(), "v0" -> "v3").
      addEdge("e1", UndirEdge(), "v1" -> "v2")
    val tensor = stringGraph(g, cap, List("v1", "v2", "v0"), List("v3"))
    assert(tensor == (cap x Tensor.idWires(1)))
  }


  it should "make wire and cap" in {
    var g = new Graph().
      addVertex(vname("v0"), WireV()).
      addVertex(vname("v1"), WireV()).
      addVertex(vname("v2"), WireV()).
      addVertex(vname("v3"), WireV()).
      addEdge("e0", UndirEdge(), "v0" -> "v3").
      addEdge("e1", UndirEdge(), "v1" -> "v2")
    val tensor = stringGraph(g, cap, List("v0", "v1", "v2"), List("v3"))
    assert(tensor == (Tensor.idWires(1) x cap))
  }


  it should "make cup and wire" in {
    var g = new Graph().
      addVertex(vname("v0"), WireV()).
      addVertex(vname("v1"), WireV()).
      addVertex(vname("v2"), WireV()).
      addVertex(vname("v3"), WireV()).
      addEdge("e0", UndirEdge(), "v0" -> "v3").
      addEdge("e1", UndirEdge(), "v1" -> "v2")
    val tensor = stringGraph(g, cap, List("v0"), List("v1", "v2", "v3"))
    assert(tensor == (cap.transpose x Tensor.idWires(1)))
  }


  it should "make wire and cup" in {
    var g = new Graph().
      addVertex(vname("v0"), WireV()).
      addVertex(vname("v1"), WireV()).
      addVertex(vname("v2"), WireV()).
      addVertex(vname("v3"), WireV()).
      addEdge("e0", UndirEdge(), "v0" -> "v3").
      addEdge("e1", UndirEdge(), "v1" -> "v2")
    val tensor = stringGraph(g, cap, List("v0"), List("v3", "v1", "v2"))
    assert(tensor == (Tensor.idWires(1) x cap.transpose))
  }


  it should "make crossing" in {
    var g = new Graph().
      addVertex(vname("v0"), WireV()).
      addVertex(vname("v1"), WireV()).
      addVertex(vname("v2"), WireV()).
      addVertex(vname("v3"), WireV()).
      addEdge("e0", UndirEdge(), "v0" -> "v3").
      addEdge("e1", UndirEdge(), "v1" -> "v2")
    val tensor = stringGraph(g, cap, List("v0", "v1"), List("v2", "v3"))
    assert(tensor == Tensor.swap(List(1, 0)))
  }


  it should "make cap and cup" in {
    var g = new Graph().
      addVertex(vname("v0"), WireV()).
      addVertex(vname("v1"), WireV()).
      addVertex(vname("v2"), WireV()).
      addVertex(vname("v3"), WireV()).
      addEdge("e0", UndirEdge(), "v0" -> "v3").
      addEdge("e1", UndirEdge(), "v1" -> "v2")
    val tensor = stringGraph(g, cap, List("v0", "v3"), List("v1", "v2"))
    assert(tensor == Tensor(Array(Array(1, 0, 0, 1), Array(0, 0, 0, 0), Array(0, 0, 0, 0), Array(1, 0, 0, 1))))
  }


  it should "make (id x s) o (s x id)" in {
    var g = QuickGraph.apply(BlockGenerators.ZXTheory).addInput(3).addOutput(3)
      .join("i-0", "o-2")
      .join("i-1", "o-0")
      .join("i-2", "o-1")
    val tensor = stringGraph(g, cap, List("i-0", "i-1", "i-2"), List("o-0", "o-1", "o-2"))
    assert(tensor == Tensor.swap(List(2, 0, 1)))
  }


  it should "make (s x id) o (id x s)" in {
    var g = QuickGraph.apply(BlockGenerators.ZXTheory).addInput(3).addOutput(3)
      .join("i-0", "o-1")
      .join("i-1", "o-2")
      .join("i-2", "o-0")
    val tensor = stringGraph(g, cap, List("i-0", "i-1", "i-2"), List("o-0", "o-1", "o-2"))
    assert(tensor == Tensor.swap(List(1, 2, 0)))
  }

  behavior of "ZX"
  val rg = Theory.fromFile("red_green")

  val pi = math.Pi
  val rdata = Vector(
    NodeV(data = JsonObject("type" -> "X", "value" -> "0"), theory = rg),
    NodeV(data = JsonObject("type" -> "X", "value" -> "1"), theory = rg),
    NodeV(data = JsonObject("type" -> "X", "value" -> "1/2"), theory = rg),
    NodeV(data = JsonObject("type" -> "X", "value" -> "-1/2"), theory = rg)
  )
  val gdata = Vector(
    NodeV(data = JsonObject("type" -> "Z", "value" -> "0"), theory = rg),
    NodeV(data = JsonObject("type" -> "Z", "value" -> "1"), theory = rg),
    NodeV(data = JsonObject("type" -> "Z", "value" -> "1/2"), theory = rg),
    NodeV(data = JsonObject("type" -> "Z", "value" -> "-1/2"), theory = rg)
  )
  //Change the last number for larger tests
  //Don't include boundaries as the methods can give permutations of each other's answers
  val smallAdjMats: Stream[AdjMat] = ColbournReadEnum.enumerate(2, 2, 2, 2)

  implicit def quickGraph(amat: AdjMat): Graph = cosy.AdjMat.toZXGraph(amat, rdata, gdata)

  implicit def stringToPhase(s: String): PhaseExpression = {
    PhaseExpression.parse(s, ValueType.AngleExpr)
  }

  var one = Complex.one
  var zero = Complex.zero

  def amatToZXTensor(adjMat: AdjMat) = Interpreter.interpretZXAdjMat(adjMat, rdata, gdata)

  it should "make hadamards" in {
    // Via "new"
    var h3 = Interpreter.makeHadamards(2)
    assert(h3.toString == " 0.5000000000000001  0.5000000000000001  0.5000000000000001  0.5000000000000001\n" +
      " 0.5000000000000001 -0.5000000000000001  0.5000000000000001 -0.5000000000000001\n" +
      " 0.5000000000000001  0.5000000000000001 -0.5000000000000001 -0.5000000000000001\n" +
      " 0.5000000000000001 -0.5000000000000001 -0.5000000000000001  0.5000000000000001")
    var h33 = h3 o h3
    assert((h33 - Tensor.id(4)).isRoughly(Tensor.zero(4, 4)))
  }

  it should "make Green Spiders" in {
    def gs(angle: String, in: Int, out: Int) = zxSpider(true, angle, in, out)

    var g12a0 = zxSpider(true, "0", 1, 2)
    var g21aPi = zxSpider(true, "pi", 2, 1)
    var g11aPi2 = zxSpider(true, "pi/2", 1, 1)
    var t = gs("pi/4", 1, 1)
    var g2 = gs("pi/2", 1, 1)
    assert((t o t).isRoughly(g2))
    assert((g2 o g2).isRoughly(gs("pi", 1, 1)))
  }

  it should "make red spiders" in {
    def rs(angle: String, in: Int, out: Int) = zxSpider(false, angle, in, out)

    assert(rs("pi", 1, 1).isRoughlyUpToScalar(Tensor(Array(Array(0, 1), Array(1, 0)))))
    var rp2 = rs("pi/2", 1, 1)
    assert((rp2 o rp2).isRoughly(rs("pi", 1, 1)))
    assert((rp2 o rp2 o rp2 o rp2).isRoughly(Tensor.id(2)))
    assert(Interpreter.cached.contains("ZX:red:1:1:1"))
  }

  it should "respect spider law" in {
    var g1 = zxSpider(true, "pi/8", 1, 2)
    var g2 = zxSpider(true, "pi/8", 1, 1)
    var g3 = zxSpider(true, "pi/4", 1, 2)
    assert(((Tensor.id(2) x g2) o g1).isRoughly(g3))
  }

  it should "apply Hadamards" in {
    var h1 = Tensor.hadamard
    var h2 = h1 x h1
    var g = zxSpider(true, "pi/4", 1, 2)
    var r = zxSpider(false, "pi/4", 1, 2)
    assert((h2 o r o h1).isRoughly(g))
  }

  it should "process green 0" in {
    var amat = new AdjMat(numRedTypes = 0, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true, true))
    println(amat)
    val i1 = amatToZXTensor(amat)
    val i2 = amatToGraphToZXTensor(amat)
    assert(i1.isRoughly(i2))
    assert(i1.isRoughly(Tensor(Array(Array(1, 0, 0, 1)))))
  }


  it should "process red pi" in {
    var amat = new AdjMat(numRedTypes = 2, numGreenTypes = 2)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    // Red 0
    amat = amat.nextType.get
    // Red pi
    amat = amat.addVertex(Vector(true, true))
    amat = amat.nextType.get
    // Green 0
    amat = amat.nextType.get
    // Green pi
    println(amat)
    val i1 = amatToZXTensor(amat)
    val i2 = amatToGraphToZXTensor(amat)
    assert(i1.isRoughly(i2))
    assert(i1.isRoughly(zxSpider(false, "pi", 2, 0)))
  }

  def amatToGraphToZXTensor(adjMat: AdjMat) = {
    val asGraph = quickGraph(adjMat)
    Interpreter.interpretZXGraph(asGraph,
      asGraph.verts.filter(asGraph.isBoundary).toList.sortBy(vn => vn.s), List())
  }

  it should "process red spider law" in {
    // Simple red and green identities
    var amat = new AdjMat(numRedTypes = 4, numGreenTypes = 4)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    // red 0
    amat = amat.nextType.get
    // red pi
    amat = amat.addVertex(Vector(true, false))
    amat = amat.nextType.get
    // red pi/2
    amat = amat.addVertex(Vector(false, true, true))
    amat = amat.nextType.get
    //red -pi/2
    amat = amat.nextType.get
    val i1 = amatToZXTensor(amat)
    val i2 = amatToGraphToZXTensor(amat)
    assert(i1.isRoughly(i2))
    assert(i1.isRoughly(zxSpider(false, "-pi / 2", 2, 0)))
  }

  it should "satisfy the Euler identity" in {
    // Euler identity
    var amat = new AdjMat(numRedTypes = 4, numGreenTypes = 4)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    // Reds 0
    amat = amat.nextType.get
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true, false))
    amat = amat.addVertex(Vector(false, true, false))
    amat = amat.nextType.get
    amat = amat.nextType.get
    // Greens 0
    amat = amat.nextType.get
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(false, false, true, true))
    amat = amat.nextType.get
    println(amat)
    val i1 = Interpreter.interpretZXAdjMat(amat, redAM = rdata, greenAM = gdata)
    val i2 = amatToGraphToZXTensor(amat)
    assert(i2.isRoughly(i2))
    val i3 = zxSpider(true, "0", 2, 0) o (Tensor.id(2) x Tensor.hadamard)
    println(i3.scaled(i1.contents(0)(0) / i3.contents(0)(0)))
    assert(i1.isRoughly(i3.scaled(i1.contents(0)(0) / i3.contents(0)(0))))
    assert(i1.isRoughlyUpToScalar(Tensor(Array(Array(1, 1, 1, -1)))))
  }

  behavior of "ZW"

  it should "Evaluate the four-output GHZ spider" in {
    val t1 = Interpreter.interpretZWSpiderNoInputs(black = true, 4)
    val g = QuickGraph(Theory.fromFile("ZW")).addOutput(4).node("w", nodeName = "w")
      .join("w", "o-0")
      .join("w", "o-1")
      .join("w", "o-2")
      .join("w", "o-3")

    val t2 = Interpreter.interpretSpiderGraph(zwSpiderInterpreter)(g, List(), List("o-0", "o-1", "o-2", "o-3"))
    val t3 = Tensor(Array(Array(1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0))).transpose
    assert(t1 == t3)
    assert(t2 == t3)
  }

  it should "make w spiders" in {
    val g = QuickGraph(Theory.fromFile("ZW")).addInput().addOutput().node("w", nodeName = "w")
      .join("i-0", "w").join("w", "o-0")


    val t1 = Interpreter.interpretSpiderGraph(zwSpiderInterpreter)(g, List("i-0"), List("o-0"))
    assert(t1.isRoughly(Tensor(Array(Array(0, 1), Array(1, 0)))))


    val t2 = Interpreter.interpretSpiderGraph(zwSpiderInterpreter)(g, List("i-0", "o-0"), List())
    assert(t2.isRoughly(Tensor(Array(Array(0, 1, 1, 0)))))


    val g3 = QuickGraph(Theory.fromFile("ZW")).addInput().addOutput(2).node("w", nodeName = "w")
      .join("i-0", "w").join("w", "o-0").join("w", "o-1")


    val t3 = Interpreter.interpretSpiderGraph(zwSpiderInterpreter)(g3, List("i-0"), List("o-0", "o-1"))
    assert(t3.isRoughly(Tensor(Array(Array(0, 1, 1, 0), Array(1, 0, 0, 0))).transpose))
  }

  it should "agree on rule nat_c^n" in {

    var amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.addVertex(Vector(false, false))
    amat = amat.nextType.get
    // Blacks (reds)
    amat = amat.addVertex(Vector(true, false, false))
    amat = amat.nextType.get
    // Whites (greens)
    amat = amat.addVertex(Vector(false, false, false, true))
    amat = amat.addVertex(Vector(false, true, true, false, true))

    var rhs = amat.copy()

    amat = new AdjMat(numRedTypes = 1, numGreenTypes = 1)
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.addVertex(Vector(false, false))
    amat = amat.nextType.get
    // Blacks (reds)
    amat = amat.addVertex(Vector(false, true, false))
    amat = amat.addVertex(Vector(false, false, true, false))
    amat = amat.nextType.get
    // Whites (greens)
    amat = amat.addVertex(Vector(false, false, false, true, true))
    amat = amat.addVertex(Vector(true, false, false, false, false, true))

    var t1 = Interpreter.interpretZWAdjMat(rhs, List("v0"), List("v1", "v2"))
    var t2 = Interpreter.interpretZWAdjMat(amat, List("v0"), List("v1", "v2"))
    assert(t1.isRoughly(t2))
  }

  it should "agree on rule inv" in {

    val g = QuickGraph(Theory.fromFile("ZW")).addInput().addOutput().node("w", nodeName = "w1").node("w", nodeName = "w2")
      .join("i-0", "w1").join("w1", "w2").join("w2", "o-0")


    var t1 = Interpreter.interpretSpiderGraph(zwSpiderInterpreter)(g, List("i-0"), List("o-0"))
    assert(t1.isRoughly(Tensor.idWires(1)))
  }

  behavior of "block stacks and graph interpreters"

  def zxSpider(isGreen: Boolean, angle: String, inputs: Int, outputs: Int): Tensor =
    Interpreter.interpretZXSpider(ZXAngleData(isGreen, angle), inputs, outputs)

  it should "interpret Z pi/4" in {

    val cnotGraph = QuickGraph(BlockGenerators.ZXTheory).addInput(1).addOutput(1).node("Z", nodeName = "z", angle = "1/4")
      .join("i-0", "z")
      .join("o-0", "z")

    val gt = Interpreter.interpretZXGraph(cnotGraph, List("i-0"), List("o-0"))
    assert(gt.isRoughly(zxSpider(true, "1/4", 1, 1)))
  }


  it should "interpret X pi/4" in {

    val cnotGraph = QuickGraph(BlockGenerators.ZXTheory).addInput(1).addOutput(1).node("X", nodeName = "x", angle = "1/4")
      .join("i-0", "x")
      .join("o-0", "x")

    val gt = Interpreter.interpretZXGraph(cnotGraph, List("i-0"), List("o-0"))
    assert(gt.isRoughly(zxSpider(false, "1/4", 1, 1)))
  }


  it should "interpret X pi" in {

    val cnotGraph = QuickGraph(BlockGenerators.ZXTheory).addInput(1).addOutput(1).node("X", nodeName = "x", angle = "1")
      .join("i-0", "x")
      .join("o-0", "x")

    val gt = Interpreter.interpretZXGraph(cnotGraph, List("i-0"), List("o-0"))
    assert(gt.isRoughly(zxSpider(false, "1", 1, 1)))
  }


  it should "interpret CNOT" in {
    val cnotGraph = QuickGraph(BlockGenerators.ZXTheory).addInput(2).addOutput(2).node("Z", nodeName = "z").node("X", xCoord = 1, nodeName = "x")
      .join("i-0", "z").join("i-1", "x")
      .join("o-0", "z").join("o-1", "x")
      .join("z", "x")


    assert(BlockGenerators.ZXClifford.filter(_.name == "CNT").head.tensor.isRoughlyUpToScalar(
      Tensor(Array(Array(1, 0, 0, 0), Array(0, 1, 0, 0), Array(0, 0, 0, 1), Array(0, 0, 1, 0)))))

    assert(Interpreter.interpretZXGraph(cnotGraph, List("i-0", "i-1"), List("o-0", "o-1")).isRoughlyUpToScalar(
      Tensor(Array(Array(1, 0, 0, 0), Array(0, 1, 0, 0), Array(0, 0, 0, 1), Array(0, 0, 1, 0)))))

  }

  it should "interpret CNOT x id" in {
    val cnotGraph = QuickGraph(BlockGenerators.ZXTheory).addInput(3).addOutput(3).node("Z", nodeName = "z").node("X", xCoord = 1, nodeName = "x")
      .join("i-0", "z").join("i-1", "x")
      .join("o-0", "z").join("o-1", "x")
      .join("z", "x")
      .join("i-2", "o-2")

    val height = 1
    val width = 3
    val rows = BlockRowMaker.makeRowsUpToSize(width, BlockGenerators.ZXCNOT, Some(width))
    val stacks = BlockStackMaker.makeStacksOfSize(height, rows)
    var gate = stacks.filter(_.toString == "(CNT x  1 )")

    val breakdown = Tensor.swap(List(2, 1, 0)) o
      (Tensor.idWires(2) x Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 0, 1)))) o
      Tensor.swap(List(2, 0, 1, 3)) o
      (Tensor.idWires(2) x Tensor(Array(Array(1, 0, 0, 1), Array(0, 1, 1, 0))).transpose) o
      Tensor.swap(List(0, 2, 1))


    val breakdown2 = Tensor(Array(Array(1, 0, 0, 0), Array(0, 1, 0, 0), Array(0, 0, 0, 1), Array(0, 0, 1, 0))) x Tensor.idWires(1)

    assert(breakdown.isRoughlyUpToScalar(breakdown2))


    assert(Interpreter.interpretZXGraph(cnotGraph, List("i-0", "i-1", "i-2"), List("o-0", "o-1", "o-2")).isRoughlyUpToScalar(
      breakdown
    ))


    assert(Interpreter.interpretZXGraph(cnotGraph, List("i-0", "i-1", "i-2"), List("o-0", "o-1", "o-2")).isRoughlyUpToScalar(
      gate.head.tensor
    ))

  }


  it should "interpret green strings" in {
    val cnotGraph = QuickGraph(BlockGenerators.ZXTheory).addInput(3).addOutput(3)
      .node("Z", nodeName = "z2")
      .node("Z", nodeName = "z1")
      .node("Z", nodeName = "z0")
      .join("i-0", "z0").join("z0", "o-0")
      .join("i-1", "z1").join("z1", "o-1")
      .join("i-2", "z2").join("z2", "o-2")

    assert(Interpreter.interpretZXGraph(cnotGraph, List("i-0", "i-1", "i-2"), List("o-0", "o-1", "o-2")).isRoughlyUpToScalar(
      Tensor.idWires(3)))

  }


  it should "agree on small adjmats" in {
    val errors: List[AdjMat] = smallAdjMats.filterNot(adj => {
      val am = amatToZXTensor(adj)
      val gr = amatToGraphToZXTensor(adj)
      am.isRoughly(gr)
    }).toList
    assert(errors.isEmpty)
  }

  it should "agree between block stacks and spiders" in {
    // Will only work up to width 10 unless you change how boundaries are sorted
    val height = 1
    val width = 3
    val rows = BlockRowMaker.makeRowsUpToSize(width, BlockGenerators.ZXClifford.filterNot(b => b.toString == " H "), Some(width))
    val stacks = BlockStackMaker.makeStacksOfSize(height, rows)
    stacks.foreach(bs => {
      val asGraph = bs.graph
      val tensorFromGraph = Interpreter.interpretZXGraph(asGraph,
        asGraph.verts.filter(_.s.matches(raw"r-0-i-\d+")).toList.sortBy(vn => vn.s),
        asGraph.verts.filter(_.s.matches(raw"r-" + (height - 1) + raw"-o-\d+")).toList.sortBy(vn => vn.s))
      assert(bs.tensor.isRoughlyUpToScalar(tensorFromGraph))
      val asGraph2 = bs.graph
      val tensorFromGraph2 = Interpreter.interpretZXGraph(asGraph,
        asGraph.verts.filter(_.s.matches(raw"r-0-i-\d+")).toList.sortBy(vn => vn.s),
        asGraph.verts.filter(_.s.matches(raw"r-" + (height - 1) + raw"-o-\d+")).toList.sortBy(vn => vn.s))
      assert(bs.tensor.isRoughlyUpToScalar(tensorFromGraph))
    }
    )
  }

  behavior of "ZH interpreter"

  val zhTheory: Theory = Theories.ZH

  it should "generate the right tensors for generalised hadamard" in {
    // Normal hadamard
    val hm1 = ZHSpiderData(ZHSpiderType.H, -1, 0)
    val in = zhSpiderInterpreter(hm1, 1, 1)
    assert(in.isRoughlyUpToScalar(Tensor.hadamard))

    // Hadamard with phase 1
    val h1 = ZHSpiderData(ZHSpiderType.H, 1, 0)
    val in2 = zhSpiderInterpreter(h1, 1, 1)
    assert(in2 == Tensor(2, 2, (_, _) => 1))

    // Hadamard with 2 inputs, phase 0
    val h2 = ZHSpiderData(ZHSpiderType.H, 0, 0)
    val in3 = zhSpiderInterpreter(h2, 2, 1)
    assert(in3.width == 4)
    assert(in3.height == 2)
    assert(in3.contents(1)(3) == Complex.zero)
  }

  it should "generate the right tensors for Z" in {
    val z21 = zhSpiderInterpreter(ZHSpiderData(ZHSpiderType.Z, 0, 0), 2, 1)
    assert(z21.width == 4)
    assert(z21.height == 2)
    assert(z21.isRoughly(Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 0, 1)))))
  }

  it should "obey (HS2)" in {
    val hm1 = ZHSpiderData(ZHSpiderType.H, -1, 0)
    val in = zhSpiderInterpreter(hm1, 1, 1)
    assert((in o in).isRoughlyUpToScalar(Tensor.idWires(1)))

    var amat = AdjMat.emptyZH()
    amat = amat.addVertex(Vector())
    amat = amat.addVertex(Vector(false))
    amat = amat.nextType.get
    amat = amat.addVertex(Vector(true, true))
    val g = amat.toZHGraph()

    val i1 = Interpreter.interpretZHGraph(g, List("v0"), List("v1"))
    assert(i1 ~ Tensor.hadamard)

    val g2 = AdjMat.emptyZH()
      .addVertex(Vector())
      .addVertex(Vector(false))
      .nextType.get
      .addVertex(Vector(true, false))
      .addVertex(Vector(false, true, true))
      .toZHGraph()

    val i2 = Interpreter.interpretZHGraph(g2, List("v0"), List("v1"))

    assert(i2 ~ Tensor.idWires(1))
  }


  it should "obey (ZS2)" in {
    val amat = AdjMat.emptyZH()
      .addVertex(Vector())
      .addVertex(Vector(false))
      .nextType.get // -1
      .nextType.get // i
      .nextType.get // 2
      .nextType.get // Z
      .addVertex(Vector(true, true))

    val g = amat.toZHGraph()

    val i = Interpreter.interpretZHGraph(g, List("v0"), List("v1"))
    assert(i ~ Tensor.idWires(1))
  }

}
