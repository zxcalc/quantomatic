package quanto.cosy

import quanto.data._
import quanto.util.json.JsonObject
import quanto.data.Names._

object BlockGenerators {


  implicit def quickList(n: Int): List[Int] = {
    n match {
      case 0 => List()
      case 1 => List(0)
      case m => quickList(m - 1) ::: List(0)
    }
  }


  class QuickGraph(graph: Graph) {
    val _g : Graph = graph
    def node(nodeType: String, angle: String = "", xCoord : Double = 0, nodeName : String = "v-0") : QuickGraph = {
      val name = _g.verts.freshWithSuggestion(VName(nodeName))
      val data = NodeV(data = JsonObject("type" -> nodeType, "value" -> angle)).withCoord((xCoord, 0))
      QuickGraph(_g.addVertex(name, data))
    }
    def addInput(count : Int = 1) : QuickGraph = {
      count match {
        case 0 => this
        case 1 =>
          val name = _g.verts.freshWithSuggestion(VName("i-0"))
          val data = WireV().withCoord(name.suffix,-0.5)
          QuickGraph(_g.addVertex(name, data))
        case n =>
          val name = _g.verts.freshWithSuggestion(VName("i-0"))
          val data = WireV().withCoord(name.suffix,-0.5)
          QuickGraph(_g.addVertex(name, data)).addInput(count -1)
      }
    }
    def addOutput(count: Int = 1) : QuickGraph = {
      count match {
        case 0 => this
        case 1 =>
          val name = _g.verts.freshWithSuggestion(VName("o-0"))
          val data = WireV().withCoord(name.suffix,0.5)
          QuickGraph(_g.addVertex(name, data))
        case n =>
          val name = _g.verts.freshWithSuggestion(VName("o-0"))
          val data = WireV().withCoord(name.suffix,0.5)
          QuickGraph(_g.addVertex(name, data)).addOutput(count -1)
      }
    }

    def join(s1 : String, s2: String) : QuickGraph = {
      val name = _g.edges.freshWithSuggestion("e-0")
      val data = UndirEdge()
      val v1 = VName(s1)
      val v2 = VName(s2)
      QuickGraph(_g.addEdge(name, data, v1 -> v2))
    }

    def joinIfNotAlready(s1: String, s2: String) : QuickGraph = {
      val isJoined = _g.adjacentVerts(s1).contains(s2)
      if(!isJoined){
        this.join(s1, s2)
      }else{
        this
      }
    }

    def apply() : Graph = _g
  }

  object QuickGraph {
    def apply(graph: Graph) = new QuickGraph(graph)
    def apply(theory: Theory) = new QuickGraph(
      new Graph(GData(data = new JsonObject(), annotation = new JsonObject(), theory = theory))
    )

    implicit def slow(qg: QuickGraph) : Graph = qg()
  }

  val ZX : Theory = Theory.fromFile("ZX")


  val ZXClifford: List[Block] = List(
    Block(1, 1, " 1 ", Tensor.idWires(1), QuickGraph(ZX).addInput().addOutput().join("i-0", "o-0")),
    Block(2, 2, " s ", Tensor.swap(List(1, 0)),
      QuickGraph(ZX).addInput(2).addOutput(2).join("i-0", "o-1").join("i-1", "o-0")),
    Block(1, 1, " H ", Hadamard(2), QuickGraph(ZX).addInput().addOutput()
      .node("hadamard", nodeName = "h").join("i-0", "h").join("h", "o-0")),
    Block(2, 2, "CNT", Tensor(Array(Array(1, 0, 0, 0), Array(0, 1, 0, 0), Array(0, 0, 0, 1), Array(0, 0, 1, 0))),
      QuickGraph(ZX).addInput(2).addOutput(2).node("Z", nodeName = "z").node("X", xCoord = 1, nodeName = "x")
        .join("i-0", "z").join("i-1", "x")
        .join("o-0", "z").join("o-1", "x")
        .join("z", "x"))
  )

  val BellTeleportation: List[Block] = List(
    Block(List(0), List(0), " A ", Tensor.id(2)),
    Block(List(-1), List(-1), " B ", Tensor.id(2)),
    Block(List(0, 0), List(1), " m1", Tensor(Array(Array(1, 0, 0, 1)))),
    Block(List(0, 0), List(2), " m2", Tensor(Array(Array(1, 0, 0, -1)))),
    Block(List(0, 0), List(3), " m3", Tensor(Array(Array(0, 1, 1, 0)))),
    Block(List(0, 0), List(4), " m4", Tensor(Array(Array(0, 1, -1, 0)))),
    Block(List(-1, 1), List(-1), " c1", Tensor(Array(Array(1, 0), Array(0, 1)))),
    Block(List(-1, 2), List(-1), " c2", Tensor(Array(Array(1, 0), Array(0, -1)))),
    Block(List(-1, 3), List(-1), " c3", Tensor(Array(Array(0, 1), Array(1, 0)))),
    Block(List(-1, 4), List(-1), " c4", Tensor(Array(Array(0, 1), Array(-1, 0)))),
    Block(List(), List(0, -1), " p ", Tensor(Array(Array(1, 0, 0, 1))).transpose)
  ) :::
    swapQuantumClassical(List(0, -1), Tensor.id(2), List(1, 2, 3, 4)) :::
    makeClassicalIdentites(List(1, 2, 3, 4))
  val ZW: List[Block] = List(
    // BOTTOM TO TOP!
    Block(1, 1, " 1 ", Tensor.idWires(1)),
    Block(2, 2, " s ", Tensor.swap(List(1, 0))),
    Block(2, 2, "crs", Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 1, 0), Array(0, 1, 0, 0), Array(0, 0, 0, -1)))),
    Block(0, 2, "cup", Tensor(Array(Array(1, 0, 0, 1))).transpose),
    Block(2, 0, "cap", Tensor(Array(Array(1, 0, 0, 1)))),
    Block(1, 1, " w ", Tensor(Array(Array(1, 0), Array(0, -1)))),
    Block(1, 2, "1w2", Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 0, -1))).transpose),
    Block(2, 1, "2w1", Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 0, -1)))),
    Block(1, 1, " b ", Tensor(Array(Array(0, 1), Array(1, 0)))),
    Block(1, 2, "1b2", Tensor(Array(Array(0, 1, 1, 0), Array(1, 0, 0, 1))).transpose),
    Block(2, 1, "2b1", Tensor(Array(Array(0, 1, 1, 0), Array(1, 0, 0, 1))))
  )

  def swapQuantumClassical(listQuantum: List[Int], quantumTensor: Tensor, listClassical: List[Int]): List[Block] = {
    (for (w1 <- listQuantum; w2 <- listClassical) yield {
      List(Block(List(w1, w2), List(w2, w1), w1 + "s" + w2, quantumTensor),
        Block(List(w2, w1), List(w1, w2), w2 + "s" + w1, quantumTensor))
    }).flatten
  }

  def swapQuantumQuantum(listQuantum: List[Int], quantumTensor: Tensor): List[Block] = {
    (for (w1 <- listQuantum; w2 <- listQuantum) yield {
      List(Block(List(w1, w2), List(w2, w1), w1 + "s" + w2, quantumTensor),
        Block(List(w2, w1), List(w1, w2), w2 + "s" + w1, quantumTensor))
    }).flatten
  }

  def makeClassicalIdentites(listClassical: List[Int]): List[Block] = {
    for (w <- listClassical) yield {
      Block(List(w), List(w), "w" + w + " ", Tensor.id(1))
    }
  }

  // Traditionally the number of angles is 3 (Clifford) or 9 (Clifford+T)
  def ZXQudit(dimension: Int, numAngles: Int): List[Block] = {
    require(dimension > 1)

    def swapIndex(i: Int): Int = {
      val left: Int = i / dimension
      val right = i % dimension
      right * dimension + left
    }

    val H: Tensor = Hadamard(dimension)

    val greenFork = Tensor(dimension, dimension * dimension,
      (i, j) => if (j == i * (dimension + 1)) Complex.one else Complex.zero)

    // Go through the diagonal entries creating all the different spiders
    val greenBlocks = (1 until dimension).foldLeft(
      List(
        Block(1, 1, "g", Tensor(dimension, dimension, (i, j) => if (i == 0 && j == 0) Complex.one else Complex.zero))
      )
    )((lb, i) => lb.flatMap(b => (0 until numAngles).map(x =>
      Block(1, 1, b.name + "|" + x, b.tensor + Tensor(dimension, dimension, (j, k) =>
        if (j == i && k == i) ei(x * 2 * math.Pi / numAngles) else Complex.zero)
      ))))

    List(
      Block(1, 1, " 1 ", Tensor.id(dimension)),
      Block(2, 2, " s ", Tensor.permutation((0 until dimension * dimension).toList.map(x => swapIndex(x)))),
      Block(1, 1, " H ", H),
      Block(1, 1, " H'", H.dagger),
      Block(2, 1, "2g1", greenFork),
      Block(1, 2, "1g2", greenFork.dagger),
      Block(0, 1, "gu ", Tensor(dimension, 1, (_, _) => Complex.one).scaled(1.0 / math.sqrt(dimension))),
      Block(1, 2, "1r2", (H.dagger o greenFork o (H x H)).dagger),
      Block(2, 1, "2r1", H.dagger o greenFork o (H x H)),
      Block(0, 1, "ru ", Tensor(dimension, 1, (i, j) => if (i == 0 && j == 0) Complex.one else Complex.zero))
    ) ::: greenBlocks ::: greenBlocks.map(b =>
      Block(1, 1, "r" + b.name.tail, H.dagger o b.tensor o H)
    )
  }

  def Hadamard(dimension: Int): Tensor =
    Tensor(dimension, dimension, (i, j) => ei(2 * math.Pi * i * j / dimension)).scaled(1 / math.sqrt(dimension))

  // Traditionally the number of angles is 3 (Clifford) or 9 (Clifford+T)
  def ZXQutrit(numAngles: Int = 9): List[Block] = {
    val H3 = Hadamard(3)
    List(
      Block(1, 1, " 1 ", Tensor.id(3)),
      Block(2, 2, " s ", Tensor.permutation(List(0, 3, 6, 1, 4, 7, 2, 5, 8))),
      Block(1, 1, " H ", H3),
      Block(1, 1, " H'", H3.dagger),
      Block(2, 1, "2g1", Tensor(Array(
        Array(1, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 1, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 1)
      ))),
      Block(1, 2, "1g2", Tensor(Array(
        Array(1, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 1, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 1)
      )).transpose),
      Block(0, 1, "gu ", Tensor(Array(Array(1, 1, 1))).scaled(1.0 / math.sqrt(3)).transpose),
      Block(1, 2, "1r2", (H3.dagger o Tensor(Array(
        Array(1, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 1, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 1)
      )) o (H3 x H3)).dagger),
      Block(2, 1, "2r1", H3.dagger o Tensor(Array(
        Array(1, 0, 0, 0, 0, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 1, 0, 0, 0, 0),
        Array(0, 0, 0, 0, 0, 0, 0, 0, 1)
      )) o (H3 x H3)),
      Block(0, 1, "ru ", Tensor(Array(Array(1, 0, 0))).transpose)
    ) :::
      (for (i <- 0 until numAngles; j <- 0 until numAngles) yield {
        val gs = Tensor(Array(
          Array[Complex](1, 0, 0),
          Array[Complex](0, ei(i * 2 * math.Pi / numAngles), 0),
          Array[Complex](0, 0, ei(j * 2 * math.Pi / numAngles))
        ))
        List(Block(1, 1, "g|" + i.toString + "|" + j.toString, gs),
          Block(1, 1, "r|" + i.toString + "|" + j.toString, H3.dagger o gs o H3))
      }).flatten.toList
  }

  def ZX(numAngles: Int = 8): List[Block] = List(
    Block(1, 1, " 1 ", Tensor.idWires(1)),
    Block(2, 2, " s ", Tensor.swap(List(1, 0))),
    Block(0, 2, "cup", Tensor(Array(Array(1, 0, 0, 1))).transpose),
    Block(2, 0, "cap", Tensor(Array(Array(1, 0, 0, 1))))) :::
    (for (i <- 0 until numAngles) yield {
      Block(1, 1, "gT" + i.toString, Tensor(Array(
        Array(Complex.one, Complex.zero),
        Array(Complex.zero, ei(2 * i * math.Pi / numAngles)))))
    }).toList :::
    (for (i <- 0 until numAngles) yield {
      Block(1, 1, "rT" + i.toString, new Tensor(Array(
        Array(1 + ei(2 * i * math.Pi / numAngles), 1 - ei(2 * i * math.Pi / numAngles)),
        Array(1 - ei(2 * i * math.Pi / numAngles), 1 + ei(2 * i * math.Pi / numAngles)))))
    }).toList :::
    List(
      Block(1, 1, " H ", Tensor(Array(Array(1, 1), Array(1, -1))).scaled(1.0 / math.sqrt(2))),
      Block(2, 1, "2g1", Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 0, 1)))),
      Block(1, 2, "1g2", Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 0, 1))).transpose),
      Block(0, 1, "gu ", Tensor(Array(Array(1, 1))).transpose),
      Block(1, 2, "1r2", Tensor(Array(Array(1, 0, 0, 1), Array(0, 1, 1, 0))).transpose),
      Block(2, 1, "2r1", Tensor(Array(Array(1, 0, 0, 1), Array(0, 1, 1, 0)))),
      Block(0, 1, "ru ", Tensor(Array(Array(1, 0))).transpose)
    )

  def StandardCircuit(numAngles: Int = 8): List[Block] = List(
    Block(1, 1, " 1 ", Tensor.idWires(1)),
    Block(2, 2, " s ", Tensor.swap(List(1, 0))),
    Block(1, 1, " H ", Tensor(Array(Array(1, 1), Array(1, -1))).scaled(1.0 / math.sqrt(2)))) :::
    (for (i <- 0 until numAngles) yield {
      Block(1, 1, "gT" + i.toString, Tensor(Array(
        Array(Complex.one, Complex.zero),
        Array(Complex.zero, ei(2 * i * math.Pi / numAngles)))))
    }).toList :::
    (for (i <- 0 until numAngles) yield {
      Block(1, 1, "rT" + i.toString, new Tensor(Array(
        Array(1 + ei(2 * i * math.Pi / numAngles), 1 - ei(2 * i * math.Pi / numAngles)),
        Array(1 - ei(2 * i * math.Pi / numAngles), 1 + ei(2 * i * math.Pi / numAngles)))))
    }).toList

  private def ei(angle: Double) = Complex(math.cos(angle), math.sin(angle))

  def Bian2Qubit: List[Block] = List(
    // Block(0, 0, " w ", Tensor.id(1).scaled(ei(math.Pi / 4))), Ignored for now.
    Block(1, 1, " 1 ", Tensor.id(2)),
    Block(2, 2, " Zc", Tensor.diagonal(Array(Complex.one, Complex.one, Complex.one, Complex.zero.-(Complex.one)))),
    Block(1, 1, " T ", Tensor.diagonal(Array(Complex.one, ei(math.Pi / 4)))),
    Block(1, 1, " H ", Tensor(Array(Array(1, 1), Array(1, -1))).scaled(1.0 / math.sqrt(2))),
    Block(1, 1, " S ", Tensor.diagonal(Array(Complex.one, ei(math.Pi / 2))))
  )


  def Bian2QubitToGraph(block: Block): Graph = {

    // The graph produced must be 0-indexed on inputs and outputs, and of the form /i\d+/ and /o\d+/

    implicit def vname(str: String): VName = VName(str)

    implicit def vnamepair(p: (String, String)): (VName, VName) = VName(p._1) -> VName(p._2)

    implicit def ename(str: String): EName = EName(str)

    val rg = Theory.fromFile("red_green")

    var g = new Graph()

    var eCount = 0

    def join(v0: String, v1: String): Unit = {
      g = g.addEdge(g.edges.fresh, UndirEdge(), vnamepair(v0,v1))
      eCount += 1
    }

    def addVertex(name: String, data: VData): Unit = {
      g = g.addVertex(vname(name), data)
    }

    for (i <- block.inputs.indices) {
      addVertex("i" + i, WireV())
    }
    for (i <- block.outputs.indices) {
      addVertex("o" + i, WireV())
    }
    block.name match {
      case " 1 " =>
        join("i0", "o0")
      case " T " =>
        addVertex("v0", NodeV(data = JsonObject("type" -> "X", "value" -> "pi/4"), theory = rg))
        join("i0", "v0")
        join("v0", "o0")
      case " H " =>
        addVertex("v0", NodeV(data = JsonObject("type" -> "hadamard", "value" -> "0"), theory = rg))
        join("i0", "v0")
        join("v0", "o0")
      case " S " =>
        addVertex("v0", NodeV(data = JsonObject("type" -> "X", "value" -> "pi/2"), theory = rg))
        join("i0", "v0")
        join("v0", "o0")
      case " Zc" =>
        addVertex("v0", NodeV(data = JsonObject("type" -> "X", "value" -> "0"), theory = rg))
        addVertex("v1", NodeV(data = JsonObject("type" -> "Z", "value" -> "0"), theory = rg))
        join("v0", "v1")
        join("i0", "v0")
        join("v0", "o0")
        join("i1", "v1")
        join("v1", "o1")
    }

    g
  }

}
