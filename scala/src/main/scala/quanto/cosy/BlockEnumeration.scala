package quanto.cosy

import quanto.cosy.BlockRowMaker.QuickGraph
import quanto.data.Names._
import quanto.data.{VName, _}
import quanto.util.json._

/**
  * Enumerates diagrams by composing simple building blocks in a 2D fashion
  */
class BlockEnumeration {

}

case class Block(inputs: List[Int], outputs: List[Int], name: String, tensor: Tensor, graph: Graph = new Graph()) {
  lazy val toJson = JsonObject(
    "inputs" -> inputs,
    "outputs" -> outputs,
    "name" -> name,
    "tensor" -> tensor.toJson,
    "graph" -> graph.toJson()
  )

  override def toString: String = this.name
}

object Block {
  def fromJson(js: JsonObject): Block = {
    new Block((js / "inputs").asArray.map(x => x.intValue).toList,
      (js / "outputs").asArray.map(x => x.intValue).toList,
      (js / "name").stringValue,
      Tensor.fromJson((js / "tensor").asObject),
      Graph.fromJson((js / "graph").asObject))
  }
}


case class BlockRow(blocks: List[Block], suggestTensor: Option[Tensor] = None, suggestGraph : Option[Graph] = None) {


  implicit def optionTensor(t: Tensor): Option[Tensor] = {
    Option(t)
  }

  lazy val inputs: List[Int] = blocks.foldRight(List[Int]())((a, b) => a.inputs ::: b)
  lazy val outputs: List[Int] = blocks.foldRight(List[Int]())((a, b) => a.outputs ::: b)
  lazy val tensor: Tensor = suggestTensor match {
    case Some(t) => t
    case None => blocks.foldLeft(Tensor.id(1))((a, b) => a x b.tensor)
  }

  lazy val graph: Graph = suggestGraph match {
    case Some(g) => g
    case None => blocks.foldLeft(new Graph()) { (g, b) =>
      BlockRow.graphsSideBySide(g, b.graph)
    }
  }

  lazy val toJson = JsonObject(
    "blocks" -> JsonArray(blocks.map(b => b.toJson)),
    "inputs" -> inputs,
    "outputs" -> outputs,
    "tensor" -> tensor.toJson,
    "string" -> toString
  )
  override val toString: String = blocks.mkString(" x ")
}

object BlockRow {
  def fromJson(js: JsonObject): BlockRow = {
    new BlockRow((js / "blocks").asArray.map(j => Block.fromJson(j.asObject)).toList)
  }



  def graphsSideBySide(fixed: Graph, added: Graph): Graph = {


    val startingInputs = fixed.verts.filter(vn => vn.prefix == "i-")
    val startingOutputs = fixed.verts.filter(vn => vn.prefix == "o-")
    val shift = math.max(startingInputs.size, startingOutputs.size)

    val aShifted: Graph = added.verts.foldLeft(added)((g, vn) =>  g.updateVData(vn)(vd => {
      val currentCoord = added.vdata(vn).coord
      added.vdata(vn).withCoord(currentCoord._1 + shift, currentCoord._2)
    }
    ))

    val renameMap = aShifted.verts.flatMap(vn => (vn.prefix, vn.suffix) match {
      case ("i-", n) => Some(vn -> VName("i-" + (n+startingInputs.size)))
      case ("o-", n) => Some(vn -> VName("o-" + (n+startingOutputs.size)))
      case (a, b) => Some(vn -> VName("bl-"+shift + "-" + a + b ))
      case _ => None
    }).toMap

    val aShiftedRename = aShifted.rename(vrn = renameMap, ern = Map(), brn = Map())
    fixed.appendGraph(aShiftedRename.renameAvoiding(fixed), false)
  }
}

case class BlockStack(rows: List[BlockRow], suggestedGraph : Option[Graph] = None) extends Ordered[BlockStack] {
  lazy val tensor: Tensor = if (rows.isEmpty) {
    Tensor.id(1)
  } else {
    rows.foldRight(Tensor.id(rows.last.tensor.width))((a, b) => a.tensor o b)
  }
  lazy val toJson = JsonObject(
    "rows" -> JsonArray(rows.map(b => b.toJson)),
    "inputs" -> inputs,
    "outputs" -> outputs,
    "tensor" -> tensor.toJson,
    "string" -> toString
  )
  lazy val inputs: List[Int] = if (rows.nonEmpty) rows.last.inputs else List()
  lazy val outputs: List[Int] = if (rows.nonEmpty) rows.head.outputs else List()
  override val toString: String = rows.mkString("(", ") o (", ")")

  def compare(that: BlockStack): Int = {
    this.rows.length - that.asInstanceOf[BlockStack].rows.length
  }


    //left-most row is on top!
  lazy val graph : Graph = {
    suggestedGraph match {
      case Some(g) => g
      case None => BlockStack.joinRowsInStack(
        rows.reverse.zipWithIndex.foldRight(new Graph())((ri, g) => BlockStack.graphStackUnjoined(g, ri._1.graph, ri._2)))
    }

  }

  //left-most row is on top!
  def append(row: BlockRow) : BlockStack = {
    if(rows.nonEmpty){
      require(rows.head.outputs == row.inputs)
    }
    val newRows = row :: rows
    val newGraph = BlockStack.joinRowsInStack(BlockStack.graphStackUnjoined(graph, row.graph, rows.length))
    new BlockStack(newRows, Some(newGraph))
  }

}

object BlockStack {
  def fromJson(js: JsonObject): BlockStack = {
    new BlockStack((js / "rows").asArray.map(j => BlockRow.fromJson(j.asObject)).toList)
  }


  def joinRowsInStack(graph: Graph) : Graph = {
    var g = QuickGraph(graph)
    val InputPattern = raw"r-(\d+)-i-(\d+)".r
    g.verts.foreach(vName => vName.s match {
      case InputPattern(n, m) =>
        // For 0.toString is coming out as "" not "0", but this shouldn't affect us
        if (g.verts.contains(s"r-${Integer.parseInt(n)-1}-o-$m")) {
        g = g.joinIfNotAlready(s"r-${Integer.parseInt(n)-1}-o-$m", s"r-$n-i-$m")
      }
      case _ => g
    }
    )
    g
  }

  def graphStackUnjoined(fixed : Graph, adding: Graph, depth : Int): Graph = {
    val renameMap = adding.verts.map(vn =>  vn -> VName(s"r-$depth-${vn.s}")).toMap
    val aRenamed = adding.rename(vrn = renameMap, ern = Map(), brn = Map())
    val aRenamedShifted = aRenamed.verts.foldLeft(aRenamed)((g, vn) => g.updateVData(vn)(vd => {
      val currentCoord = aRenamed.vdata(vn).coord
      aRenamed.vdata(vn).withCoord(currentCoord._1, currentCoord._2 + depth)
    }
    ))

  fixed.appendGraph(aRenamedShifted.renameAvoiding(fixed), noOverlap = false)
  }
}

object BlockRowMaker {

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
    def apply(graph: Graph = new Graph()) = new QuickGraph(graph)

    implicit def slow(qg: QuickGraph) : Graph = qg()
  }


  val ZXClifford: List[Block] = List(
    Block(1, 1, " 1 ", Tensor.idWires(1), QuickGraph().addInput().addOutput().join("i-0", "o-0")),
    Block(2, 2, " s ", Tensor.swap(List(1, 0)),
      QuickGraph().addInput(2).addOutput(2).join("i-0", "o-1").join("i-1", "o-0")),
    Block(1, 1, " H ", Hadamard(2), QuickGraph().addInput().addOutput()
      .node("hadamard", nodeName = "h").join("i-0", "h").join("h", "o-0")),
    Block(2, 2, "CNT", Tensor(Array(Array(1, 0, 0, 0), Array(0, 1, 0, 0), Array(0, 0, 0, 1), Array(0, 0, 1, 0))),
      QuickGraph().addInput(2).addOutput(2).node("Z", nodeName = "z").node("X", xCoord = 1, nodeName = "x")
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

  def stackToGraph(stack: BlockStack, blockToGraph: Block => Graph): Graph = {
    var g = (for ((row, index) <- stack.rows.zipWithIndex) yield {
      val rowGraph = rowToGraph(row, blockToGraph)
      val verts = rowGraph.verts.toList
      val vRename = verts.map(v => v -> VName("r" + index + v.s)).toMap
      val eRename = rowGraph.edges.map(e => e -> EName("r" + index + e.s)).toMap
      val bRename = rowGraph.bboxes.map(b => b -> BBName("r" + index + b.s)).toMap
      rowGraph.rename(vRename, eRename, bRename)
    }).foldLeft(new Graph())((g, sg) => g.appendGraph(sg))
    for ((row, index) <- stack.rows.init.zipWithIndex) {
      for (j <- row.outputs.indices) {
        g = g.addEdge(
          EName("r" + index + "e" + j),
          UndirEdge(),
          VName("r" + index + "o" + j) -> VName("r" + (index + 1) + "i" + j)
        )
      }
    }
    g
  }

  def rowToGraph(row: BlockRow, blockToGraph: Block => Graph): Graph = {
    var inputsCovered = 0
    var outputsCovered = 0
    val inputRegex = raw"i(\d+)".r
    val outputRegex = raw"o(\d+)".r
    (for ((block, index) <- row.blocks.zipWithIndex) yield {
      val blockGraph = blockToGraph(block)
      val verts = blockGraph.verts.toList
      val vRename = verts.map(v => v -> (v.s match {
        case inputRegex(num) => VName("i" + (inputsCovered + num.toInt))
        case outputRegex(num) => VName("o" + (outputsCovered + num.toInt))
        case _ => VName("b" + index + v.s)
      })).toMap
      val eRename = blockGraph.edges.map(e => e -> EName("b" + index + e.s)).toMap
      val bRename = blockGraph.bboxes.map(b => b -> BBName("b" + index + b.s)).toMap
      inputsCovered += block.inputs.length
      outputsCovered += block.outputs.length
      blockGraph.rename(vRename, eRename, bRename)
    }).foldLeft(new Graph())((g, bg) => g.appendGraph(bg))
  }


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

  def apply(maxBlocks: Int, allowedBlocks: List[Block], maxInOut: Option[Int] = None): List[BlockRow] = {
    require(maxBlocks >= 0)
    (for (i <- 0 to maxBlocks) yield {
      makeRowsOfSize(i, allowedBlocks, maxInOut)
    }).flatten.toList
  }

  def makeRowsOfSize(size: Int,
                     allowedBlocks: List[Block],
                     maxInOut: Option[Int] = None): List[BlockRow] = {
    val maybeTooLargeRows: List[BlockRow] = size match {
      case 0 => List[BlockRow]()
      case 1 => allowedBlocks.map(b => new BlockRow(List(b)))
      case n => for (base <- makeRowsOfSize(n - 1, allowedBlocks, maxInOut); block <- allowedBlocks) yield {
        new BlockRow(block :: base.blocks)
      }
    }
    maxInOut match {
      case None => maybeTooLargeRows
      case Some(max) => maybeTooLargeRows.filter(r => (r.inputs.length <= max) && (r.outputs.length <= max))
    }
  }
}

object BlockStackMaker {

  def apply(maxRows: Int,
            allowedRows: List[BlockRow]): List[BlockStack] = {
    require(maxRows >= 0)
    (for (i <- 0 to maxRows) yield {
      makeStacksOfSize(i, allowedRows)
    }).flatten.toList
  }

  def makeStacksOfSize(maxRows: Int,
                       allowedRows: List[BlockRow]): List[BlockStack] = {
    require(maxRows >= 0)
    val nonEmptyRows = allowedRows.filter(r => r.blocks.nonEmpty)
    maxRows match {
      case 0 => List[BlockStack]()
      case 1 => nonEmptyRows.map(row => new BlockStack(List(row)))
      case n => for (
        base <- makeStacksOfSize(n - 1, nonEmptyRows);
        row <- nonEmptyRows.filter(r => r.blocks.nonEmpty && r.inputs == base.outputs)
      ) yield {
        new BlockStack(row :: base.rows)
      }
    }
  }
}