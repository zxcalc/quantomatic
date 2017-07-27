package quanto.cosy

import quanto.util.json._
import quanto.data._

/**
  * Enumerates diagrams by composing simple building blocks in a 2D fashion
  */
class BlockEnumeration {

}

case class Block(inputs: Int, outputs: Int, name: String, tensor: Tensor, parameter: String = "") {
  lazy val toJson = JsonObject(
    "inputs" -> inputs,
    "outputs" -> outputs,
    "name" -> name,
    "tensor" -> tensor.toJson,
    "parameter" -> parameter
  )

  override def toString: String = this.name
}

object Block {
  def fromJson(js: JsonObject): Block = {
    new Block((js / "inputs").intValue,
      (js / "outputs").intValue,
      (js / "name").stringValue,
      Tensor.fromJson((js / "tensor").asObject),
      (js / "parameter").stringValue)
  }
}

class BlockRow(val blocks: List[Block], suggestTensor: Tensor = Tensor.zero(1, 1)) {
  lazy val inputs: Int = blocks.foldLeft(0)((a, b) => a + b.inputs)
  lazy val outputs: Int = blocks.foldLeft(0)((a, b) => a + b.outputs)
  lazy val tensor: Tensor =
    if (suggestTensor == Tensor.zero(1, 1)) {
      blocks.foldLeft(Tensor.id(1))((a, b) => a x b.tensor)
    } else {
      suggestTensor
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
}

class BlockStack(val rows: List[BlockRow]) extends Ordered[BlockStack] {
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
  override val toString: String = rows.mkString("(", ") o (", ")")

  def inputs: Int = if (rows.nonEmpty) rows.last.inputs else 0

  def outputs: Int = if (rows.nonEmpty) rows.head.outputs else 0

  def compare(that: BlockStack): Int = {
    this.rows.length - that.asInstanceOf[BlockStack].rows.length
  }

}

object BlockStack {
  def fromJson(js: JsonObject): BlockStack = {
    new BlockStack((js / "rows").asArray.map(j => BlockRow.fromJson(j.asObject)).toList)
  }
}

object BlockRowMaker {
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

  val H3: Tensor = Tensor(
    Array(Array[Complex](1, 1, 1),
      Array[Complex](1, ei(2 * math.Pi / 3), ei(4 * math.Pi / 3)),
      Array[Complex](1, ei(4 * math.Pi / 3), ei(2 * math.Pi / 3)))).
    scaled(1.0 / math.sqrt(3))

  def ZXQutrit(numAngles: Int = 9): List[Block] = List(
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
      List(Block(1, 1, "g" + i.toString + "|" + j.toString, gs),
        Block(1, 1, "r" + i.toString + "|" + j.toString, H3.dagger o gs o H3))
    }).flatten.toList


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

  def Bian2Qubit: List[Block] = List(
    // Block(0, 0, " w ", Tensor.id(1).scaled(ei(math.Pi / 4))), Ignored for now.
    Block(1, 1, " 1 ", Tensor.id(2)),
    Block(2, 2, " Zc", Tensor.diagonal(Array(Complex.one, Complex.one, Complex.one, Complex.zero.-(Complex.one)))),
    Block(1, 1, " T ", Tensor.diagonal(Array(Complex.one, ei(math.Pi / 4)))),
    Block(1, 1, " H ", Tensor(Array(Array(1, 1), Array(1, -1))).scaled(1.0 / math.sqrt(2))),
    Block(1, 1, " S ", Tensor.diagonal(Array(Complex.one, ei(math.Pi / 2))))
  )

  private def ei(angle: Double) = Complex(math.cos(angle), math.sin(angle))

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
      for (j <- 0 until row.outputs) {
        g = g.addEdge(EName("r" + index + "e" + j), UndirEdge(), VName("r" + index + "o" + j) -> VName("r" + (index + 1) + "i" + j))
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
      inputsCovered += block.inputs
      outputsCovered += block.outputs
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
      g = g.addEdge("e" + eCount, UndirEdge(), v0 -> v1)
      eCount += 1
    }

    def addVertex(name: String, data: VData): Unit = {
      g = g.addVertex(name, data)
    }

    for (i <- 0 until block.inputs) {
      addVertex("i" + i, WireV())
    }
    for (i <- 0 until block.outputs) {
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

  def apply(maxBlocks: Int, maxInOut: Int = -1, allowedBlocks: List[Block]): List[BlockRow] = {
    var builtRows: List[BlockRow] = allowedBlocks.map(b => new BlockRow(List(b))).filter(
      r => maxInOut == -1 || (r.inputs <= maxInOut && r.outputs <= maxInOut)
    )
    var nextRows: List[BlockRow] = List()
    for (i <- 1 until maxBlocks) {
      for (row <- builtRows) {
        for (block <- allowedBlocks) {
          val newRow = new BlockRow(block :: row.blocks, block.tensor x row.tensor)
          if (maxInOut == -1 || (newRow.inputs <= maxInOut && newRow.outputs <= maxInOut)) nextRows = newRow :: nextRows
        }
      }
      builtRows = builtRows ::: nextRows
      nextRows = List()
    }
    builtRows
  }
}

object BlockStackMaker {
  var allowedRows: List[BlockRow] = List()


  def apply(maxRows: Int, allowedRows: List[BlockRow] = allowedRows): List[BlockStack] = {
    var builtStacks: List[BlockStack] = allowedRows.map(r => new BlockStack(List(r)))
    var nextStacks: List[BlockStack] = List()
    for (i <- 1 until maxRows) {
      for (stack <- builtStacks) {
        for (row <- allowedRows) {
          if (stack.rows.isEmpty || stack.outputs == row.inputs) {
            if (row.blocks.nonEmpty) nextStacks = new BlockStack(row :: stack.rows) :: nextStacks
          }
        }
      }
      builtStacks = builtStacks ::: nextStacks
      nextStacks = List()
    }
    builtStacks
  }
}