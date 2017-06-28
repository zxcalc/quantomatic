package quanto.cosy

import quanto.util.json.{JsonArray, JsonObject}

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

class BlockStack(val rows: List[BlockRow]) {
  lazy val tensor: Tensor = if (rows.isEmpty) {
    Tensor.id(1)
  } else {

    rows.foldRight(Tensor.idWires(inputs))((a, b) => a.tensor o b)
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
    Block(2, 2, "crs", new Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 1, 0), Array(0, 1, 0, 0), Array(0, 0, 0, -1)))),
    Block(0, 2, "cup", new Tensor(Array(Array(1, 0, 0, 1))).transpose),
    Block(2, 0, "cap", new Tensor(Array(Array(1, 0, 0, 1)))),
    Block(1, 1, " w ", new Tensor(Array(Array(1, 0), Array(0, -1)))),
    Block(1, 2, "1w2", new Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 0, -1))).transpose),
    Block(2, 1, "2w1", new Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 0, -1)))),
    Block(1, 1, " b ", new Tensor(Array(Array(0, 1), Array(1, 0)))),
    Block(1, 2, "1b2", new Tensor(Array(Array(0, 1, 1, 0), Array(1, 0, 0, 1))).transpose),
    Block(2, 1, "2b1", new Tensor(Array(Array(0, 1, 1, 0), Array(1, 0, 0, 1))))
  )
  val ZX: List[Block] = List(
    Block(1, 1, " 1 ", Tensor.idWires(1)),
    Block(2, 2, " s ", Tensor.swap(List(1, 0))),
    Block(0, 2, "cup", new Tensor(Array(Array(1, 0, 0, 1))).transpose),
    Block(2, 0, "cap", new Tensor(Array(Array(1, 0, 0, 1)))),
    Block(1, 1, "gT ", new Tensor(Array(Array(1, 0), Array(0, ei(math.Pi / 4))))),
    Block(1, 1, " H ", new Tensor(Array(Array(1, 1), Array(1, -1))).scaled(1.0 / math.sqrt(2))),
    Block(1, 1, "rT ", new Tensor(Array(
      Array(1 + ei(math.Pi / 4), 1 - ei(math.Pi / 4)),
      Array(1 - ei(math.Pi / 4), 1 + ei(math.Pi / 4))))),
    Block(2, 1, "2g1", new Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 0, 1)))),
    Block(1, 2, "1g2", new Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 0, 1))).transpose),
    Block(0, 1, "gu ", new Tensor(Array(Array(1, 1))).transpose),
    Block(1, 2, "1r2", new Tensor(Array(Array(1, 0, 0, 1), Array(0, 1, 1, 0))).transpose),
    Block(2, 1, "2r1", new Tensor(Array(Array(1, 0, 0, 1), Array(0, 1, 1, 0)))),
    Block(0, 1, "ru ", new Tensor(Array(Array(1, 0))).transpose)
  )

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

  private def ei(angle: Double) = Complex(math.cos(angle), math.sin(angle))
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