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

class BlockRow(val blocks: List[Block]) {
  lazy val inputs: Int = blocks.foldLeft(0)((a, b) => a + b.inputs)
  lazy val outputs: Int = blocks.foldLeft(0)((a, b) => a + b.outputs)
  lazy val tensor: Tensor = blocks.foldLeft(Tensor.id(1))((a, b) => a x b.tensor)
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

    rows.foldLeft(Tensor.idWires(inputs))((a, b) => a o b.tensor)
  }
  lazy val toJson = JsonObject(
    "rows" -> JsonArray(rows.map(b => b.toJson)),
    "inputs" -> inputs,
    "outputs" -> outputs,
    "tensor" -> tensor.toJson,
    "string" -> toString
  )
  override val toString: String = rows.mkString("(", ") o (", ")")

  def inputs: Int = if (rows.nonEmpty) rows.reverse.head.inputs else 0

  def outputs: Int = if (rows.nonEmpty) rows.head.outputs else 0
}

object BlockStack {
  def fromJson(js: JsonObject): BlockStack = {
    new BlockStack((js / "rows").asArray.map(j => BlockRow.fromJson(j.asObject)).toList)
  }
}

object BlockRowMaker {
  val ZW: List[Block] = List(
    Block(1, 1, " 1 ", Tensor.idWires(1)),
    Block(2, 2, " s ", Tensor.swap(List(1, 0))),
    Block(2, 2, "crs", new Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 1, 0), Array(0, 1, 0, 0), Array(0, 0, 0, -1)))),
    Block(0, 2, "cup", new Tensor(Array(Array(1, 0, 0, 1)))),
    Block(2, 0, "cap", new Tensor(Array(Array(1, 0, 0, 1))).transpose),
    Block(1, 1, " w ", new Tensor(Array(Array(1, 0), Array(0, -1)))),
    Block(1, 2, "1w2", new Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 0, -1))).transpose),
    Block(2, 1, "2w1", new Tensor(Array(Array(1, 0, 0, 0), Array(0, 0, 0, -1)))),
    Block(1, 1, " b ", new Tensor(Array(Array(0, 1), Array(1, 0)))),
    Block(1, 2, "1b2", new Tensor(Array(Array(0, 1, 1, 0), Array(1, 0, 0, 1))).transpose),
    Block(2, 1, "2b1", new Tensor(Array(Array(0, 1, 1, 0), Array(1, 0, 0, 1))))
  )
  var allowedBlocks: List[Block] = List()

  def apply(maxBlocks: Int, allowedBlocks: List[Block] = allowedBlocks): List[BlockRow] = {
    var builtRows: List[BlockRow] = allowedBlocks.map(b => new BlockRow(List(b)))
    var nextRows: List[BlockRow] = List()
    for (i <- 1 until maxBlocks) {
      for (row <- builtRows) {
        for (block <- allowedBlocks) {
          nextRows = new BlockRow(block :: row.blocks) :: nextRows
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