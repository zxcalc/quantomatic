package quanto.cosy

import quanto.cosy.BlockGenerators.QuickGraph
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

  implicit def toTensor(b: Block) : Tensor = b.tensor
}


case class BlockRow(blocks: List[Block], suggestTensor: Option[Tensor] = None, suggestGraph: Option[Graph] = None) {


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

    val aShifted: Graph = added.verts.foldLeft(added)((g, vn) => g.updateVData(vn)(vd => {
      val currentCoord = added.vdata(vn).coord
      added.vdata(vn).withCoord(currentCoord._1 + shift, currentCoord._2)
    }
    ))

    val renameMap = aShifted.verts.flatMap(vn => (vn.prefix, vn.suffix) match {
      case ("i-", n) => Some(vn -> VName("i-" + (n + startingInputs.size)))
      case ("o-", n) => Some(vn -> VName("o-" + (n + startingOutputs.size)))
      case (a, b) => Some(vn -> VName("bl-" + shift + "-" + a + b))
      case _ => None
    }).toMap

    val aShiftedRename = aShifted.rename(vrn = renameMap, ern = Map(), brn = Map())
    fixed.appendGraph(aShiftedRename.renameAvoiding(fixed), false)
  }
}

case class BlockStack(rows: List[BlockRow],
                      suggestedTensor: Option[Tensor] = None,
                      suggestedGraph: Option[Graph] = None) extends Ordered[BlockStack] {

  require(rows.flatMap(_.blocks).nonEmpty) // require at least one block

  lazy val tensor: Tensor = if (rows.isEmpty) {
    Tensor.id(1)
  } else {
    suggestedTensor match {
      case Some(t) => t
      case None => rows.foldRight(Tensor.id(rows.last.tensor.width))((a, b) => a.tensor o b)
    }
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
  lazy val graph: Graph = {
    suggestedGraph match {
      case Some(g) => g
      case None =>
        val blocks = rows.flatMap(row => row.blocks)
        if (blocks.isEmpty) {
          new Graph()
        } else {
          val gdata = blocks.head.graph.data
          BlockStack.joinRowsInStack(
            rows.reverse.zipWithIndex.foldRight(new Graph(data = gdata))((ri, g) =>
              BlockStack.graphStackUnjoined(g, ri._1.graph, ri._2)))

        }
    }

  }

  // newly added row is on top!
  // And our graphs go bottom to top
  // Forces computation of tensor and graph (as that's what this is designed for)
  def append(row: BlockRow): BlockStack = {
    //TODO: Make this parallel?
    val newTensor = if (rows.nonEmpty) {
      require(rows.head.outputs == row.inputs)
      row.tensor o tensor
    } else {
      row.tensor
    }
    val newRows = row :: rows
    val newGraph = BlockStack.joinRowsInStack(BlockStack.graphStackUnjoined(graph, row.graph, rows.length))
    new BlockStack(newRows, Some(newTensor), Some(newGraph))
  }

}

object BlockStack {
  def fromJson(js: JsonObject): BlockStack = {
    new BlockStack((js / "rows").asArray.map(j => BlockRow.fromJson(j.asObject)).toList)
  }


  def joinRowsInStack(graph: Graph): Graph = {
    var g = QuickGraph(graph)
    val InputPattern = raw"r-(\d+)-i-(\d+)".r
    g.verts.foreach(vName => vName.s match {
      case InputPattern(n, m) =>
        // For 0.toString is coming out as "" not "0", but this shouldn't affect us
        if (g.verts.contains(s"r-${Integer.parseInt(n) - 1}-o-$m")) {
          g = g.joinIfNotAlready(s"r-${Integer.parseInt(n) - 1}-o-$m", s"r-$n-i-$m", Some("rail"))
        }
      case _ => g
    }
    )
    g
  }

  def graphStackUnjoined(fixed: Graph, adding: Graph, depth: Int): Graph = {
    val renameMap = adding.verts.map(vn => vn -> VName(s"r-$depth-${vn.s}")).toMap
    val aRenamed = adding.rename(vrn = renameMap, ern = Map(), brn = Map())
    val aRenamedShifted = aRenamed.verts.foldLeft(aRenamed)((g, vn) => g.updateVData(vn)(vd => {
      val currentCoord = aRenamed.vdata(vn).coord
      aRenamed.vdata(vn).withCoord(currentCoord._1, currentCoord._2 + depth)
    }
    ))

    if(fixed.data.theory != adding.data.theory){
      fixed.copy(data = fixed.data.copy(theory = fixed.data.theory.mixin(adding.data.theory, None)))
        .appendGraph(aRenamedShifted.renameAvoiding(fixed), noOverlap = false)
    }else{
      fixed
        .appendGraph(aRenamedShifted.renameAvoiding(fixed), noOverlap = false)
    }
  }
}

object BlockRowMaker {


  def predicateStackToGraph(stack: BlockStack, blockToGraph: Block => Graph): Graph = {
    var g = (for ((row, index) <- stack.rows.zipWithIndex) yield {
      val rowGraph = predicateRowToGraph(row, blockToGraph)
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

  def predicateRowToGraph(row: BlockRow, blockToGraph: Block => Graph): Graph = {
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

  def apply(maxBlocks: Int, allowedBlocks: List[Block], maxInOut: Option[Int] = None): List[BlockRow] = {
    require(maxBlocks >= 0)
    makeRowsUpToSize(maxBlocks, allowedBlocks, maxInOut)
  }

  def makeRowsUpToSize(size: Int,
                       allowedBlocks: List[Block],
                       maxInOut: Option[Int] = None): List[BlockRow] = {
    val maybeTooLargeRows: List[BlockRow] = size match {
      case 0 => List[BlockRow]()
      case 1 => allowedBlocks.map(b => new BlockRow(List(b)))
      case n => {
        val fewerBlocks = makeRowsUpToSize(n - 1, allowedBlocks, maxInOut)

        (for (base <- fewerBlocks; block <- allowedBlocks) yield {
          new BlockRow(block :: base.blocks)
        }) ::: fewerBlocks
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