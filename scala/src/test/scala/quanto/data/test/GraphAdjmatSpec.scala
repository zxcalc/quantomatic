package quanto.data.test

import org.scalatest._
import quanto.cosy.AdjMat
import quanto.data._
import quanto.util.json._


class GraphAdjmatSpec extends FlatSpec {
  val rg = Theory.fromFile("red_green")

  behavior of "A graph from adjacency matrix"

  val gdata = Vector(
    NodeV(data = JsonObject("type" -> "Z", "value" -> "0"), theory = rg),
    NodeV(data = JsonObject("type" -> "Z", "value" -> "pi"), theory = rg),
    NodeV(data = JsonObject("type" -> "Z", "value" -> "(1/2) pi"), theory = rg),
    NodeV(data = JsonObject("type" -> "Z", "value" -> "(-1/2) pi"), theory = rg)
  )

  val rdata = Vector(
    NodeV(data = JsonObject("type" -> "X", "value" -> "0"), theory = rg),
    NodeV(data = JsonObject("type" -> "X", "value" -> "pi"), theory = rg),
    NodeV(data = JsonObject("type" -> "X", "value" -> "(1/2) pi"), theory = rg),
    NodeV(data = JsonObject("type" -> "X", "value" -> "(-1/2) pi"), theory = rg)
  )

  it should "be constructible" in {
    val amat = AdjMat(numRedTypes = 2, numGreenTypes = 2, numBoundaries = 2, mat = Vector(
      Vector(true,false,true,true)))
  }
}
