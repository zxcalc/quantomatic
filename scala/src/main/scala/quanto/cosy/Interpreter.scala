package quanto.cosy

/**
  * An interpreter is given a diagram (as an adjMat and variable assignment) and returns a tensor
  */

object Interpreter {

  def makeHadamards(n: Int, current: Tensor = Tensor.id(1)): Tensor = n match {
    case 0 => Tensor.id(1)
    case _ => Tensor.hadamard x makeHadamards(n - 1, current)
  }

  type cachedSpiders = collection.mutable.Map[String, Tensor]
  val cached: cachedSpiders = collection.mutable.Map.empty[String, Tensor]

  def interpretSpider(green: Boolean, angle: Double, inputs: Int, outputs: Int): Tensor = {
    val toString = green.toString + ":" + angle + ":" + inputs + ":" + outputs
    if (cached.contains(toString)) cached(toString) else {
      def gen(i: Int, j: Int): Complex =
        if (i == 0 && j == 0) Complex.one
        else if (i == math.pow(2, outputs) - 1 && j == math.pow(2, inputs) - 1) Complex(math.cos(angle), math.sin(angle))
        else Complex.zero

      val mid = Tensor(math.pow(2, outputs).toInt, math.pow(2, inputs).toInt, gen)
      val spider = if (green) mid else makeHadamards(outputs) o mid o makeHadamards(inputs)
      cached += (toString -> spider)
      spider
    }
  }
}
