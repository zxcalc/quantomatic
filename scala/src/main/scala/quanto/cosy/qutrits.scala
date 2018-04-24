package quanto.cosy

object qutrits {

  val w: Complex = ei(2 * math.Pi / 3)
  val H_unnormalised = Tensor(Array(Array[Complex](1, 1, 1), Array[Complex](1, w, w * w), Array[Complex](1, w * w, w)))
  val H  : Tensor= H_unnormalised.scaled(1.0 / math.sqrt(3))
  val e0 : Tensor = Tensor(Array(Array(1, 0, 0))).transpose
  val e1 : Tensor = Tensor(Array(Array(0, 1, 0))).transpose
  val e2 : Tensor = Tensor(Array(Array(0, 0, 1))).transpose
  val f0 : Tensor = Tensor(Array(Array(1, 1, 1))).transpose.scaled(1.0 / math.sqrt(3))
  val f1 : Tensor = Tensor(Array(Array(Complex.one, w, w * w))).transpose.scaled(1.0 / math.sqrt(3))
  val f2 : Tensor = Tensor(Array(Array(Complex.one, w * w, w))).transpose.scaled(1.0 / math.sqrt(3))

  def ei(arg: Double) = Complex(math.cos(arg), math.sin(arg))

  def conj(T: Tensor) = Tensor(T.height, T.width, (i, j) => T(j, i).conjugate)
}

