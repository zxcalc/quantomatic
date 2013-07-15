package quanto.gui


class Transformer(
  var scale: Double = 50.0f,
  var origin: (Double, Double) = (250.0f, 250.0f)
)
{
  def toScreen(pt: (Double,Double)) = (pt._1 * scale + origin._1, origin._2 - pt._2 * scale)
  def fromScreen(pt: (Double,Double)) = ((pt._1 - origin._1) / scale, ((-1 * pt._2) + origin._2) / scale)

  def scaleToScreen(x: Double) = x * scale
  def scaleFromScreen(x: Double) = x / scale
}
