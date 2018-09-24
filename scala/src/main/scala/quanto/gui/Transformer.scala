package quanto.gui


class Transformer(
  var scale: Double = 50.0f,
  var screenDrawOrigin: (Double, Double) = (250f, 250f)
)
{
  // screenDrawOrigin is where _on the screen_ _in pixels_ we want the origin to be drawn
  def toScreen(pt: (Double,Double)) = (pt._1 * scale + screenDrawOrigin._1, screenDrawOrigin._2 - pt._2 * scale)
  def fromScreen(pt: (Double,Double)) = ((pt._1 - screenDrawOrigin._1) / scale, ((-1 * pt._2) + screenDrawOrigin._2) / scale)

  def scaleToScreen(x: Double) = x * scale
  def scaleFromScreen(x: Double) = x / scale
}
