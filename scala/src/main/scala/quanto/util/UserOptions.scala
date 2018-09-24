package quanto.util

import java.awt.Font
import java.text.SimpleDateFormat
import java.util.prefs.Preferences
import javax.swing.UIManager
import javax.swing.plaf.FontUIResource

import scala.swing.Publisher
import scala.swing.event.Event

class UserOptions {

}

object UserOptions {

  val prefs: Preferences = Preferences.userRoot().node(this.getClass.getName)
  // A scaling of 1 corresponds to a font size of 14
  // changing the font size will also change the rest of the scaling
  private var _uiScale: Double = prefs.getDouble("uiScale", 1)
  private var _fontDecoration = Font.PLAIN
  private var _fontFamily = "defaultFont"
  private var _logging: Boolean = prefs.getBoolean("logging", false)
  private var _graphScale: Double = 1
  private var _preferredTimeFormat: SimpleDateFormat = new SimpleDateFormat("HH:mm:ss")
  private var _preferredDateTimeFormat: SimpleDateFormat = new SimpleDateFormat("yy-MM-dd.HH:mm:ss")

  def scaleInt(d: Double): Int = math.floor(scale(d)).toInt

  def scale(d: Double): Double = {
    d * uiScale
  }

  def resetScale(): Unit = {
    uiScale = 1
  }

  def uiScale: Double = _uiScale

  def uiScale_=(d: Double) {
    _uiScale = d
    _uiScale = math.max(_uiScale, 0.5) // Limit scaling to equivalent of 7pt font
    _uiScale = math.min(_uiScale, 4) // Limit scaling to equivalent of 56pt font
    setUIFont(new FontUIResource(_fontFamily, _fontDecoration, fontSize))
    prefs.putDouble("uiScale", _uiScale)
    requestUIRefresh()
  }

  private def requestUIRefresh(): Unit = {
    OptionsChanged.publish(UIRedrawRequest())
  }

  private def setUIFont(f: FontUIResource): Unit = {
    val keys = UIManager.getDefaults.keys
    while ( {
      keys.hasMoreElements
    }) {
      val key = keys.nextElement
      val value = UIManager.get(key)
      if (value.isInstanceOf[FontUIResource]) UIManager.put(key, f)
    }
  }

  def font: Font = {
    new Font(_fontFamily, _fontDecoration, fontSize)
  }

  def fontSize: Int = {
    Math.floor(14 * _uiScale).toInt
  }

  def fontSize_=(n: Int) {
    uiScale = n / 14.0 //changing uiScale triggers redraw and font size changes
  }

  def fontDecoration: Int = _fontDecoration

  def fontDecoration_=(n: Int) {
    _fontDecoration = n
  }

  def fontFamily: String = _fontFamily

  def fontFamily_=(s: String): Unit = {
    _fontFamily = s
  }

  def logging: Boolean = _logging

  def logging_=(b: Boolean): Unit = {
    prefs.putBoolean("logging", b)
    _logging = b
  }

  def graphScale: Double = _graphScale

  def graphScale_=(n: Double): Unit = {
    _graphScale = n
  }

  def preferredTimeFormat: SimpleDateFormat = _preferredTimeFormat

  def preferredTimeFormat_=(format: String): Unit = {
    try {
      _preferredTimeFormat = new SimpleDateFormat(format)
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
    }
  }

  def preferredDateTimeFormat: SimpleDateFormat = _preferredDateTimeFormat

  def preferredDateTimeFormat_=(format: String): Unit = {
    try {
      _preferredDateTimeFormat = new SimpleDateFormat(format)
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
    }
  }

  case class UIRedrawRequest() extends Event

  object OptionsChanged extends Publisher


}
