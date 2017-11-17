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


  case class UIRedrawRequest() extends Event

  object OptionsChanged extends Publisher

  private def requestUIRefresh(): Unit = {
    OptionsChanged.publish(UIRedrawRequest())
  }

  // Changes the default font but doesn't request redraw
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

  // A scaling of 1 corresponds to a font size of 14
  // changing the font size will also change the rest of the scaling
  private var _uiScale : Double = 1
  def uiScale : Double = _uiScale
  def uiScale_=(d: Double){
    _uiScale = math.max(d, 0.5) // Limit scaling to equivalent of 7pt font
    setUIFont(new FontUIResource(_fontFamily, _fontDecoration, fontSize))
    val prefs = Preferences.userRoot().node(this.getClass.getName)
    prefs.putDouble("uiScale", _uiScale)
    requestUIRefresh()
  }

  def scale(d: Double) : Double = {
    d * uiScale
  }

  def scaleInt(d: Double) : Int = math.floor(scale(d)).toInt

  def resetScale(): Unit ={
    uiScale = 1
  }

  def font : Font = {
    new Font(_fontFamily, _fontDecoration, fontSize)
  }

  private var _fontDecoration = Font.PLAIN
  def fontDecoration : Int = _fontDecoration
  def fontDecoration_=(n: Int){
    _fontDecoration = n
  }

  private var _fontFamily = "defaultFont"
  def fontFamily : String = _fontFamily
  def fontFamily_=(s: String): Unit ={
    _fontFamily = s
  }

  def fontSize : Int = {
    Math.floor(14*_uiScale).toInt
  }
  def fontSize_=(n: Int){
    uiScale = n / 14.0 //changing uiScale triggers redraw and font size changes
  }

  private var _graphScale : Double = 1
  def graphScale : Double = _graphScale
  def graphScale_=(n: Double): Unit ={
    _graphScale = n
  }

  private var _preferredTimeFormat : SimpleDateFormat = new SimpleDateFormat("HH:mm:ss")
  def preferredTimeFormat : SimpleDateFormat = _preferredTimeFormat
  def preferredTimeFormat_=(format: String): Unit = {
    try {
      _preferredTimeFormat = new SimpleDateFormat(format)
    }
      catch {
        case e: Exception =>
          e.printStackTrace()
      }
  }

}
