package quanto.util

import java.awt.Desktop
import java.net.URL

object WebHelper {

  // From https://stackoverflow.com/questions/10967451/open-a-link-in-browser-with-java-button


  def openWebpage(urlString: String): Unit = {
    try
      Desktop.getDesktop.browse(new URL(urlString).toURI)
    catch {
      case e: Exception =>
        e.printStackTrace()
    }
  }
}
