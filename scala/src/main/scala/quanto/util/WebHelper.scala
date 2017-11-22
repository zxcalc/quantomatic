package quanto.util
import java.net.URL
import java.awt.Desktop

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
