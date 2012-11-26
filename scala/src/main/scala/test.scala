import quanto.core._
import swing._

object Test extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Test"
    contents = new Button {
      text = "Click Me"
    }
  }
}