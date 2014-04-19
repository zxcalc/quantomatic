package quanto.util

import java.io.File

object Globals {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
  def isMacApp: Boolean = new File("../Resources").exists
}
