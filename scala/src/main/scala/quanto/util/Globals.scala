package quanto.util

import java.io.File

object Globals {
  val CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
  def isMacBundle: Boolean = new File("mac-bundle").exists
  def isLinuxBundle: Boolean = new File("linux-bundle").exists
}
