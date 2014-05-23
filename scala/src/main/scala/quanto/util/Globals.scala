package quanto.util

import java.io.File
import java.awt.event.InputEvent

object Globals {
  def CommandMask = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask
  def CommandDownMask = if (CommandMask == InputEvent.META_MASK) InputEvent.META_DOWN_MASK
                        else InputEvent.CTRL_DOWN_MASK
  def isMacBundle: Boolean = new File("osx-bundle").exists
  def isLinuxBundle: Boolean = new File("linux-bundle").exists
  def isWindowsBundle: Boolean = new File("windows-bundle").exists
  def isBundle: Boolean = isMacBundle || isWindowsBundle || isLinuxBundle
}
