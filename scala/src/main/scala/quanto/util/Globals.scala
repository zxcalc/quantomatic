package quanto.util

import java.awt.event.InputEvent
import java.io.File

object Globals {
  def CommandDownMask: Int = if (CommandMask == InputEvent.META_MASK) InputEvent.META_DOWN_MASK
  else InputEvent.CTRL_DOWN_MASK

  def CommandMask: Int = java.awt.Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  def isBundle: Boolean = isMacBundle || isWindowsBundle || isLinuxBundle

  def isMacBundle: Boolean = new File("osx-bundle").exists

  def isLinuxBundle: Boolean = new File("linux-bundle").exists

  def isWindowsBundle: Boolean = new File("windows-bundle").exists
}
