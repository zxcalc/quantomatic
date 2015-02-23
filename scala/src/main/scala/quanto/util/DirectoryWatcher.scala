package quanto.util

import java.io._

sealed abstract class FileTree
case class DirNode(name: String, files: Set[FileTree]) extends FileTree
case class FileNode(name: String) extends FileTree

class DirectoryWatcher(val dir: String, onChange: FileTree => Any) extends Thread {
  var fileTree: FileTree = FileNode("")
  var poll = true
  private def buildFileTree(f: File): FileTree = {
    if (f.isDirectory) DirNode(f.getName, f.listFiles.toSet.map(buildFileTree))
    else FileNode(f.getName)
  }

  override def run() {
    while(poll) {
      val ft = buildFileTree(new File(dir))
      if (ft != fileTree) {
        fileTree = ft

        // re-check "poll", in case I get stopped while rebuilding file tree
        if (poll) onChange(ft)
      }
      Thread.sleep(500)
    }
  }

  def stopPolling() { poll = false }
}

object DirectoryWatcher {
  def apply(dir: String)(onChange: FileTree => Any) = new DirectoryWatcher(dir, onChange)
}

