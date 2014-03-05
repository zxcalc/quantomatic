package quanto.gui

import scala.swing._
import javax.swing.JTree
import javax.swing.tree.{TreePath, TreeModel}
import java.io.File
import javax.swing.event.{TreeModelEvent, TreeModelListener}

class FileTree extends BorderPanel {
  val tree = new JTree
  val scrollPane = new ScrollPane

  scrollPane.peer.add(tree)
  add(scrollPane, BorderPanel.Position.Center)

  class FileSystemModel(val root: File) extends TreeModel {
    val listeners = collection.mutable.Set[TreeModelListener]()
    def getRoot = root

    def getChild(parent: AnyRef, index: Int): AnyRef = {
      val directory = parent.asInstanceOf[File]
      new TreeFile(directory, directory.list()(index))
    }

    def getChildCount(parent : AnyRef) = {
      val file = parent.asInstanceOf[File]
      if (file.isDirectory && file.list() != null) file.list().length
      else 0
    }

    def isLeaf(node: AnyRef) = node.asInstanceOf[File].isFile

    def getIndexOfChild(parent: AnyRef, child: AnyRef) = {
      val directory = parent.asInstanceOf[File]
      directory.list().indexOf(child.asInstanceOf[File].getName)
    }

    def valueForPathChanged(path: TreePath, value: AnyRef) {
      val oldFile = path.getLastPathComponent.asInstanceOf[File]
      val fileParentPath = oldFile.getParent
      val targetFile = new File(fileParentPath, value.asInstanceOf[String])
      oldFile.renameTo(targetFile)
      val parent = new File(fileParentPath)
      fireTreeNodesChanged(
        path.getParentPath,
        Array(getIndexOfChild(parent, targetFile)),
        Array[AnyRef](targetFile))
    }

    def fireTreeNodesChanged(parentPath: TreePath, indices: Array[Int], children: Array[AnyRef]) {
      val event = new TreeModelEvent(this, parentPath, indices, children)
      listeners.map { _.treeNodesChanged(event) }
    }

    def addTreeModelListener(listener: TreeModelListener) {
      listeners += listener
    }

    def removeTreeModelListener(listener: TreeModelListener) {
      listeners -= listener
    }

    class TreeFile(parent: File, child: String) extends File(parent, child) {
      override def toString = getName
    }
  }
}
