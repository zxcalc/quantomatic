package quanto.gui

import scala.swing._
import javax.swing.{SwingUtilities, JScrollPane, JTree}
import javax.swing.tree.{DefaultTreeModel, TreePath, TreeModel}
import java.io.File
import javax.swing.event._
import java.awt.BorderLayout

class FileTree extends BorderPanel {
  val treeModel = new FileTreeModel
  val fileTree = new JTree(treeModel)

  val scrollPane = new JScrollPane(fileTree)

  peer.add(scrollPane, BorderLayout.CENTER)
  //add(scrollPane, BorderPanel.Position.Center)

  fileTree.setEditable(true)
  fileTree.addTreeSelectionListener(new TreeSelectionListener() {
    def valueChanged(event: TreeSelectionEvent) {
      fileTree.getLastSelectedPathComponent match {
        case FileNode(file) => println("FILE: " + file.getName)
        case EmptyNode => println("empty node")
        case _ => println("something else")
      }
    }
  })

  def root_=(rootDir: Option[String]) {
    rootDir match {
      case Some(dir) =>
        treeModel.root = FileNode(new File(dir))
        SwingUtilities.updateComponentTreeUI(fileTree)
        fileTree.expandRow(0)
      case None =>
        treeModel.root = EmptyNode
        SwingUtilities.updateComponentTreeUI(fileTree)
    }
  }

  def root = treeModel.root match {
    case FileNode(file) => Some(file.getPath)
    case _ => None
  }

  sealed abstract class TreeNode
  case object EmptyNode extends TreeNode {
    override def toString = "(No project open)"
  }

  case class FileNode(file: File) extends TreeNode {
    override def toString = file.getName
  }

  class FileTreeModel extends TreeModel {
    var root: TreeNode = EmptyNode
    val listeners = collection.mutable.Set[TreeModelListener]()
    def getRoot = root

    def getChild(parent: AnyRef, index: Int): AnyRef = parent match {
      case FileNode(directory) =>
        FileNode(new File(directory, directory.list().reverse(index)))
      case _ => EmptyNode
    }

    def getChildCount(parent : AnyRef) = {
      parent match {
        case FileNode(file) =>
          if (file.isDirectory && file.list() != null) file.list().length
          else 0
        case _ => 0
      }
    }

    def isLeaf(node: AnyRef) = node match {
      case FileNode(file) => file.isFile
      case _ => true
    }

    def getIndexOfChild(parent: AnyRef, child: AnyRef) = parent match {
      case FileNode(directory) =>
        directory.list().reverse.indexOf(child.asInstanceOf[File].getName)
      case _ => -1
    }

    def valueForPathChanged(path: TreePath, value: AnyRef) {
      path.getLastPathComponent match {
        case FileNode(oldFile) =>
          val fileParentPath = oldFile.getParent
          val parentNode = FileNode(new File(fileParentPath))
          val newNode = FileNode(new File(fileParentPath, value.asInstanceOf[String]))
          oldFile.renameTo(newNode.file)
          fireTreeNodesChanged(
            path.getParentPath,
            Array(getIndexOfChild(parentNode, newNode)),
            Array[AnyRef](newNode))
        case _ => // do nothing
      }
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
  }
}
