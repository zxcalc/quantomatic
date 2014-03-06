package quanto.gui

import scala.swing._
import scala.collection.JavaConversions._
import javax.swing.{SwingUtilities, JScrollPane, JTree}
import javax.swing.tree.{TreePath, TreeModel}
import java.io.File
import javax.swing.event._
import java.awt.BorderLayout
import quanto.util._
import java.nio.file.{WatchEvent, FileSystems}

class FileTree extends BorderPanel {
  val fileTreeModel = new FileTreeModel
  val fileTree = new JTree(fileTreeModel)

  val scrollPane = new JScrollPane(fileTree)

  peer.add(scrollPane, BorderLayout.CENTER)
  //add(scrollPane, BorderPanel.Position.Center)

  fileTree.setEditable(true)
  fileTree.addTreeSelectionListener(new TreeSelectionListener() {
    def valueChanged(event: TreeSelectionEvent) {
      fileTree.getLastSelectedPathComponent match {
        case FileNode(file) =>
        case EmptyNode =>
        case _ =>
      }
    }
  })

  def refreshTree(rootChanged: Boolean = false) {
    if (!rootChanged) {
      // save the tree state
      val expanded = fileTree.getExpandedDescendants(fileTreeModel.rootPath)
      val selected = fileTree.getSelectionPaths

      // reload the tree
      fileTreeModel.fireTreeStructureChanged(fileTreeModel.rootPath)

      // restore the tree state
      if (expanded != null) expanded.foreach { fileTree.expandPath }
      fileTree.addSelectionPaths(selected)
    } else {
      // just reload the tree
      fileTreeModel.fireTreeStructureChanged(fileTreeModel.rootPath)
    }
  }

//  class PollThread extends Thread {
//    var continue = true
//
//    def stopPolling() { continue = false }
//
//    override def run() {
//      fileTreeModel.root match {
//        case FileNode(d) =>
//          val dir = Paths.get(d.getPath)
//          val watcher = dir.getFileSystem.newWatchService()
//          dir.register(watcher,
//            StandardWatchEventKinds.ENTRY_CREATE,
//            StandardWatchEventKinds.ENTRY_DELETE,
//            StandardWatchEventKinds.ENTRY_MODIFY)
//          val watchKey = watcher.take()
//
//          while (continue) {
//            val events = watchKey.pollEvents()
//            if (!events.isEmpty && continue) { println("got events: " + events) } // do something here
//          }
//        case _ => // do nothing
//      }
//    }
//  }


  var pollThread: Option[Thread] = None

  def root_=(rootDir: Option[String]) {
    rootDir match {
      case Some(dir) =>
        fileTreeModel.root = FileNode(new File(dir))

        pollThread.map { _.interrupt() }
        val rootPath = FileSystems.getDefault.getPath(dir)

        val watcher = DirectoryWatcher(rootPath, recursive = true){ e =>
          SwingUtilities.invokeLater(new Runnable {
            def run() = refreshTree()
          })
        }

        val watcherThread = new Thread(watcher)
        watcherThread.start()
        pollThread = Some(watcherThread)

        refreshTree(rootChanged = true)
        fileTree.expandRow(0)
      case None =>
        fileTreeModel.root = EmptyNode
        refreshTree()
    }
  }

  def root = fileTreeModel.root match {
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

    def rootPath = new TreePath(root)

    def getChild(parent: AnyRef, index: Int): AnyRef = parent match {
      case FileNode(directory) =>
        val files = directory.list().sorted
        FileNode(new File(directory, files(index)))
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

    def getIndexOfChild(parent: AnyRef, child: AnyRef) = (parent, child) match {
      case (FileNode(directory), FileNode(file)) =>
        val files = directory.list().sorted
        files.indexOf(file.getName)
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

    def fireTreeStructureChanged(path: TreePath) {
      val event = new TreeModelEvent(this, path, Array[Int](), Array[AnyRef]())
      listeners.map { _.treeStructureChanged(event) }
    }

    def addTreeModelListener(listener: TreeModelListener) {
      listeners += listener
    }

    def removeTreeModelListener(listener: TreeModelListener) {
      listeners -= listener
    }
  }
}
