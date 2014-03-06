package quanto.util

/**
 * DirectoryWatcher.scala
 *
 * Uses the Java 7 WatchEvent filesystem API from within Scala.
 * Adapted from:
 *  http://download.oracle.com/javase/tutorial/essential/io/examples/WatchDir.java
 *
 * @author Chris Eberle <eberle1080@gmail.com>
 * @version 0.1
 */

import scala.collection.JavaConverters._
import util.control.Breaks._
import java.nio.file.attribute._
import java.io.IOException
import java.nio.file._

class DirectoryWatcher(val path:Path, val recursive:Boolean, callback: WatchEvent[_] => Unit) extends Runnable {

  val watchService = path.getFileSystem.newWatchService()
  val keys = collection.mutable.HashMap[WatchKey,Path]()
  var trace = false

  /**
   * Print an event
   */
  def printEvent(event:WatchEvent[_]) : Unit = {
    val kind = event.kind
    val event_path = event.context().asInstanceOf[Path]
    if(kind.equals(StandardWatchEventKinds.ENTRY_CREATE)) {
      println("Entry created: " + event_path)
    }
    else if(kind.equals(StandardWatchEventKinds.ENTRY_DELETE)) {
      println("Entry deleted: " + event_path)
    }
    else if(kind.equals(StandardWatchEventKinds.ENTRY_MODIFY)) {
      println("Entry modified: " + event_path)
    }
  }

  /**
   * Register a particular file or directory to be watched
   */
  def register(dir:Path): Unit = {
    val key = dir.register(watchService, StandardWatchEventKinds.ENTRY_CREATE,
      StandardWatchEventKinds.ENTRY_MODIFY,
      StandardWatchEventKinds.ENTRY_DELETE)

    if (trace) {
      val prev = keys.getOrElse(key, null)
      if (prev == null) {
        println("register: " + dir)
      } else {
        if (!dir.equals(prev)) {
          println("update: " + prev + " -> " + dir)
        }
      }
    }

    keys(key) = dir
  }

  /**
   * Makes it easier to walk a file tree
   */
  implicit def makeDirVisitor(f: (Path) => Unit) = new SimpleFileVisitor[Path] {
    override def preVisitDirectory(p: Path, attrs: BasicFileAttributes) = {
      f(p)
      FileVisitResult.CONTINUE
    }
  }

  /**
   *  Recursively register directories
   */
  def registerAll(start:Path): Unit = {
    Files.walkFileTree(start, (f: Path) => {
      register(f)
    })
  }

  /**
   * The main directory watching thread
   */
  override def run(): Unit = {
    try {
      if(recursive) {
        if (trace) println("Scanning " + path + "...")
        registerAll(path)
        if (trace) println("Done.")
      } else {
        register(path)
      }

      //trace = true

      breakable {
        while (true) {
          val key = watchService.take()
          val dir = keys.getOrElse(key, null)
          if(dir != null) {
            key.pollEvents().asScala.foreach( event => {
              val kind = event.kind

              if(kind != StandardWatchEventKinds.OVERFLOW) {
                val name = event.context().asInstanceOf[Path]
                var child = dir.resolve(name)

                callback(event)

                if (recursive && (kind == StandardWatchEventKinds.ENTRY_CREATE)) {
                  try {
                    if (Files.isDirectory(child, LinkOption.NOFOLLOW_LINKS)) {
                      registerAll(child)
                    }
                  } catch {
                    case ioe: IOException => println("IOException: " + ioe)
                    case e: Exception => println("Exception: " + e)
                      break()
                  }
                }
              }
            })
          } else {
            println("WatchKey not recognized!!")
          }

          if (!key.reset()) {
            keys.remove(key)
            if (keys.isEmpty) {
              break()
            }
          }
        }
      }
    } catch {
      case ie: InterruptedException => println("InterruptedException: " + ie)
      case ioe: IOException => println("IOException: " + ioe)
      case e: Exception => println("Exception: " + e)
    }
  }
}

object DirectoryWatcher {
  def apply(path:Path, recursive:Boolean)(callback: WatchEvent[_] => Unit) =
    new DirectoryWatcher(path, recursive, callback)
}
