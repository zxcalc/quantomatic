package quanto.gui

import java.io.File

import quanto.cosy.{SimprocBatchResult, SimprocLazyBatchResult, SimprocSingleRun}
import quanto.data.Graph

import scala.swing.{Component, Publisher}

// this is necessarily boring - used to interface correctly with the tabbed GUI
class BatchDerivationResultsDocument(val parent: Component) extends Document with Publisher {
  val description = "Batch Derivation Results"
  val fileExtension = "qsbr"

  var simprocsUsed: Option[List[String]] = None
  var resultsCount: Option[Int] = None
  var allSimprocs: Option[Map[String, String]] = None
  var results: Option[List[SimprocSingleRun]] = None
  var timeTaken: Option[Double] = None
  var notes: Option[String] = None

  override def loadDocument(f: File) = {
    val lazyResults = quanto.util.FileHelper.readFile[SimprocLazyBatchResult](f, SimprocBatchResult.lazyFromJson)
    simprocsUsed = Some(lazyResults.selectedSimprocs)
    notes = Some(lazyResults.notes)
    resultsCount = Some(lazyResults.resultCount)
    publish(DocumentChanged(this))
  }

  def loadFullDocument(f: File): Unit = {
    val batchResults = quanto.util.FileHelper.readFile[SimprocBatchResult](f, SimprocBatchResult.fromJson)
    allSimprocs = Some(batchResults.allSimprocs)
    results = Some(batchResults.singleResults)
    timeTaken = Some(batchResults.singleResults.map(sr => sr.derivationTimings.map(tt => tt._2).sum).sum)
    publish(DocumentChanged(this))
  }

  override def unsavedChanges: Boolean = false

  protected def clearDocument() {
  }

  protected def saveDocument(f: File) {
  }

  override protected def exportDocument(f: File) {
  }

}
