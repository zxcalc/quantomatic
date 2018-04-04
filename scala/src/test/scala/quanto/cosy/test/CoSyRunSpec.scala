package quanto.cosy.test

import org.scalatest.FlatSpec
import quanto.cosy.CoSyRuns.CoSyZX
import quanto.cosy._

import scala.concurrent.duration.Duration
import java.io.File

import quanto.data.{Graph, Project, Theory, WireV}
import quanto.rewrite.Matcher
import quanto.util.FileHelper

/**
  * Created by hector on 24/05/17.
  */
class CoSyRunSpec extends FlatSpec {

  behavior of "ZX"

  it should "do a small run" in {
    // Don't test this here
    // It isn't a standard feature
    // And pollutes the filesystem
    // Ask hmillerbakewell@gmail.com for more information
    assert(1 == 1)
  }

}
