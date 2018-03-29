package quanto.cosy.test

import org.scalatest.FlatSpec
import quanto.cosy.CoSyRuns.CoSyZX
import quanto.cosy._

import scala.concurrent.duration.Duration

/**
  * Created by hector on 24/05/17.
  */
class CoSyRunSpec extends FlatSpec {

  behavior of "ZX"

  it should "do a small run" in {
    val run: CoSyZX = new CoSyRuns.CoSyZX(List(), Duration.Inf, "~/cosytest", 4, 2, 2)

    run.begin()
  }

}
