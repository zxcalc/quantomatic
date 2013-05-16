package quanto.core.test
import org.scalatest._
import akka.actor._
import akka.pattern.ask
import quanto.core._
import scala.concurrent._
import ExecutionContext.Implicits.global

class ParCoreSpec extends FlatSpec {
  val sys = ActorSystem("Quanto-Test")
  val core = sys.actorOf(Props { new CoreState("../core/bin/quanto-core") }, "core_state")

  def testReq(rid: Int) = new SimpleRequest(
    s"""
      |{
      |  "request_id": ${rid},
      |  "controller": "!!",
      |  "module": "system",
      |  "function": "version"
      |}
    """.stripMargin)

  "CoreState actor" should "accept some requests" in {
    core ! testReq(0)
    core ! testReq(1)
  }

  //sys.shutdown()
}
