package quanto.core.test
import org.scalatest._
import akka.actor._
import akka.pattern.ask
import quanto.core._
import scala.concurrent._
import duration._
import ExecutionContext.Implicits.global

class ParCoreSpec extends FlatSpec {
  val sys = ActorSystem("Quanto-Test")
  val core = sys.actorOf(Props { new CoreState("../core/bin/quanto-core") }, "core_state")
  implicit val timeout = new akka.util.Timeout(5 seconds)

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
    val f1 = core ? testReq(0)
    f1 foreach { json => println(json) }
  }

  it should "stop" in {
    Thread.sleep(500)
    core ! StopCore
  }
}
