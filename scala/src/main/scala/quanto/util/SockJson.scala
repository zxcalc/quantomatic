package quanto.util

import java.net._
import java.io._
import scala.io._
import quanto.util.json._


object SockJsonErrorType extends Enumeration {
  val NoPrev, GoodEval, ErrEval = Value
}

case class SockJsonError(message: String, errorType: SockJsonErrorType.Value) extends Exception(message)


object SockJson {
  private var socket : Socket = null;
  private var sockIn : Json.Input  = null;
  private var sockOut : Json.Output = null;

  val ctrl = "PSGraphCtrl"
  val module = "JsonSocket"
  var input = JsonNull();
  val rid = 28;

  def request(function: String, resp : Boolean): Json =
  {
    JsonObject(
      "request_id" -> rid,
      "controller" -> ctrl,
      "module"     -> module,
      "function"   -> function,
      "input"      -> input
    ).writeTo(sockOut)

    sockOut.flush()

    if (resp){
      Json.parse(sockIn) match {
        case JsonObject(map) => map("output")
      }
    }
    else{
      JsonNull()
    }

  }

  def connectSock () {
    socket = new Socket(InetAddress.getByName("localhost"), 4234);

    sockOut = new Json.Output(new BufferedWriter(new OutputStreamWriter(socket.getOutputStream)))
    sockIn = new Json.Input(new BufferedReader(new InputStreamReader(socket.getInputStream)))
  }

  def closeSock () {
    sockIn.close();
    sockOut.close();
    socket.close();
    socket = null
    sockIn = null;
    sockOut = null;
  }

  def requestInit () = {
    request("current_status", true);
  }

  def requestNext () = {
    val json = request("next_status", true);

    json match{
      case JsonNull() =>
        throw new SockJsonError ("Eval error !", SockJsonErrorType.ErrEval)
      case JsonString(v) =>
        if (json.stringValue == "SUCCESS")
          throw new SockJsonError ("Proof success !", SockJsonErrorType.GoodEval)
        else
          json
      case _ =>
        json
    }
  }

  def requestPrev () = {
    val json = request("previous_status", true);
    json match{
      case JsonNull() =>
        throw new SockJsonError ("Eval error !", SockJsonErrorType.ErrEval)
      case JsonString(v) =>
        if (json.stringValue == "TOP")
          throw new SockJsonError ("No Prev !", SockJsonErrorType.NoPrev)
        else
          json
      case _ =>
        json
    }
  }


  def requestDeinit () = {
    request("close", false);
  }
}
