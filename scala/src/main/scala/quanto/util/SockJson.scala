package quanto.util

import java.net._
import java.io._
import scala.io._
import quanto.util.json._

object SockJson {
  private var socket : Socket = null;
  private var sock_in : Json.Input  = null;
  private var sock_out : Json.Output = null;

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
    ).writeTo(sock_out)

    sock_out.flush()

    if (resp){
      Json.parse(sock_in) match {
        case JsonObject(map) => map("output")
      }
    }
    else{
      JsonNull()
    }

  }

  def connect_sock () {
    socket = new Socket(InetAddress.getByName("localhost"), 4230);

    sock_out = new Json.Output(new BufferedWriter(new OutputStreamWriter(socket.getOutputStream)))
    sock_in = new Json.Input(new BufferedReader(new InputStreamReader(socket.getInputStream)))
  }

  def close_sock () {
    sock_in.close();
    sock_out.close();
    socket.close();
    socket = null
    sock_in = null;
    sock_out = null;
  }

  def request_init () = {
    request("current_status", true);
  }

  def request_next () = {
    request("next_status", true);
  }

  def request_prev () = {
    request("previous_status", true);
  }

  def request_deinit () = {
    request("close", false);
  }
}
