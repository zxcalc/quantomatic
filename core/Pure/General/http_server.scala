/*  Title:      Pure/General/http_server.scala
    Author:     Makarius

Minimal HTTP server.
*/

package isabelle


import java.net.{InetAddress, InetSocketAddress}
import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}


object HTTP_Server
{
  def apply(handler: HttpExchange => Unit): HTTP_Server =
  {
    val localhost = InetAddress.getByName("127.0.0.1")

    val server = HttpServer.create(new InetSocketAddress(localhost, 0), 0)
    server.createContext("/", new HttpHandler { def handle(x: HttpExchange) { handler(x) } })
    server.setExecutor(null)
    new HTTP_Server(server)
  }
}

class HTTP_Server private(val server: HttpServer)
{
  def start: Unit = server.start
  def stop: Unit = server.stop(0)

  def address: InetSocketAddress = server.getAddress
  def url: String = "http://" + address.getHostName + ":" + address.getPort
  override def toString: String = url
}
