/*  Title:      Pure/System/system_channel.scala
    Author:     Makarius

Socket-based system channel for inter-process communication.
*/

package isabelle


import java.io.{InputStream, OutputStream}
import java.net.{ServerSocket, InetAddress}


object System_Channel
{
  def apply(): System_Channel = new System_Channel
}

class System_Channel private
{
  private val server = new ServerSocket(0, 2, InetAddress.getByName("127.0.0.1"))

  def params: List[String] = List("127.0.0.1", server.getLocalPort.toString)

  def prover_args: List[String] = List("-P", "127.0.0.1:" + server.getLocalPort)

  def rendezvous(): (OutputStream, InputStream) =
  {
    val socket = server.accept
    socket.setTcpNoDelay(true)
    (socket.getOutputStream, socket.getInputStream)
  }

  def accepted() { server.close }
}
