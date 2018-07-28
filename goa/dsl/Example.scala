package goa.dsl

import java.nio.ByteBuffer

import goa.dsl.PathMatcher._
import goa.dsl.PathMatchers.IntNumber
import goa.http1.{BodyReader, HttpRequest}
import goa.util.BufferUtils

case class User(name: String)

object Example extends App {
  val headers = Seq("User-Agent" -> "Firefox", "Content-Type" -> "text/html")
  val httpRequest = HttpRequest("GET", "/test/111?age=111&name=666", 1, 1, headers, new BodyReader {
    override def discard(): Unit = ???

    override def apply(): ByteBuffer = {
      BufferUtils.emptyBuffer
    }

    override def isExhausted: Boolean = ???
  })
  val request = goa.Request(null, httpRequest)
  val con = new RequestContextImpl(request)
  val ret = GET / "test" / IntNumber +? query[Long]("age") & query[Long]("name") |>> { (a, b, c) =>
    println(a)
    println(b)
    println(c)
    println("how are you")
    OK()
  }

  def OK(): Route = {
    _ => RouteResult.Complete(200)
  }

  //println(ret)
  println(ret(con))
}
