package akka.dsl

import PathDirectives._
import PathMatchers._
import PathMatcher._

import scala.concurrent.Future

object Example extends App {

  val con = new RequestContextImpl(Path("/users/1/1"))

  val ret = pathPrefix("users" / IntNumber / LongNumber) { (a, b) =>
    println(a)
    con => Future.successful(RouteResult.Complete(200))
  }
  ret(con)
}
