package akka

import scala.concurrent.Future


package object dsl {
  type Route = RequestContext ⇒ Future[RouteResult]

  type RouteGenerator[T] = T ⇒ Route
  type Directive0 = Directive[Unit]
  type Directive1[T] = Directive[Tuple1[T]]
  type PathMatcher0 = PathMatcher[Unit]
  type PathMatcher1[T] = PathMatcher[Tuple1[T]]

  def FIXME = throw new RuntimeException("Not yet implemented")
}
