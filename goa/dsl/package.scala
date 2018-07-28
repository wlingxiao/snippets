package goa

package object dsl {

  type Route = RequestContext ⇒ RouteResult

  type RouteGenerator[T] = T ⇒ Route
  type Directive0 = Directive[Unit]
  type Directive1[T] = Directive[Tuple1[T]]
  type PathMatcher0 = PathMatcher[Unit]
  type PathMatcher1[T] = PathMatcher[Tuple1[T]]

  type Request = goa.Request

}
