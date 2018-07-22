package akka.dsl

trait RouteDirectives {

  /**
    * Rejects the request with an empty set of rejections.
    *
    * @group route
    */
  def reject: StandardRoute = RouteDirectives._reject

}

object RouteDirectives extends RouteDirectives {
  private val _reject = StandardRoute(_.reject())
}
