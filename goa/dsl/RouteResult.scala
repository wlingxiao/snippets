package goa.dsl

trait RouteResult {

}

object RouteResult {

  final case class Complete(response: Int) extends RouteResult {
    def getResponse: Int = response
  }

  final case class Rejected(rejections: Seq[Rejection]) extends RouteResult {
  }

}