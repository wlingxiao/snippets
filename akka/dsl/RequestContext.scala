package akka.dsl

import scala.concurrent.Future

trait RequestContext {

  val unmatchedPath: Path

  def withUnmatchedPath(path: Path): RequestContext

  def reject(rejections: Rejection*): Future[RouteResult]

}
