package akka.dsl

import scala.concurrent.Future

class RequestContextImpl(val unmatchedPath: Path) extends RequestContext {

  override def withUnmatchedPath(path: Path): RequestContext =
    if (path != unmatchedPath) copy(unmatchedPath = path) else this

  override def reject(rejections: Rejection*): Future[RouteResult] = {
    Future.successful(RouteResult.Rejected(rejections.toList))
  }

  private def copy(unmatchedPath: Path = unmatchedPath) = new RequestContextImpl(unmatchedPath)
}
