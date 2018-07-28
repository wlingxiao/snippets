package goa.dsl

import scala.concurrent.Future

trait RequestContext {

  val unmatchedPath: Path

  def withUnmatchedPath(path: Path): RequestContext

  def reject(rejections: Rejection*): RouteResult

  def request: Request

  def withUnmatchedPath(path: Request): RequestContext

}
