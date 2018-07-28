package goa.dsl

class RequestContextImpl(val request: Request) extends RequestContext {

  override def withUnmatchedPath(path: Path): RequestContext =
    if (path != unmatchedPath) copy(unmatchedPath = path) else this

  override def reject(rejections: Rejection*): RouteResult = {
    RouteResult.Rejected(rejections.toList)
  }

  private def copy(unmatchedPath: Path = unmatchedPath) = new RequestContextImpl(request)


  override val unmatchedPath: Path = null

  override def withUnmatchedPath(path: Request): RequestContext = new RequestContextImpl(request)
}
