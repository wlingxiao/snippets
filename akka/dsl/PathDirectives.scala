package akka.dsl


trait PathDirectives {

  import akka.dsl.PathMatchers._
  import akka.dsl.PathMatcher._
  import Directive._
  import RouteDirectives._

  def mapInnerRoute(f: Route => Route): Directive0 =
    Directive { inner => f(inner(())) }

  def path[L](pm: PathMatcher[L]): Directive[L] = pathPrefix(pm ~ PathEnd)

  def pathPrefix[L](pm: PathMatcher[L]): Directive[L] = rawPathPrefix(Slash ~ pm)

  def rawPathPrefix[L](pm: PathMatcher[L]): Directive[L] = {
    implicit val LIsTuple = pm.ev
    extract(ctx ⇒ pm(ctx.unmatchedPath)).flatMap {
      case Matched(rest, values) ⇒
        tprovide(values) & mapRequestContext(_ withUnmatchedPath rest)
      case Unmatched ⇒
        reject
    }
  }

  def pathEnd: Directive0 = rawPathPrefix(PathEnd)

  def mapRequestContext(f: RequestContext => RequestContext): Directive0 = {
    mapInnerRoute { inner => {
      ctx => {
        inner(f(ctx))
      }
    }
    }
  }

  def tprovide[L: Tuple](values: L): Directive[L] =
    Directive {
      _ (values)
    }

  def extract[T](f: RequestContext ⇒ T): Directive1[T] =
    textract(ctx ⇒ Tuple1(f(ctx)))

  def textract[L: Tuple](f: RequestContext ⇒ L): Directive[L] =
    Directive { inner ⇒ ctx ⇒ inner(f(ctx))(ctx) }
}

object PathDirectives extends PathDirectives