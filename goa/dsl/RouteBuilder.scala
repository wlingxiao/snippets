package goa.dsl

import goa.dsl.TupleOps.Join

abstract class RouteBuilder[L] extends (Request ⇒ Matching[L]) {
  self =>
  def /[R](other: PathMatcher[R])(implicit join: Join[L, R]): PathMatcher[join.Out] = {
    abce(other)
  }

  private def abce[R](other: PathMatcher[R])(implicit join: Join[L, R]): PathMatcher[join.Out] = {
    implicit val joinProducesTuple = Tuple.yes[join.Out]
    transform(_.andThen((restL, valuesL) ⇒ other(restL).map(join(valuesL, _))))
  }

  private def transform[R: Tuple](f: Matching[L] ⇒ Matching[R]): PathMatcher[R] =
    new PathMatcher[R] {
      def apply(path: Request) = f(self(path))
    }
}

object GET extends RouteBuilder[Unit] {
  override def apply(v1: Request): Matching[Unit] = Matched(v1, Tuple2(1, 2))
}