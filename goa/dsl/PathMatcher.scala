package goa.dsl

import goa.dsl.TupleOps.Join

abstract class PathMatcher[L](implicit val ev: Tuple[L]) extends (Request ⇒ Matching[L]) {
  self =>

  def /[R](other: PathMatcher[R])(implicit join: Join[L, R]): PathMatcher[join.Out] =
    this ~ PathMatchers.Slash ~ other

  private def ~[R](other: PathMatcher[R])(implicit join: Join[L, R]): PathMatcher[join.Out] = {
    implicit val joinProducesTuple = Tuple.yes[join.Out]
    transform(_.andThen((restL, valuesL) ⇒ other(restL).map(join(valuesL, _))))
  }

  private def transform[R: Tuple](f: Matching[L] ⇒ Matching[R]): PathMatcher[R] =
    new PathMatcher[R] {
      def apply(path: Request) = f(self(path))
    }

  def +?[R](other: QueryBuilder[R])(implicit join: Join[L, R]): QueryBuilder[join.Out] = {
    aada(other)
  }

  def aada[R](other: QueryBuilder[R])(implicit join: Join[L, R]): QueryBuilder[join.Out] = {
    implicit val joinProducesTuple = Tuple.yes[join.Out]
    trffansform(_.andThen { (restL, valuesL) =>
      other(restL).map(join(valuesL, _))
    })
  }

  private def trffansform[R: Tuple](f: Matching[L] ⇒ Matching[R]): QueryBuilder[R] =
    new QueryBuilder[R] {
      def apply(path: Request) = f(self(path))
    }

  def |>> : Directive[L] = ???

}

object PathMatcher extends ImplicitPathMatcherConstruction {

  def apply[L: Tuple](prefix: Path, extractions: L): PathMatcher[L] =
    if (prefix.isEmpty) provide(extractions)
    else new PathMatcher[L] {
      def apply(path: Request) =
        if (true) Matched(path, extractions)(ev)
        else Unmatched
    }


  def provide[L: Tuple](extractions: L): PathMatcher[L] =
    new PathMatcher[L] {
      def apply(path: Request) = Matched(path, extractions)(ev)
    }

  def apply[T: Tuple](f: (T ⇒ Route) ⇒ Route): Directive[T] =
    new Directive[T] {
      def tapply(inner: T ⇒ Route) = f(inner)
    }

}

trait PathMatchers {

  object IntNumber extends PathMatcher[Tuple1[Int]] {
    override def apply(v1: Request): Matching[Tuple1[Int]] = {
      Matched(v1, Tuple1(1))
    }
  }

  object Slash extends PathMatcher[Unit] {
    override def apply(v1: Request): Matching[Unit] = Matched(v1, ())
  }

}

object PathMatchers extends PathMatchers