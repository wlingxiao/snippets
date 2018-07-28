package goa.dsl

import goa.dsl.TupleOps.Join

abstract class QueryBuilder[L](implicit val ev: Tuple[L]) extends (Request ⇒ Matching[L]) {
  self =>

  import QueryBuilder._

  def &[R](other: QueryBuilder[R])(implicit join: Join[L, R]
  ): QueryBuilder[join.Out] = {
    this ~ Slash ~ other
  }

  def ~[R](other: QueryBuilder[R])(implicit join: Join[L, R]): QueryBuilder[join.Out] = {
    implicit val joinProducesTuple = Tuple.yes[join.Out]
    transform(_.andThen { (restL, valuesL) =>
      other(restL).map(join(valuesL, _))
    })
  }

  private def transform[R: Tuple](f: Matching[L] ⇒ Matching[R]): QueryBuilder[R] =
    new QueryBuilder[R] {
      def apply(path: Request) = f(self(path))
    }

  def |>> : Directive[L] = {
    extract { ctx: RequestContext => this (ctx.request) }.flatMap {
      case Matched(rest, values) ⇒
        tprovide(values) & mapRequestContext(_ withUnmatchedPath rest)
      case Unmatched ⇒
        throw new IllegalStateException("unmatched")
    }
  }

  private def mapInnerRoute(f: Route => Route): Directive[Unit] =
    Directive { inner: (Unit => Route) => f(inner(())) }

  private def mapRequestContext(f: RequestContext => RequestContext): Directive[Unit] = {
    mapInnerRoute { inner => ctx => inner(f(ctx)) }
  }

  private def tprovide[T: Tuple](values: T): Directive[T] =
    Directive {
      _ (values)
    }

  private def extract[T](f: RequestContext ⇒ T): Directive[Tuple1[T]] =
    textract(ctx ⇒ Tuple1(f(ctx)))

  private def textract[T: Tuple](f: RequestContext ⇒ T): Directive[T] =
    Directive { inner ⇒ ctx ⇒ inner(f(ctx))(ctx) }

}

object QueryBuilder {

  def apply[L: Tuple](str: String): QueryBuilder[L] =
    new QueryBuilder[L] {
      override def apply(v1: Request): Matching[L] = ???
    }

  object Slash extends QueryBuilder[Unit] {
    override def apply(v1: Request): Matching[Unit] = Matched(v1, ())
  }

}

object query {

  import scala.reflect._

  class Query[T: ClassTag](name: String) extends QueryBuilder[Tuple1[T]] {
    override def apply(path: Request): Matching[Tuple1[T]] = {
      path match {
        case req if req.queryParam.get(name).isDefined =>
          if (classTag[T].runtimeClass.isAssignableFrom(classOf[Long])) {
            Matched(path, Tuple1(req.queryParam.get(name).get.toLong)).asInstanceOf[Matching[Tuple1[T]]]
          } else {
            Matched(path, Tuple1(req.queryParam.get(name).get)).asInstanceOf[Matching[Tuple1[T]]]
          }
        case _ ⇒ Unmatched
      }
    }
  }

  def apply[T: ClassTag](str: String): QueryBuilder[Tuple1[T]] = new Query[T](str)

  def apply[T]: QueryBuilder[Tuple1[T]] = QueryBuilder(null)

}

object cookie {

  def apply[T](str: String): QueryBuilder[Tuple1[T]] = QueryBuilder(str)

}

object header {

  def apply[T](str: String): QueryBuilder[Tuple1[T]] = QueryBuilder(str)

}

object body {

  def apply[T]: QueryBuilder[Tuple1[T]] = QueryBuilder(null)

}