package goa.dsl

trait ConjunctionMagnet[L] {
  type Out

  def apply(underlying: Directive[L]): Out
}

object ConjunctionMagnet {
  implicit def fromDirective[L, R](other: Directive[R])(implicit join: TupleOps.Join[L, R]): ConjunctionMagnet[L] {type Out = Directive[join.Out]} =
    new ConjunctionMagnet[L] {
      type Out = Directive[join.Out]

      def apply(underlying: Directive[L]) =
        Directive[join.Out] { inner ⇒
          underlying.tapply { prefix ⇒ other.tapply { suffix ⇒ inner(join(prefix, suffix)) } }
        }(Tuple.yes) // we know that join will only ever produce tuples
    }

  implicit def fromStandardRoute[L](route: StandardRoute): ConjunctionMagnet[L] {type Out = StandardRoute} =
    new ConjunctionMagnet[L] {
      type Out = StandardRoute

      def apply(underlying: Directive[L]) = StandardRoute(underlying.tapply(_ ⇒ route))
    }

  implicit def fromRouteGenerator[T, R <: Route](generator: T ⇒ R): ConjunctionMagnet[Unit] {type Out = RouteGenerator[T]} =
    new ConjunctionMagnet[Unit] {
      type Out = RouteGenerator[T]

      def apply(underlying: Directive[Unit]) = value ⇒ underlying.tapply(_ ⇒ generator(value))
    }
}



