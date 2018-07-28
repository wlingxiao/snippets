package goa.dsl

abstract class Directive[L](implicit val ev: Tuple[L]) {
  def tapply(f: L ⇒ Route): Route

  def &(magnet: ConjunctionMagnet[L]): magnet.Out = magnet(this)

  def tmap[R](f: L ⇒ R)(implicit tupler: Tupler[R]): Directive[tupler.Out] =
    Directive[tupler.Out] { inner ⇒ tapply { values ⇒ inner(tupler(f(values))) } }(tupler.OutIsTuple)

  def tflatMap[R: Tuple](f: L ⇒ Directive[R]): Directive[R] =
    Directive[R] { inner ⇒ tapply { values ⇒ f(values) tapply inner } }

  def trequire(predicate: L ⇒ Boolean, rejections: Rejection*): Directive[Unit] =
    tfilter(predicate, rejections: _*).tflatMap(_ ⇒ Directive.Empty)

  def tfilter(predicate: L ⇒ Boolean, rejections: Rejection*): Directive[L] =
    Directive[L] { inner ⇒ tapply { values ⇒ ctx ⇒ if (predicate(values)) inner(values)(ctx) else ctx.reject(rejections: _*) } }

}

object Directive {

  def apply[T: Tuple](f: (T ⇒ Route) ⇒ Route): Directive[T] =
    new Directive[T] {
      def tapply(inner: T ⇒ Route) = f(inner)
    }

  val Empty: Directive[Unit] = Directive(_ (()))

  implicit def addDirectiveApply[L](directive: Directive[L])(implicit hac: ApplyConverter[L]): hac.In ⇒ Route =
    f ⇒ directive.tapply(hac(f))

  implicit def addByNameNullaryApply(directive: Directive[Unit]): (⇒ Route) ⇒ Route =
    r ⇒ directive.tapply(_ ⇒ r)

  implicit class SingleValueModifiers[T](underlying: Directive[Tuple1[T]]) extends AnyRef {
    def map[R](f: T ⇒ R)(implicit tupler: Tupler[R]): Directive[tupler.Out] =
      underlying.tmap { case Tuple1(value) ⇒ f(value) }

    def flatMap[R: Tuple](f: T ⇒ Directive[R]): Directive[R] =
      underlying.tflatMap { case Tuple1(value) ⇒ f(value) }

    def require(predicate: T ⇒ Boolean, rejections: Rejection*): Directive[Unit] =
      underlying.filter(predicate, rejections: _*).tflatMap(_ ⇒ Empty)

    def filter(predicate: T ⇒ Boolean, rejections: Rejection*): Directive[Tuple1[T]] =
      underlying.tfilter({ case Tuple1(value) ⇒ predicate(value) }, rejections: _*)
  }

}