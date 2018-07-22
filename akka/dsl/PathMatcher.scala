package akka.dsl

import akka.dsl.TupleOps.Join

import scala.annotation.tailrec

abstract class PathMatcher[L](implicit val ev: Tuple[L]) extends (Path ⇒ PathMatcher.Matching[L]) {
  self =>

  import PathMatcher._

  def / : PathMatcher[L] = this ~ PathMatchers.Slash

  def /[R](other: PathMatcher[R])(implicit join: Join[L, R]): PathMatcher[join.Out] =
    this ~ PathMatchers.Slash ~ other

  def ~[R](other: PathMatcher[R])(implicit join: Join[L, R]): PathMatcher[join.Out] = {
    implicit val joinProducesTuple = Tuple.yes[join.Out]
    transform(_.andThen((restL, valuesL) ⇒ other(restL).map(join(valuesL, _))))
  }

  def transform[R: Tuple](f: Matching[L] ⇒ Matching[R]): PathMatcher[R] =
    new PathMatcher[R] {
      def apply(path: Path) = f(self(path))
    }
}

object PathMatcher extends ImplicitPathMatcherConstruction {

  sealed abstract class Matching[+L: Tuple] {
    def map[R: Tuple](f: L ⇒ R): Matching[R]

    def flatMap[R: Tuple](f: L ⇒ Option[R]): Matching[R]

    def andThen[R: Tuple](f: (Path, L) ⇒ Matching[R]): Matching[R]

    def orElse[R >: L](other: ⇒ Matching[R]): Matching[R]
  }

  case class Matched[L: Tuple](pathRest: Path, extractions: L) extends Matching[L] {
    def map[R: Tuple](f: L ⇒ R) = Matched(pathRest, f(extractions))

    def flatMap[R: Tuple](f: L ⇒ Option[R]) = f(extractions) match {
      case Some(valuesR) ⇒ Matched(pathRest, valuesR)
      case None ⇒ Unmatched
    }

    def andThen[R: Tuple](f: (Path, L) ⇒ Matching[R]) = f(pathRest, extractions)

    def orElse[R >: L](other: ⇒ Matching[R]) = this
  }

  object Matched {
    val Empty = Matched(Path.Empty, ())
  }

  case object Unmatched extends Matching[Nothing] {
    def map[R: Tuple](f: Nothing ⇒ R) = this

    def flatMap[R: Tuple](f: Nothing ⇒ Option[R]) = this

    def andThen[R: Tuple](f: (Path, Nothing) ⇒ Matching[R]) = this

    def orElse[R](other: ⇒ Matching[R]) = other
  }

  def apply[L: Tuple](prefix: Path, extractions: L): PathMatcher[L] =
    if (prefix.isEmpty) provide(extractions)
    else new PathMatcher[L] {
      def apply(path: Path) =
        if (path startsWith prefix) Matched(path dropChars prefix.charCount, extractions)(ev)
        else Unmatched
    }

  def provide[L: Tuple](extractions: L): PathMatcher[L] =
    new PathMatcher[L] {
      def apply(path: Path) = Matched(path, extractions)(ev)
    }
}

trait PathMatchers {

  import PathMatcher._

  object Slash extends PathMatcher0 {
    def apply(path: Path) = path match {
      case Path.Slash(tail) ⇒ Matched(tail, ())
      case _ ⇒ Unmatched
    }
  }

  object PathEnd extends PathMatcher0 {
    def apply(path: Path) = path match {
      case Path.Empty ⇒ Matched.Empty
      case _ ⇒ Unmatched
    }
  }

  abstract class NumberMatcher[@specialized(Int, Long) T](max: T, base: T)(implicit x: Integral[T])
    extends PathMatcher1[T] {

    import x._ // import implicit conversions for numeric operators
    val minusOne = x.zero - x.one
    val maxDivBase = max / base

    def apply(path: Path): Matching[Tuple1[T]] = path match {
      case Path.Segment(segment, tail) ⇒
        @tailrec def digits(ix: Int = 0, value: T = minusOne): Matching[Tuple1[T]] = {
          val a = if (ix < segment.length) fromChar(segment charAt ix) else minusOne
          if (a == minusOne) {
            if (value == minusOne) Unmatched
            else Matched(if (ix < segment.length) segment.substring(ix) :: tail else tail, Tuple1(value))
          } else {
            if (value == minusOne) digits(ix + 1, a)
            else if (value <= maxDivBase && value * base <= max - a) // protect from overflow
              digits(ix + 1, value * base + a)
            else Unmatched
          }
        }

        digits()

      case _ => Unmatched
    }

    def fromChar(c: Char): T

    def fromDecimalChar(c: Char): T = if ('0' <= c && c <= '9') x.fromInt(c - '0') else minusOne

    def fromHexChar(c: Char): T =
      if ('0' <= c && c <= '9') x.fromInt(c - '0') else {
        val cn = c | 0x20 // normalize to lowercase
        if ('a' <= cn && cn <= 'f') x.fromInt(cn - 'a' + 10) else minusOne
      }
  }

  object IntNumber extends NumberMatcher[Int](Int.MaxValue, 10) {
    def fromChar(c: Char): Int = fromDecimalChar(c)
  }

  object LongNumber extends NumberMatcher[Long](Long.MaxValue, 10) {
    def fromChar(c: Char): Long = fromDecimalChar(c)
  }

}

object PathMatchers extends PathMatchers