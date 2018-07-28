package goa.dsl

sealed abstract class Matching[+L: Tuple] {
  def map[R: Tuple](f: L ⇒ R): Matching[R]

  def flatMap[R: Tuple](f: L ⇒ Option[R]): Matching[R]

  def andThen[R: Tuple](f: (Request, L) ⇒ Matching[R]): Matching[R]

  def orElse[R >: L](other: ⇒ Matching[R]): Matching[R]
}

case object Unmatched extends Matching[Nothing] {
  def map[R: Tuple](f: Nothing ⇒ R) = this

  def flatMap[R: Tuple](f: Nothing ⇒ Option[R]) = this

  def andThen[R: Tuple](f: (Request, Nothing) ⇒ Matching[R]) = this

  def orElse[R](other: ⇒ Matching[R]) = other
}

case class Matched[L: Tuple](pathRest: Request, extractions: L) extends Matching[L] {
  def map[R: Tuple](f: L ⇒ R) = Matched(pathRest, f(extractions))

  def flatMap[R: Tuple](f: L ⇒ Option[R]) = f(extractions) match {
    case Some(valuesR) ⇒ Matched(pathRest, valuesR)
    case None ⇒ Unmatched
  }

  def andThen[R: Tuple](f: (Request, L) => Matching[R]) = f(pathRest, extractions)

  def orElse[R >: L](other: ⇒ Matching[R]) = this
}
