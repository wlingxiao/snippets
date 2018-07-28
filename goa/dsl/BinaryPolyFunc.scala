package goa.dsl

trait BinaryPolyFunc {
  def at[A, B] = new CaseBuilder[A, B]

  class CaseBuilder[A, B] {
    def apply[R](f: (A, B) ⇒ R) = new BinaryPolyFunc.Case[A, B, BinaryPolyFunc.this.type] {
      type Out = R

      def apply(a: A, b: B) = f(a, b)
    }
  }

}

object BinaryPolyFunc {

  sealed trait Case[A, B, Op] {
    type Out

    def apply(a: A, b: B): Out
  }

}