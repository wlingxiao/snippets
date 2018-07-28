package goa.dsl

class TupleOps[T](val tuple: T) extends AnyVal {

  import TupleOps._

  def append[S](value: S)(implicit ao: AppendOne[T, S]): ao.Out = ao(tuple, value)

  def foldLeft[In](zero: In)(op: BinaryPolyFunc)(implicit fold: FoldLeft[In, T, op.type]): fold.Out = fold(zero, tuple)

  def join[S](suffixTuple: S)(implicit join: Join[T, S]): join.Out = join(tuple, suffixTuple)
}

object TupleOps {
  implicit def enhanceTuple[T: Tuple](tuple: T): TupleOps[T] = new TupleOps(tuple)

  trait AppendOne[P, S] {
    type Out

    def apply(prefix: P, last: S): Out
  }

  object AppendOne extends TupleAppendOneInstances

  trait FoldLeft[In, T, Op] {
    type Out

    def apply(zero: In, tuple: T): Out
  }

  object FoldLeft extends TupleFoldInstances

  trait Join[P, S] {
    type Out

    def apply(prefix: P, suffix: S): Out
  }

  type JoinAux[P, S, O] = Join[P, S] {type Out = O}

  object Join extends LowLevelJoinImplicits {
    implicit def join0P[T]: JoinAux[Unit, T, T] =
      new Join[Unit, T] {
        type Out = T

        def apply(prefix: Unit, suffix: T): Out = suffix
      }

    // we implement the join by folding over the suffix with the prefix as growing accumulator
    object Fold extends BinaryPolyFunc {
      implicit def step[T, A](implicit append: AppendOne[T, A]): BinaryPolyFunc.Case[T, A, Fold.type] {type Out = append.Out} =
        at[T, A](append(_, _))
    }

  }

  sealed abstract class LowLevelJoinImplicits {
    implicit def join[P, S](implicit fold: FoldLeft[P, S, Join.Fold.type]): JoinAux[P, S, fold.Out] =
      new Join[P, S] {
        type Out = fold.Out

        def apply(prefix: P, suffix: S): Out = fold(prefix, suffix)
      }
  }

}

