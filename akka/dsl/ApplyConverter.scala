package akka.dsl

abstract class ApplyConverter[L] {
  type In
  def apply(f: In): L ⇒ Route
}

object ApplyConverter extends ApplyConverterInstances
