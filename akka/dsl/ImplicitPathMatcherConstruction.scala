package akka.dsl

trait ImplicitPathMatcherConstruction {
  import PathMatcher._
  type PathMatcher0 = PathMatcher[Unit]
  type PathMatcher1[T] = PathMatcher[Tuple1[T]]

  implicit def _stringExtractionPair2PathMatcher[T](tuple: (String, T)): PathMatcher1[T] =
    PathMatcher(tuple._1 :: Path.Empty, Tuple1(tuple._2))

  implicit def _segmentStringToPathMatcher(segment: String): PathMatcher0 =
    PathMatcher(segment :: Path.Empty, ())


}
