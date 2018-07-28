package goa.dsl

import java.nio.charset.{Charset, StandardCharsets}

import scala.annotation.tailrec

sealed abstract class Path {
  type Head

  def isEmpty: Boolean

  def startsWithSlash: Boolean

  def startsWithSegment: Boolean

  def endsWithSlash: Boolean = {
    import Path.{Empty ⇒ PEmpty, _}
    @tailrec def check(path: Path): Boolean = path match {
      case PEmpty ⇒ false
      case Slash(PEmpty) ⇒ true
      case Slash(tail) ⇒ check(tail)
      case Segment(_, tail) ⇒ check(tail)
    }

    check(this)
  }

  def head: Head

  def tail: Path

  def length: Int

  def charCount: Int

  def ::(c: Char): Path = {
    require(c == '/')
    Path.Slash(this)
  }

  def ::(segment: String): Path

  def +(pathString: String): Path = this ++ Path(pathString)

  def ++(suffix: Path): Path

  def reverse: Path = reverseAndPrependTo(Path.Empty)

  def reverseAndPrependTo(prefix: Path): Path

  def /(segment: String): Path = this ++ Path.Slash(segment :: Path.Empty)

  def ?/(segment: String): Path = if (this.endsWithSlash) this + segment else this / segment

  def startsWith(that: Path): Boolean

  def dropChars(count: Int): Path
}

object Path {
  val SingleSlash = Slash(Empty)

  def / : Path = SingleSlash

  def /(path: Path): Path = Slash(path)

  def /(segment: String): Path = Slash(segment :: Empty)

  def apply(string: String, charset: Charset = StandardCharsets.UTF_8): Path = {
    @tailrec def build(path: Path = Empty, ix: Int = string.length - 1, segmentEnd: Int = 0): Path =
      if (ix >= 0)
        if (string.charAt(ix) == '/')
          if (segmentEnd == 0) build(Slash(path), ix - 1)
          else build(Slash(decode(string.substring(ix + 1, segmentEnd), charset) :: path), ix - 1)
        else if (segmentEnd == 0) build(path, ix - 1, ix + 1)
        else build(path, ix - 1, segmentEnd)
      else if (segmentEnd == 0) path else decode(string.substring(0, segmentEnd), charset) :: path

    build()
  }

  def decode(string: String, charset: Charset): String = {
    string
  }

  def fail(summary: String, detail: String = "") = throw new IllegalArgumentException(summary + ": " + detail)

  def isAsciiCompatible(cs: Charset) = true

  def unapply(path: Path): Option[String] = Some(path.toString)

  def unapply(uri: Uri): Option[String] = None

  sealed abstract class SlashOrEmpty extends Path {
    def startsWithSegment = false
  }

  case object Empty extends SlashOrEmpty {
    type Head = Nothing

    def isEmpty = true

    def startsWithSlash = false

    def head: Head = throw new NoSuchElementException("head of empty path")

    def tail: Path = throw new UnsupportedOperationException("tail of empty path")

    def length = 0

    def charCount = 0

    def ::(segment: String) = if (segment.isEmpty) this else Segment(segment, this)

    def ++(suffix: Path) = suffix

    def reverseAndPrependTo(prefix: Path) = prefix

    def startsWith(that: Path): Boolean = that.isEmpty

    def dropChars(count: Int) = this
  }

  final case class Slash(tail: Path) extends SlashOrEmpty {
    type Head = Char

    def head = '/'

    def startsWithSlash = true

    def isEmpty = false

    def length: Int = tail.length + 1

    def charCount: Int = tail.charCount + 1

    def ::(segment: String) = if (segment.isEmpty) this else Segment(segment, this)

    def ++(suffix: Path) = Slash(tail ++ suffix)

    def reverseAndPrependTo(prefix: Path) = tail.reverseAndPrependTo(Slash(prefix))

    def startsWith(that: Path): Boolean = that.isEmpty || that.startsWithSlash && tail.startsWith(that.tail)

    def dropChars(count: Int): Path = if (count < 1) this else tail.dropChars(count - 1)
  }

  final case class Segment(head: String, tail: SlashOrEmpty) extends Path {
    if (head.isEmpty) throw new IllegalArgumentException("Path segment must not be empty")
    type Head = String

    def isEmpty = false

    def startsWithSlash = false

    def startsWithSegment = true

    def length: Int = tail.length + 1

    def charCount: Int = head.length + tail.charCount

    def ::(segment: String) = if (segment.isEmpty) this else Segment(segment + head, tail)

    def ++(suffix: Path) = head :: (tail ++ suffix)

    def reverseAndPrependTo(prefix: Path): Path = tail.reverseAndPrependTo(head :: prefix)

    def startsWith(that: Path): Boolean = that match {
      case Segment(`head`, t) ⇒ tail.startsWith(t)
      case Segment(h, Empty) ⇒ head.startsWith(h)
      case x ⇒ x.isEmpty
    }

    def dropChars(count: Int): Path =
      if (count < 1) this
      else if (count >= head.length) tail.dropChars(count - head.length)
      else head.substring(count) :: tail
  }

  object ~ {
    def unapply(cons: Segment): Option[(String, Path)] = Some((cons.head, cons.tail))

    def unapply(cons: Slash): Option[(Char, Path)] = Some(('/', cons.tail))
  }

}
