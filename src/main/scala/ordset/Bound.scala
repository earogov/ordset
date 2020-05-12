package ordset

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

sealed trait Bound[@sp(spNum) +T] {

  def value: T

  def isUpper: Boolean

  def isLower: Boolean = !isUpper

  def isInclusive: Boolean

  def isExclusive: Boolean = !isInclusive

  def flip: Bound[T]

  def offset: Int
}

object Bound {

  case class Upper[+T](override val value: T, override val isInclusive: Boolean) extends Bound[T] {

    override def isUpper: Boolean = true

    override def isLower: Boolean = false

    override def flip: Bound[T] = Lower(value, !isInclusive)

    def flipUpper: Lower[T] = Lower(value, !isInclusive)

    override def offset: Int = if (isInclusive) 0 else -1

    override def toString: String = s"x ${if (isInclusive) "<=" else "<"} $value"
  }

  case class Lower[+T](override val value: T, override val isInclusive: Boolean) extends  Bound[T] {

    override def isUpper: Boolean = false

    override def isLower: Boolean = true

    override def flip: Bound[T] = Upper(value, !isInclusive)

    def flipLower: Upper[T] = Upper(value, !isInclusive)

    override def offset: Int = if (isInclusive) 0 else 1

    override def toString: String = s"x ${if (isInclusive) ">=" else ">"} $value"
  }

  implicit def toRightUnbounded[T](bound: Lower[T]): Interval.RightUnbounded[T] = Interval.RightUnbounded(bound)

  implicit def toLeftUnbounded[T](bound: Upper[T]): Interval.LeftUnbounded[T] = Interval.LeftUnbounded(bound)

  implicit def defaultAscOrder[T](implicit ord: Order[T]): Order[Bound[T]] = new DefaultAscOrder[T]()(ord)

  implicit def defaultHash[T](implicit hash: Hash[T]): Hash[Bound[T]] = new DefaultHash[T]()(hash)

  class DefaultAscOrder[T](implicit ord: Order[T]) extends Order[Bound[T]] {
    override def compare(x: Bound[T], y: Bound[T]): Int = {
      val cmp = ord.compare(x.value, y.value)
      if (cmp != 0) cmp
      else IntAscOrder.compare(x.offset, y.offset)
    }
  }

  class DefaultHash[T](implicit hash: Hash[T]) extends Hash[Bound[T]] {
    override def hash(x: Bound[T]): Int = 41 * (hash.hash(x.value) + 41 * x.offset)
    override def eqv(x: Bound[T], y: Bound[T]): Boolean = hash.eqv(x.value, y.value) && x.offset == y.offset
  }
}
