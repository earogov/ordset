package ordset

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

sealed trait Interval[@sp(spNum) +T] {

  def ->[@sp(Boolean) V](value: V): IntervalMapping[T, V] = IntervalMapping(this, value)
}

object Interval {

  def unbounded(): Unbounded.type = Unbounded

  def rightUnbounded[@sp(spNum) T](value: T, isInclusive: Boolean): RightUnbounded[T] =
    RightUnbounded(Bound.Lower(value, isInclusive))

  def leftUnbounded[@sp(spNum) T](value: T, isInclusive: Boolean): LeftUnbounded[T] =
    LeftUnbounded(Bound.Upper(value, isInclusive))

  def bounded[@sp(spNum) T](leftVal: T, leftIncl: Boolean, rightVal: T, rightIncl: Boolean): Bounded[T] =
    Bounded(Bound.Lower(leftVal, leftIncl), Bound.Upper(rightVal, rightIncl))

  case object Unbounded extends Interval[Nothing] {

    override def toString: String = "x in U"
  }

  case class RightUnbounded[+T](leftBound: Bound.Lower[T]) extends Interval[T] {

    override def toString: String = leftBound.toString
  }

  case class LeftUnbounded[+T](rightBound: Bound.Upper[T]) extends Interval[T] {

    override def toString: String = rightBound.toString
  }

  case class Bounded[+T](leftBound: Bound.Lower[T], rightBound: Bound.Upper[T]) extends Interval[T] {

    override def toString: String = s"$leftBound & $rightBound"
  }
}
