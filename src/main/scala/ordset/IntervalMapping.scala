package ordset

import scala.{specialized => sp}
import scala.Specializable.{AllNumeric => spNum}

case class IntervalMapping[@sp(spNum) +E, @sp(Boolean) +V](
    interval: Interval[E], value: V) {

  override def toString: String = s"$value forAll $interval"
}

object IntervalMapping {

  def bounded[E, V](leftBound: Bound.Lower[E], rightBound: Bound.Upper[E], value: V): IntervalMapping[E, V] =
    IntervalMapping(Interval.Bounded(leftBound, rightBound), value)

  def leftUnbounded[E, V](rightBound: Bound.Upper[E], value: V): IntervalMapping[E, V] =
    IntervalMapping(Interval.LeftUnbounded(rightBound), value)

  def rightUnbounded[E, V](leftBound: Bound.Lower[E], value: V): IntervalMapping[E, V] =
    IntervalMapping(Interval.RightUnbounded(leftBound), value)

  def unbounded[V](value: V): IntervalMapping[Nothing, V] = IntervalMapping(Interval.Unbounded, value)

  implicit def defaultHash[E, V](implicit hashI: Hash[Interval[E]], hashV: Hash[V]): Hash[IntervalMapping[E, V]] =
    new DefaultHash()(hashI, hashV)

  class DefaultHash[E, V](implicit hashI: Hash[Interval[E]], hashV: Hash[V]) extends Hash[IntervalMapping[E, V]] {
    import util.Hash._

    override def hash(x: IntervalMapping[E, V]): Int = product2Hash(hashI.hash(x.interval), hashV.hash(x.value))

    override def eqv(x: IntervalMapping[E, V], y: IntervalMapping[E, V]): Boolean =
      hashI.eqv(x.interval, y.interval) && hashV.eqv(x.value, y.value)
  }
}