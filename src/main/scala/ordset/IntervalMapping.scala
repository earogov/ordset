package ordset

import ordset.domain.Domain

import scala.{specialized => sp}

case class IntervalMapping[E, D <: Domain[E], @sp(Boolean) +V](
    interval: Interval[E, D], value: V) {

  override def toString: String = s"$value forAll $interval"
}

object IntervalMapping {

  implicit def defaultHash[E, D <: Domain[E], V](
    implicit intervalHash: Hash[Interval[E, D]], valueHash: Hash[V]): Hash[IntervalMapping[E, D, V]] =
    new DefaultHash()(intervalHash, valueHash)

  class DefaultHash[E, D <: Domain[E], V]()(
    implicit intervalHash: Hash[Interval[E, D]], valueHash: Hash[V]
  ) extends Hash[IntervalMapping[E, D, V]] {

    import util.Hash._

    override def hash(x: IntervalMapping[E, D, V]): Int =
      product2Hash(intervalHash.hash(x.interval), valueHash.hash(x.value))

    override def eqv(x: IntervalMapping[E, D, V], y: IntervalMapping[E, D, V]): Boolean =
      intervalHash.eqv(x.interval, y.interval) && valueHash.eqv(x.value, y.value)
  }
}