package ordset.core

import ordset.core.domain.Domain
import ordset.{Hash, Show, util}

import scala.{specialized => sp}

case class IntervalRelation[E, D <: Domain[E], @sp(Boolean) +V](
    interval: Interval[E, D], 
    value: V
) {

  /**
   * Creates new interval relation with specified value.
   */
  def withValue[U](newValue: U): IntervalRelation[E, D, U] = new IntervalRelation(interval, newValue)

  def mapValue[U](mapFunc: V => U): IntervalRelation[E, D, U] = withValue(mapFunc.apply(value))
  
  override def toString: String =
    SetBuilderFormat.intervalRelation(
      interval, value, SetBuilderFormat.toStringFunc[E], SetBuilderFormat.toStringFunc[V]
    )
}

object IntervalRelation {

  implicit def defaultHash[E, D <: Domain[E], V](
    implicit intervalHash: Hash[Interval[E, D]], valueHash: Hash[V]): Hash[IntervalRelation[E, D, V]] =
    new DefaultHash()(intervalHash, valueHash)

  implicit def defaultShow[E, D <: Domain[E], V](
    implicit  elementShow: Show[E], valueShow: Show[V]): Show[IntervalRelation[E, D, V]] =
    SetBuilderFormat.intervalRelationShow(elementShow, valueShow)

  final class DefaultHash[E, D <: Domain[E], V]()(
    implicit intervalHash: Hash[Interval[E, D]],
    valueHash: Hash[V]
  ) extends Hash[IntervalRelation[E, D, V]] {

    import util.HashUtil._

    override def hash(x: IntervalRelation[E, D, V]): Int =
      product2Hash(intervalHash.hash(x.interval), valueHash.hash(x.value))

    override def eqv(x: IntervalRelation[E, D, V], y: IntervalRelation[E, D, V]): Boolean =
      intervalHash.eqv(x.interval, y.interval) && valueHash.eqv(x.value, y.value)
  }
}