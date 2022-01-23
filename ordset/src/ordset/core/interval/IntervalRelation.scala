package ordset.core.interval

import ordset.core.SetBuilderFormat
import ordset.core.domain.Domain
import ordset.{Hash, Show, util}

import scala.{specialized => sp}

case class IntervalRelation[E, D[X] <: Domain[X], @sp(Boolean) +V](
    val interval: Interval[E, D], 
    val value: V
) {

  /**
   * Creates new interval relation with specified value.
   */
  def withValue[U](newValue: U): IntervalRelation[E, D, U] = new IntervalRelation(interval, newValue)

  /**
   * Creates new interval relation with value mapped by `mapFunc`.
   */
  def mapValue[U](mapFunc: V => U): IntervalRelation[E, D, U] = withValue(mapFunc(value))
  
  override def toString: String =
    SetBuilderFormat.intervalRelation(
      interval, value, SetBuilderFormat.toStringFunc[E], SetBuilderFormat.toStringFunc[V]
    )
}

object IntervalRelation {

  implicit def defaultHash[E, D[X] <: Domain[X], V](
    implicit 
    intervalHash: Hash[Interval[E, D]], 
    valueHash: Hash[V]
  ): DefaultHash[E, D, V] =
    new DefaultHashImpl(intervalHash, valueHash)

  implicit def defaultShow[E, D[X] <: Domain[X], V](
    implicit  
    elementShow: Show[E], 
    valueShow: Show[V]
  ): Show[IntervalRelation[E, D, V]] =
    SetBuilderFormat.intervalRelationShow(elementShow, valueShow)

  trait DefaultHash[E, D[X] <: Domain[X], V] extends Hash[IntervalRelation[E, D, V]] {

    import util.HashUtil._

    def intervalHash: Hash[Interval[E, D]]
    
    def valueHash: Hash[V]

    override def hash(x: IntervalRelation[E, D, V]): Int =
      product2Hash(intervalHash.hash(x.interval), valueHash.hash(x.value))

    override def eqv(x: IntervalRelation[E, D, V], y: IntervalRelation[E, D, V]): Boolean =
      intervalHash.eqv(x.interval, y.interval) && valueHash.eqv(x.value, y.value)
  }

  case class DefaultHashImpl[E, D[X] <: Domain[X], V](
    override val intervalHash: Hash[Interval[E, D]],
    override val valueHash: Hash[V]
  ) extends DefaultHash[E, D, V]
}