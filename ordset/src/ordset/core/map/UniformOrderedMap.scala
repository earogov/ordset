package ordset.core.map

import ordset.core.value.ValueOps
import ordset.core.{AbstractUniformSegmentSeq, Bound, SegmentSeq}
import ordset.core.domain.{Domain, DomainOps, OrderValidationFunc}
import ordset.random.RngManager

class UniformOrderedMap[E, D <: Domain[E], V](
  final override val value: V
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val valueOps: ValueOps[V],
  final override val rngManager: RngManager
) extends AbstractUniformSegmentSeq[E, D, V] 
  with OrderedMapCommons[E, D, V] {

  // Protected section -------------------------------------------------------- //  
  @inline
  protected final override def isValueIncluded(value: V): Boolean = valueOps.isIncluded(value)

  protected final override def consPrepended(bound: Bound[E], firstValue: V): SegmentSeq[E, D, V] =
    if (valueOps.eqv(firstValue, value))
      this
    else
      TreapOrderedMap.fromIterableUnsafe(
        List(bound.provideUpper), List(firstValue, value), domainOps
      )(
        OrderValidationFunc.alwaysTrue
      )(
        valueOps, rngManager
      )
  
  protected final override def consAppended(bound: Bound[E], lastValue: V): SegmentSeq[E, D, V] =
    if (valueOps.eqv(value, lastValue))
      this
    else
      TreapOrderedMap.fromIterableUnsafe(
        List(bound.provideUpper), List(value, lastValue), domainOps
      )(
        OrderValidationFunc.alwaysTrue
      )(
        valueOps, rngManager
      )
}

object UniformOrderedMap {

  def apply[E, D <: Domain[E], V](
    value: V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): UniformOrderedMap[E, D, V] =
    new UniformOrderedMap[E, D, V](value)
}
