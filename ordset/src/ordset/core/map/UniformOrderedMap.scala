package ordset.core.map

import ordset.core.value.ValueOps
import ordset.core.{AbstractUniformSegmentSeq, Bound, SegmentSeq, SeqValidationPredicate}
import ordset.core.domain.{Domain, DomainOps}
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
      TreapOrderedMap.unsafeBuildAsc[E, D, V](
        List((bound.provideUpper, firstValue), (null, value)), domainOps, valueOps
      )(
        SeqValidationPredicate.alwaysTrue, SeqValidationPredicate.alwaysTrue
      )(
        rngManager
      )
  
  protected final override def consAppended(bound: Bound[E], lastValue: V): SegmentSeq[E, D, V] =
    if (valueOps.eqv(value, lastValue))
      this
    else
      TreapOrderedMap.unsafeBuildAsc[E, D, V](
        List((bound.provideUpper, value), (null, lastValue)), domainOps, valueOps
      )(
        SeqValidationPredicate.alwaysTrue, SeqValidationPredicate.alwaysTrue
      )(
        rngManager
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
