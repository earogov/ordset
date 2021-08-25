package ordset.core.map

import ordset.core.AbstractUniformSegmentSeq.UniformSingleSegment
import ordset.core.value.ValueOps
import ordset.core.{AbstractUniformSegmentSeq, Bound, ExtendedBound, SegmentSeq, SeqValidationPredicate}
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager

class UniformOrderedMap[E, D <: Domain[E], V] protected (
  final override val value: V,
  final val mapFactory: OrderedMapFactory[E, D, V, OrderedMap[E, D, V]]
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val valueOps: ValueOps[V],
  final override val rngManager: RngManager
) extends AbstractUniformSegmentSeq[E, D, V]
  with OrderedMapCommons[E, D, V, UniformSingleSegment[E, D, V]] {

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def consUniform(value: V): UniformOrderedMap[E, D, V] =
    if (valueOps.eqv(this.value, value)) this
    else UniformOrderedMap.apply(value, mapFactory)
  
  protected final override def consPrepended(bound: Bound[E], firstValue: V): OrderedMap[E, D, V] =
    if (valueOps.eqv(firstValue, value))
      this
    else
      mapFactory.unsafeBuildAsc(
        List((bound.provideUpper, firstValue), (ExtendedBound.AboveAll, value)), 
        domainOps, 
        valueOps
      )(
        SeqValidationPredicate.alwaysTrue, 
        SeqValidationPredicate.alwaysTrue
      )(
        rngManager
      )
  
  protected final override def consAppended(bound: Bound[E], lastValue: V): OrderedMap[E, D, V] =
    if (valueOps.eqv(value, lastValue))
      this
    else
      mapFactory.unsafeBuildAsc(
        List((bound.provideUpper, value), (ExtendedBound.AboveAll, lastValue)), 
        domainOps, 
        valueOps
      )(
        SeqValidationPredicate.alwaysTrue, 
        SeqValidationPredicate.alwaysTrue
      )(
        rngManager
      )
}

object UniformOrderedMap {

  def apply[E, D <: Domain[E], V](
    value: V,
    mapFactory: OrderedMapFactory[E, D, V, OrderedMap[E, D, V]]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): UniformOrderedMap[E, D, V] =
    new UniformOrderedMap(value, mapFactory)

  def default[E, D <: Domain[E], V](
    value: V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): UniformOrderedMap[E, D, V] =
    apply(value, TreapOrderedMap.getFactory)
}
