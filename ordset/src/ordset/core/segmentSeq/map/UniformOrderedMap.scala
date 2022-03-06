package ordset.core.segmentSeq.map

import ordset.core.value.ValueOps
import ordset.core.{Bound, ExtendedBound}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.{AbstractUniformSegmentSeq, SegmentSeq}
import ordset.core.segmentSeq.AbstractUniformSegmentSeq.UniformSingleSegment
import ordset.core.segmentSeq.validation.ValidatingIterable
import ordset.random.RngManager

class UniformOrderedMap[E, D[X] <: Domain[X], V] protected (
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
      mapFactory.unsafeBuild(
        ValidatingIterable.unchecked(
          List((bound.provideUpper, firstValue), (ExtendedBound.AboveAll, value))
        )
      )(
        domainOps, 
        valueOps,
        rngManager
      )
  
  protected final override def consAppended(bound: Bound[E], lastValue: V): OrderedMap[E, D, V] =
    if (valueOps.eqv(value, lastValue))
      this
    else
      mapFactory.unsafeBuild(
        ValidatingIterable.unchecked(
          List((bound.provideUpper, value), (ExtendedBound.AboveAll, lastValue))
        )
      )(
        domainOps, 
        valueOps,
        rngManager
      )
}

object UniformOrderedMap {

  def apply[E, D[X] <: Domain[X], V](
    value: V,
    mapFactory: OrderedMapFactory[E, D, V, OrderedMap[E, D, V]]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): UniformOrderedMap[E, D, V] =
    new UniformOrderedMap(value, mapFactory)

  def default[E, D[X] <: Domain[X], V](
    value: V
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): UniformOrderedMap[E, D, V] =
    apply(value, TreapOrderedMap.getFactory)

  def defaultUnit[E, D[X] <: Domain[X], V](
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): UniformOrderedMap[E, D, V] =
    apply(valueOps.unit, TreapOrderedMap.getFactory)
}
