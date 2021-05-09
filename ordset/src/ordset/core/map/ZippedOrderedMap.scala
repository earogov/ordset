package ordset.core.map

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.set.ZippedOrderedSet
import ordset.core.value.ValueOps
import ordset.core.{AbstractZippedSegmentSeq, SegmentSeq}
import ordset.random.RngManager

class ZippedOrderedMap[E, D <: Domain[E], U1, U2, V](
  override final val firstSeq: SegmentSeq[E, D, U1],
  override final val secondSeq: SegmentSeq[E, D, U2],
  final val operatorFunc: (U1, U2) => V,
  final val firstInvariantFunc: U1 => Boolean,
  final val secondInvariantFunc: U2 => Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val valueOps: ValueOps[V],
  final override val rngManager: RngManager
) extends AbstractZippedSegmentSeq[E, D, U1, U2, V]
  with OrderedMapCommons[E, D, V] {

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def operator(first: U1, second: U2): V = operatorFunc(first, second)

  @inline
  protected final override def firstInvariant(value: U1): Boolean = firstInvariantFunc(value)

  @inline
  protected final override def secondInvariant(value: U2): Boolean = secondInvariantFunc(value)

  @inline
  protected final override def isValueIncluded(value: V): Boolean = valueOps.isIncluded(value)
  
  protected final override def cons(first: SegmentSeq[E, D, U1], second: SegmentSeq[E, D, U2]): SegmentSeq[E, D, V] =
    new ZippedOrderedMap(first, second, operatorFunc, firstInvariantFunc, secondInvariantFunc)
}

object ZippedOrderedMap {

  def apply[E, D <: Domain[E], U1, U2, V](
    first: SegmentSeq[E, D, U1],
    second: SegmentSeq[E, D, U2],
    operatorFunc: (U1, U2) => V,
    firstInvariantFunc: U1 => Boolean,
    secondInvariantFunc: U2 => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V],
    rngManager: RngManager
  ): ZippedOrderedMap[E, D, U1, U2, V] =
    new ZippedOrderedMap[E, D, U1, U2, V](first, second, operatorFunc, firstInvariantFunc, secondInvariantFunc)
}
