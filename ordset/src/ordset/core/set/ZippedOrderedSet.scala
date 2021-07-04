package ordset.core.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.AbstractZippedSegmentSeq
import ordset.random.RngManager

class ZippedOrderedSet[E, D <: Domain[E], S1, S2] protected (
  override final val firstSeq: OrderedSetT[E, D, S1],
  override final val secondSeq: OrderedSetT[E, D, S2],
  final val operatorFunc: (Boolean, Boolean) => Boolean,
  final val invariantFunc: Boolean => Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractZippedSegmentSeq[E, D, Boolean, Boolean, Boolean, S1, S2]
  with OrderedSetCommons[E, D] {

  // Inspection --------------------------------------------------------------- //
  @inline
  final override def operator(first: Boolean, second: Boolean): Boolean = operatorFunc(first, second)

  @inline
  final override def firstInvariant(value: Boolean): Boolean = invariantFunc(value)

  @inline
  final override def secondInvariant(value: Boolean): Boolean = invariantFunc(value)

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def isValueIncluded(value: Boolean): Boolean = value

  protected final override def cons(first: OrderedSet[E, D], second: OrderedSet[E, D]): OrderedSet[E, D] =
    new ZippedOrderedSet(first, second, operatorFunc, invariantFunc)
}

object ZippedOrderedSet {

  def apply[E, D <: Domain[E], S1, S2](
    first: OrderedSetT[E, D, S1],
    second: OrderedSetT[E, D, S2],
    operatorFunc: (Boolean, Boolean) => Boolean,
    invariantFunc: Boolean => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedOrderedSet[E, D, S1, S2] =
    new ZippedOrderedSet(first, second, operatorFunc, invariantFunc)

  def union[E, D <: Domain[E], S1, S2](
    first: OrderedSetT[E, D, S1],
    second: OrderedSetT[E, D, S2]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedOrderedSet[E, D, S1, S2] =
    new ZippedOrderedSet(first, second, _ || _, x => x)

  def intersection[E, D <: Domain[E], S1, S2](
    first: OrderedSetT[E, D, S1],
    second: OrderedSetT[E, D, S2]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedOrderedSet[E, D, S1, S2] =
    new ZippedOrderedSet(first, second, _ && _, !_)
}