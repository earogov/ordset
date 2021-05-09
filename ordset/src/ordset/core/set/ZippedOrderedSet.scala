package ordset.core.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.{AbstractZippedSegmentSeq, OrderedSet}
import ordset.random.RngManager

class ZippedOrderedSet[E, D <: Domain[E]](
  override final val firstSeq: OrderedSet[E, D],
  override final val secondSeq: OrderedSet[E, D],
  final val operatorFunc: (Boolean, Boolean) => Boolean,
  final val invariantFunc: Boolean => Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractZippedSegmentSeq[E, D, Boolean, Boolean, Boolean]
  with OrderedSetCommons[E, D] {

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def operator(first: Boolean, second: Boolean): Boolean = operatorFunc(first, second)

  @inline
  protected final override def firstInvariant(value: Boolean): Boolean = invariantFunc(value)

  @inline
  protected final override def secondInvariant(value: Boolean): Boolean = invariantFunc(value)

  @inline
  protected final override def isValueIncluded(value: Boolean): Boolean = value

  protected final override def cons(first: OrderedSet[E, D], second: OrderedSet[E, D]): OrderedSet[E, D] =
    new ZippedOrderedSet(first, second, operatorFunc, invariantFunc)
}

object ZippedOrderedSet {

  def apply[E, D <: Domain[E]](
    first: OrderedSet[E, D],
    second: OrderedSet[E, D],
    operatorFunc: (Boolean, Boolean) => Boolean,
    invariantFunc: Boolean => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedOrderedSet[E, D] =
    new ZippedOrderedSet[E, D](first, second, operatorFunc, invariantFunc)

  def union[E, D <: Domain[E]](
    first: OrderedSet[E, D],
    second: OrderedSet[E, D]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedOrderedSet[E, D] =
    new ZippedOrderedSet[E, D](first, second, _ || _, x => x)

  def intersection[E, D <: Domain[E]](
    first: OrderedSet[E, D],
    second: OrderedSet[E, D]
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedOrderedSet[E, D] =
    new ZippedOrderedSet[E, D](first, second, _ && _, !_)
}