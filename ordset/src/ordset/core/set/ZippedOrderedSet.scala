package ordset.core.set

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.{AbstractZippedSegmentSeq, OrderedSet}
import ordset.random.RngManager

class ZippedOrderedSet[E, D <: Domain[E]](
  override final val left: OrderedSet[E, D],
  override final val right: OrderedSet[E, D],
  final val operatorFunc: (Boolean, Boolean) => Boolean,
  final val invariantFunc: Boolean => Boolean
)(
  implicit
  final override val domainOps: DomainOps[E, D],
  final override val rngManager: RngManager
) extends AbstractZippedSegmentSeq[E, D, Boolean]
  with OrderedSetCommons[E, D] {

  // Protected section -------------------------------------------------------- //
  @inline
  protected final override def operator(left: Boolean, right: Boolean): Boolean = operatorFunc(left, right)

  @inline
  protected final override def invariant(value: Boolean): Boolean = invariantFunc(value)

  @inline
  protected final override def isIncludedInSet(value: Boolean): Boolean = value

  protected final override def cons(left: OrderedSet[E, D], right: OrderedSet[E, D]): OrderedSet[E, D] =
    new ZippedOrderedSet(left, right, operatorFunc, invariantFunc)
}

object ZippedOrderedSet {

  def create[E, D <: Domain[E]](
    left: OrderedSet[E, D],
    right: OrderedSet[E, D],
    operatorFunc: (Boolean, Boolean) => Boolean,
    invariantFunc: Boolean => Boolean
  )(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedOrderedSet[E, D] =
    new ZippedOrderedSet[E, D](left, right, operatorFunc, invariantFunc)

  def union[E, D <: Domain[E]](left: OrderedSet[E, D], right: OrderedSet[E, D])(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedOrderedSet[E, D] =
    new ZippedOrderedSet[E, D](left, right, _ || _, x => x)

  def intersection[E, D <: Domain[E]](left: OrderedSet[E, D], right: OrderedSet[E, D])(
    implicit
    domainOps: DomainOps[E, D],
    rngManager: RngManager
  ): ZippedOrderedSet[E, D] =
    new ZippedOrderedSet[E, D](left, right, _ && _, !_)
}