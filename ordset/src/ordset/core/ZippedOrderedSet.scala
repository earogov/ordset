package ordset.core

import ordset.Eq
import ordset.core.domain.{Domain, DomainOps}

class ZippedOrderedSet[E, D <: Domain[E]](
  override final val left: OrderedSet[E, D],
  override final val right: OrderedSet[E, D],
  final val operatorFunc: (Boolean, Boolean) => Boolean,
  final val invariantFunc: Boolean => Boolean
)(
  implicit final override val domainOps: DomainOps[E, D]
) extends AbstractZippedSegmentSeq[E, D, Boolean] {

  @inline
  override def valueEq: Eq[Boolean] = instances.Boolean.booleanHash

  @inline
  protected final override def operator(left: Boolean, right: Boolean): Boolean = operatorFunc(left, right)

  @inline
  protected final override def invariant(value: Boolean): Boolean = invariantFunc(value)

  @inline
  protected final override def isIncludedInSet(value: Boolean): Boolean = value
}

object ZippedOrderedSet {

  def union[E, D <: Domain[E]](left: OrderedSet[E, D], right: OrderedSet[E, D])(
    implicit domainOps: DomainOps[E, D]): ZippedOrderedSet[E, D] =
    new ZippedOrderedSet[E, D](left, right, _ || _, x => x)

  def intersection[E, D <: Domain[E]](left: OrderedSet[E, D], right: OrderedSet[E, D])(
    implicit domainOps: DomainOps[E, D]): ZippedOrderedSet[E, D] =
    new ZippedOrderedSet[E, D](left, right, _ && _, !_)
}