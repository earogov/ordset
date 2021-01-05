package ordset

import ordset.domain.{Domain, DomainOps}

class ZippedOrderedSet[E, D <: Domain[E]](
  override final val left: SetSegmentSeq[E, D],
  override final val right: SetSegmentSeq[E, D],
  final val operatorFunc: (Boolean, Boolean) => Boolean,
  final val invariantFunc: Boolean => Boolean
)(
  implicit override final val domainOps: DomainOps[E, D]
) extends AbstractZippedSegmentSeq[E, D, Boolean] {

  @inline
  override def valueEq: Eq[Boolean] = instances.Boolean.booleanHash

  @inline
  override protected final def operator(left: Boolean, right: Boolean): Boolean = operatorFunc(left, right)

  @inline
  override protected final def invariant(value: Boolean): Boolean = invariantFunc(value)

  @inline
  override protected final def belongsToSet(value: Boolean): Boolean = value
}

object ZippedOrderedSet {

  def union[E, D <: Domain[E]](left: SetSegmentSeq[E, D], right: SetSegmentSeq[E, D])(
    implicit domainOps: DomainOps[E, D]): ZippedOrderedSet[E, D] =
    new ZippedOrderedSet[E, D](left, right, _ || _, x => x)

  def intersection[E, D <: Domain[E]](left: SetSegmentSeq[E, D], right: SetSegmentSeq[E, D])(
    implicit domainOps: DomainOps[E, D]): ZippedOrderedSet[E, D] =
    new ZippedOrderedSet[E, D](left, right, _ && _, !_)
}