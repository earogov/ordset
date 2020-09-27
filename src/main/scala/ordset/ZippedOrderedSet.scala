package ordset

import ordset.domain.{Domain, DomainOps}

class ZippedOrderedSet[E, D <: Domain[E]](
  override val left: SetSegmentSeq[E, D],
  override val right: SetSegmentSeq[E, D],
  val operatorFunc: (Boolean, Boolean) => Boolean,
  val invariantFunc: Boolean => Boolean
)(
  implicit override val domainOps: DomainOps[E, D]
) extends AbstractZippedSegmentSeq[E, D, Boolean] {

  override final val valueEq: Eq[Boolean] = instances.Boolean.booleanHash

  override protected def operator(left: Boolean, right: Boolean): Boolean = operatorFunc(left, right)

  override protected def invariant(value: Boolean): Boolean = invariantFunc(value)

  override protected def belongsToSet(value: Boolean): Boolean = value
}

object ZippedOrderedSet {

  def union[E, D <: Domain[E]](left: SetSegmentSeq[E, D], right: SetSegmentSeq[E, D])(
    implicit domainOps: DomainOps[E, D]): ZippedOrderedSet[E, D] =
    new ZippedOrderedSet[E, D](left, right, _ || _, x => x)

  def intersection[E, D <: Domain[E]](left: SetSegmentSeq[E, D], right: SetSegmentSeq[E, D])(
    implicit domainOps: DomainOps[E, D]): ZippedOrderedSet[E, D] =
    new ZippedOrderedSet[E, D](left, right, _ && _, !_)
}