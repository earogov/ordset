package ordset.core.domain

import ordset.Hash
import ordset.core.{Interval, IntervalBuilder, IntervalOps, IntervalRelation, SegmentT}

trait DomainOps[E, D <: Domain[E]] extends DomainLike.Wrapper[E, D] {

  implicit val interval: IntervalBuilder[E, D]

  implicit val intervalOps: IntervalOps[E, D]

  implicit lazy val domainHash: Hash[D] = new Domain.DefaultHash[E, D](DirectedOrder.defaultHash)

  implicit lazy val intervalHash: Hash[Interval[E, D]] = Interval.defaultHash(boundOrd, domainHash)

  implicit def intervalRelationHash[V](implicit valueHash: Hash[V]): Hash[IntervalRelation[E, D, V]] =
    IntervalRelation.defaultHash(intervalHash, valueHash)

  /**
   * Ascending order of segments by their upper bounds.
   *
   * All order implementations MUST provide property:
   * <tr>
   *   - upper bound of last segment has maximal value in domain 
   *   (for unbounded domain it's equivalent to plus infinity).
   * </tr>
   * <tr></tr>
   * I.e [[Segment.Last]] MUST be maximal segment according to this order.
   */
  implicit lazy val segmentUpperOrd: SegmentT.UpperBoundAscOrder[E, D] = SegmentT.upperBoundAscOrder

  /**
   * Ascending order of segments by their lower bounds.
   *
   * All order implementations MUST provide property:
   * <tr>
   *   - lower bound of first segment has minimal value in domain 
   *   (for unbounded domain it's equivalent to minus infinity). 
   * </tr>
   * <tr></tr>
   * I.e [[Segment.First]] MUST be minimal segment according to this order.
   */
  implicit lazy val segmentLowerOrd: SegmentT.LowerBoundAscOrder[E, D] = SegmentT.lowerBoundAscOrder
}

object DomainOps {

  implicit def defaultDomainOps[E, D <: Domain[E]](
    implicit domain: D
  ): DomainOps[E, D] =
    new DefaultImpl[E, D](
      new IntervalBuilder.UnboundedBuilder(_),
      new IntervalOps.UnboundedOps(_),
      domain
    )

  def apply[E, D <: Domain[E]](
    intervalBuilderFunc: DomainOps[E, D] => IntervalBuilder[E, D],
    intervalOpsFunc: DomainOps[E, D] => IntervalOps[E, D]
  )(
    implicit domain: D
  ): DomainOps[E, D] =
    new DefaultImpl(intervalBuilderFunc, intervalOpsFunc, domain)

  final class DefaultImpl[E, D <: Domain[E]](
    intervalBuilderFunc: DomainOps[E, D] => IntervalBuilder[E, D],
    intervalOpsFunc: DomainOps[E, D] => IntervalOps[E, D],
    override val domain: D
  ) extends DomainOps[E, D] {

    override implicit val interval: IntervalBuilder[E, D] = intervalBuilderFunc(this)

    override implicit val intervalOps: IntervalOps[E, D] = intervalOpsFunc(this)
  }
}