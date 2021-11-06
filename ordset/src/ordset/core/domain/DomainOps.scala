package ordset.core.domain

import ordset.Hash
import ordset.core.interval.{Interval, IntervalBuilder, IntervalOps, IntervalRelation}
import ordset.core.SegmentT
import scala.util.Try

trait DomainOps[E, D <: Domain[E]] extends DomainLike.Wrapper[E, D] {

  def domainBounds: Interval.NonEmpty[E, D]

  implicit def domainHash: Hash[D]

  implicit def interval: IntervalBuilder[E, D]

  implicit def intervalOps: IntervalOps[E, D]

  implicit def intervalHash: Hash[Interval[E, D]]

  implicit def intervalRelationHash[V](implicit valueHash: Hash[V]): Hash[IntervalRelation[E, D, V]]

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
  implicit def segmentUpperOrd: SegmentT.UpperBoundAscOrder[E, D]

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
  implicit def segmentLowerOrd: SegmentT.LowerBoundAscOrder[E, D]
}

object DomainOps {

  implicit def defaultDomainOps[E, D <: Domain.UnboundedContinuous[E]](
    implicit 
    domain: D,
    domainHash: Hash[D]
  ): DomainOps[E, D] = 
    new DefaultUnboundedImpl(domain, domainHash)

  def tryDefault[E, D <: Domain[E]](implicit domain: D, domainHash: Hash[D]): Try[DomainOps[E, D]] = {
    val intervalBuilder = IntervalBuilder.defaultBuilder(domain)
    val intervalOps = IntervalOps.defaultOps(domain, intervalBuilder)
    DomainValidation.tryBuildBounds(domain, intervalBuilder).map { bounds =>
      DefaultImpl(domain, bounds, domainHash, intervalBuilder, intervalOps)
    }
  }

  trait Unbounded[E, D <: Domain.Unbounded[E]] extends DomainOps[E, D] {

    override def domainBounds: Interval.Universal[E, D]
  }

  // Private section ---------------------------------------------------------- //
  private final case class DefaultImpl[E, D <: Domain[E]](
    override val domain: D,
    override val domainBounds: Interval.NonEmpty[E, D],
    override val domainHash: Hash[D],
    override val interval: IntervalBuilder[E, D],
    override val intervalOps: IntervalOps[E, D],
  ) extends DomainOps[E, D] {

    override implicit val intervalHash: Hash[Interval[E, D]] = Interval.defaultHash(boundOrd, domainHash)

    override implicit def intervalRelationHash[V](implicit valueHash: Hash[V]): Hash[IntervalRelation[E, D, V]] =
      IntervalRelation.defaultHash(intervalHash, valueHash)

    override implicit val segmentUpperOrd: SegmentT.UpperBoundAscOrder[E, D] = SegmentT.upperBoundAscOrder

    override implicit val segmentLowerOrd: SegmentT.LowerBoundAscOrder[E, D] = SegmentT.lowerBoundAscOrder
  }

  private final case class DefaultUnboundedImpl[E, D <: Domain.Unbounded[E]](
    override val domain: D,
    override val domainHash: Hash[D]
  ) extends DomainOps.Unbounded[E, D] {

    override implicit val intervalHash: Hash[Interval[E, D]] = Interval.defaultHash(boundOrd, domainHash)

    override implicit val interval: IntervalBuilder.UnboundedBuilder[E, D] = IntervalBuilder.UnboundedBuilder(domain)
    
    override implicit val intervalOps: IntervalOps.UnboundedOps[E, D] = IntervalOps.UnboundedOps(domain, interval)

    override implicit def intervalRelationHash[V](implicit valueHash: Hash[V]): Hash[IntervalRelation[E, D, V]] =
      IntervalRelation.defaultHash(intervalHash, valueHash)

    override implicit val segmentUpperOrd: SegmentT.UpperBoundAscOrder[E, D] = SegmentT.upperBoundAscOrder

    override implicit val segmentLowerOrd: SegmentT.LowerBoundAscOrder[E, D] = SegmentT.lowerBoundAscOrder

    override val domainBounds: Interval.Universal[E, D] = interval.universal
  }
}