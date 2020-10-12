package ordset.domain

import ordset.util.label.Label
import ordset.{Hash, Interval, IntervalBuilder, IntervalMapping, IntervalOps, Segment}

trait DomainOps[E, D <: Domain[E]] extends DomainLike.Wrapper[E, D] {

  implicit val interval: IntervalBuilder[E, D]

  implicit val intervalOps: IntervalOps[E, D]

  implicit lazy val domainHash: Hash[D] = new Domain.DefaultHash[E, D](Label.defaultOrder, DirectedOrder.defaultHash)

  implicit lazy val intervalHash: Hash[Interval[E, D]] = Interval.defaultHash(boundOrd, domainHash)

  implicit def intervalMappingHash[V](implicit valueHash: Hash[V]): Hash[IntervalMapping[E, D, V]] =
    IntervalMapping.defaultHash(intervalHash, valueHash)

  implicit lazy val segmentUpperOrd: Segment.UpperBoundAscOrder[E, D] = Segment.upperBoundAscOrder

  implicit lazy val segmentLowerOrd: Segment.LowerBoundAscOrder[E, D] = Segment.lowerBoundAscOrder
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