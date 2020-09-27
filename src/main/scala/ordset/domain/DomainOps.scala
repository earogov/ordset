package ordset.domain

import ordset.{Hash, Interval, IntervalBuilder, IntervalMapping, Segment}

trait DomainOps[E, D <: Domain[E]] extends Domain.Wrapper[E, D] {

  implicit val interval: IntervalBuilder[E, D]

  implicit lazy val domainHash: Hash[D] = Domain.defaultHash[E, D](DirectedOrder.defaultAscHash)

  implicit lazy val intervalHash: Hash[Interval[E, D]] = Interval.defaultHash(boundOrd, domainHash)

  implicit def intervalMappingHash[V](implicit valueHash: Hash[V]): Hash[IntervalMapping[E, D, V]] =
    IntervalMapping.defaultHash(intervalHash, valueHash)

  implicit lazy val segmentUpperOrd: Segment.UpperBoundAscOrder[E, D] = Segment.upperBoundAscOrder

  implicit lazy val segmentLowerOrd: Segment.LowerBoundAscOrder[E, D] = Segment.lowerBoundAscOrder
}

object DomainOps {

  implicit def defaultDomainOps[E, D <: Domain[E]](
    implicit domain: D): DomainOps[E, D] =
    new DefaultImpl[E, D](new IntervalBuilder.UnboundedBuilder(_), domain)

  def apply[E, D <: Domain[E]](intervalBuilderFunc: DomainOps[E, D] => IntervalBuilder[E, D])(
    implicit domain: D): DomainOps[E, D] = new DefaultImpl(intervalBuilderFunc, domain)

  class DefaultImpl[E, D <: Domain[E]](
    intervalBuilderFunc: DomainOps[E, D] => IntervalBuilder[E, D],
    override val domain: D
  ) extends DomainOps[E, D] {

    override implicit val interval: IntervalBuilder[E, D] = intervalBuilderFunc(this)
  }
}