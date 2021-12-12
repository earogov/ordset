package ordset.core.domain

import ordset.core.domain.DomainOpsComponents.*

sealed trait DomainOps[E, D <: Domain[E]] extends DomainLike.Proxy[E, D] {

  def domains: Domains[E, D]

  def intervals: Intervals[E, D]

  def intervalRelations: IntervalRelations[E, D]

  def segments: Segments[E, D]

  def validation: Validation[E, D]
}

object DomainOps {

  implicit def default[E, D <: Domain[E]](implicit domain: D): DomainOps[E, D] = domain match {
    case d: Domain.Unbounded[E] => UnboundedOps.default(d)
    // TODO: implement bounded DomainOps
    case d: Domain.Bounded[E] => ???
  }

  trait UnboundedOps[E, D <: Domain[E]] extends DomainOps[E, D] {

    override def domain: D & Domain.Unbounded[E]

    override def intervals: Intervals.Unbounded[E, D]
  }

  object UnboundedOps {

    implicit def default[E, D <: Domain[E]](domain: D & Domain.Unbounded[E]): UnboundedOps[E, D] = 
      new DefaultImpl(domain)

    class DefaultImpl[E, D <: Domain[E]](
      override val domain: D & Domain.Unbounded[E],
    ) extends UnboundedOps[E, D] {

      override val domains: Domains[E, D] = Domains.default

      override val intervals: Intervals.Unbounded[E, D] = Intervals.Unbounded.default(domain, domains.hash)

      override val intervalRelations: IntervalRelations[E, D] = IntervalRelations.default(intervals.hash)

      override val segments: Segments[E, D] = Segments.default(domain)

      override val validation: Validation[E, D] = Validation.default(domain) 
    }
  }
}