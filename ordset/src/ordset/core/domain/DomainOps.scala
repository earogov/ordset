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
    case d: Domain.BoundedBelow[E] => BoundedBelowOps.default(d)
    case d: Domain.BoundedAbove[E] => BoundedAboveOps.default(d)
    case d: Domain.Bounded[E] => BoundedOps.default(d)
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

  trait BoundedBelowOps[E, D <: Domain[E]] extends DomainOps[E, D] {

    override def domain: D & Domain.BoundedBelow[E]

    override def intervals: Intervals.BoundedBelow[E, D]
  }

  object BoundedBelowOps {

    implicit def default[E, D <: Domain[E]](domain: D & Domain.BoundedBelow[E]): BoundedBelowOps[E, D] = 
      new DefaultImpl(domain)

    class DefaultImpl[E, D <: Domain[E]](
      override val domain: D & Domain.BoundedBelow[E],
    ) extends BoundedBelowOps[E, D] {

      override val domains: Domains[E, D] = Domains.default

      override val intervals: Intervals.BoundedBelow[E, D] = Intervals.BoundedBelow.default(domain, domains.hash)

      override val intervalRelations: IntervalRelations[E, D] = IntervalRelations.default(intervals.hash)

      override val segments: Segments[E, D] = Segments.default(domain)

      override val validation: Validation[E, D] = Validation.default(domain) 
    }
  }

  trait BoundedAboveOps[E, D <: Domain[E]] extends DomainOps[E, D] {

    override def domain: D & Domain.BoundedAbove[E]

    override def intervals: Intervals.BoundedAbove[E, D]
  }

  object BoundedAboveOps {

    implicit def default[E, D <: Domain[E]](domain: D & Domain.BoundedAbove[E]): BoundedAboveOps[E, D] = 
      new DefaultImpl(domain)

    class DefaultImpl[E, D <: Domain[E]](
      override val domain: D & Domain.BoundedAbove[E],
    ) extends BoundedAboveOps[E, D] {

      override val domains: Domains[E, D] = Domains.default

      override val intervals: Intervals.BoundedAbove[E, D] = Intervals.BoundedAbove.default(domain, domains.hash)

      override val intervalRelations: IntervalRelations[E, D] = IntervalRelations.default(intervals.hash)

      override val segments: Segments[E, D] = Segments.default(domain)

      override val validation: Validation[E, D] = Validation.default(domain) 
    }
  }

  trait BoundedOps[E, D <: Domain[E]] extends DomainOps[E, D] {

    override def domain: D & Domain.Bounded[E]

    override def intervals: Intervals.Bounded[E, D]
  }

  object BoundedOps {

    implicit def default[E, D <: Domain[E]](domain: D & Domain.Bounded[E]): BoundedOps[E, D] = 
      new DefaultImpl(domain)

    class DefaultImpl[E, D <: Domain[E]](
      override val domain: D & Domain.Bounded[E],
    ) extends BoundedOps[E, D] {

      override val domains: Domains[E, D] = Domains.default

      override val intervals: Intervals.Bounded[E, D] = Intervals.Bounded.default(domain, domains.hash)

      override val intervalRelations: IntervalRelations[E, D] = IntervalRelations.default(intervals.hash)

      override val segments: Segments[E, D] = Segments.default(domain)

      override val validation: Validation[E, D] = Validation.default(domain) 
    }
  }
}