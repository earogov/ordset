package ordset.core.domain

import ordset.Show
import ordset.core.domain.DomainOpsComponents.*

sealed trait DomainOps[E, D[X] <: Domain[X]] extends DomainLike.Proxy[E, D] {

  def domains: Domains[E, D]

  def intervals: Intervals[E, D]

  def intervalRelations: IntervalRelations[E, D]

  def segments: Segments[E, D]

  def validation: Validation[E, D]

  def showOps: ShowOps[E, D]
}

object DomainOps {

  implicit def default[E, D[X] <: Domain[X]](
    implicit 
    domain: D[E],
    elementShow: Show[E] = Show.fromToString[E]
  ): DomainOps[E, D] = 
    domain match {
      case d: Domain.Unbounded[E] => UnboundedOps.default(d, elementShow)
      case d: Domain.BoundedBelow[E] => BoundedBelowOps.default(d, elementShow)
      case d: Domain.BoundedAbove[E] => BoundedAboveOps.default(d, elementShow)
      case d: Domain.Bounded[E] => BoundedOps.default(d, elementShow)
    }

  trait UnboundedOps[E, D[X] <: Domain[X]] extends DomainOps[E, D] {

    override def domain: D[E] & Domain.Unbounded[E]

    override def intervals: Intervals.Unbounded[E, D]
  }

  object UnboundedOps {

    implicit def default[E, D[X] <: Domain[X]](
      implicit 
      domain: D[E] & Domain.Unbounded[E],
      elementShow: Show[E] = Show.fromToString[E]
    ): UnboundedOps[E, D] = 
      new DefaultImpl(domain, elementShow)

    class DefaultImpl[E, D[X] <: Domain[X]](
      override val domain: D[E] & Domain.Unbounded[E],
      elementShow: Show[E]
    ) extends UnboundedOps[E, D] {

      override val domains: Domains[E, D] = Domains.default

      override val intervals: Intervals.Unbounded[E, D] = Intervals.Unbounded.default(domain, domains.hash)

      override val intervalRelations: IntervalRelations[E, D] = IntervalRelations.default(intervals.hash)

      override val segments: Segments[E, D] = Segments.default(domain)

      override val validation: Validation[E, D] = Validation.default(domain)

      override lazy val showOps: ShowOps[E, D] = ShowOps.default(elementShow)
    }
  }

  trait BoundedBelowOps[E, D[X] <: Domain[X]] extends DomainOps[E, D] {

    override def domain: D[E] & Domain.BoundedBelow[E]

    override def intervals: Intervals.BoundedBelow[E, D]
  }

  object BoundedBelowOps {

    implicit def default[E, D[X] <: Domain[X]](
      implicit 
      domain: D[E] & Domain.BoundedBelow[E],
      elementShow: Show[E] = Show.fromToString[E]
    ): BoundedBelowOps[E, D] = 
      new DefaultImpl(domain, elementShow)

    class DefaultImpl[E, D[X] <: Domain[X]](
      override val domain: D[E] & Domain.BoundedBelow[E],
      elementShow: Show[E]
    ) extends BoundedBelowOps[E, D] {

      override val domains: Domains[E, D] = Domains.default

      override val intervals: Intervals.BoundedBelow[E, D] = Intervals.BoundedBelow.default(domain, domains.hash)

      override val intervalRelations: IntervalRelations[E, D] = IntervalRelations.default(intervals.hash)

      override val segments: Segments[E, D] = Segments.default(domain)

      override val validation: Validation[E, D] = Validation.default(domain)

      override lazy val showOps: ShowOps[E, D] = ShowOps.default(elementShow)
    }
  }

  trait BoundedAboveOps[E, D[X] <: Domain[X]] extends DomainOps[E, D] {

    override def domain: D[E] & Domain.BoundedAbove[E]

    override def intervals: Intervals.BoundedAbove[E, D]
  }

  object BoundedAboveOps {

    implicit def default[E, D[X] <: Domain[X]](
      implicit 
      domain: D[E] & Domain.BoundedAbove[E],
      elementShow: Show[E] = Show.fromToString[E]
    ): BoundedAboveOps[E, D] = 
      new DefaultImpl(domain, elementShow)

    class DefaultImpl[E, D[X] <: Domain[X]](
      override val domain: D[E] & Domain.BoundedAbove[E],
      elementShow: Show[E]
    ) extends BoundedAboveOps[E, D] {

      override val domains: Domains[E, D] = Domains.default

      override val intervals: Intervals.BoundedAbove[E, D] = Intervals.BoundedAbove.default(domain, domains.hash)

      override val intervalRelations: IntervalRelations[E, D] = IntervalRelations.default(intervals.hash)

      override val segments: Segments[E, D] = Segments.default(domain)

      override val validation: Validation[E, D] = Validation.default(domain)

      override lazy val showOps: ShowOps[E, D] = ShowOps.default(elementShow)
    }
  }

  trait BoundedOps[E, D[X] <: Domain[X]] extends DomainOps[E, D] {

    override def domain: D[E] & Domain.Bounded[E]

    override def intervals: Intervals.Bounded[E, D]
  }

  object BoundedOps {

    implicit def default[E, D[X] <: Domain[X]](
      implicit 
      domain: D[E] & Domain.Bounded[E],
      elementShow: Show[E] = Show.fromToString[E]
    ): BoundedOps[E, D] = 
      new DefaultImpl(domain, elementShow)

    class DefaultImpl[E, D[X] <: Domain[X]](
      override val domain: D[E] & Domain.Bounded[E],
      elementShow: Show[E]
    ) extends BoundedOps[E, D] {

      override val domains: Domains[E, D] = Domains.default

      override val intervals: Intervals.Bounded[E, D] = Intervals.Bounded.default(domain, domains.hash)

      override val intervalRelations: IntervalRelations[E, D] = IntervalRelations.default(intervals.hash)

      override val segments: Segments[E, D] = Segments.default(domain)

      override val validation: Validation[E, D] = Validation.default(domain)

      override lazy val showOps: ShowOps[E, D] = ShowOps.default(elementShow)
    }
  }
}