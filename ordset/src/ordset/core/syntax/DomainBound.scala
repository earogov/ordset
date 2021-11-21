package ordset.core.syntax

import ordset.core.Bound
import ordset.core.domain.{Domain, DomainOps}


trait DomainBound[E, D <: Domain[E]] {

  def domainOps: DomainOps[E, D]

  def bound: Bound[E]
}

object DomainBound {

  implicit def lowerToBound[E, D <: Domain[E]](lower: DomainBound.Lower[E, D]): Bound.Lower[E] = lower.bound

  implicit def upperToBound[E, D <: Domain[E]](upper: DomainBound.Upper[E, D]): Bound.Upper[E] = upper.bound

  implicit def boundToBound[E, D <: Domain[E]](bound: DomainBound[E, D]): Bound[E] = bound.bound

  case class Lower[E, D <: Domain[E]](
    override val domainOps: DomainOps[E, D],
    override val bound: Bound.Lower[E]
  ) extends DomainBound[E, D]

  object Lower {

    def apply[E, D <: Domain[E]](domainOps: DomainOps[E, D], element: E, isIncluding: Boolean): Lower[E, D] =
      new Lower(domainOps, Bound.Lower(element, isIncluding))
  }

  case class Upper[E, D <: Domain[E]](
    override val domainOps: DomainOps[E, D],
    override val bound: Bound.Upper[E]
  ) extends DomainBound[E, D]

  object Upper {

    def apply[E, D <: Domain[E]](domainOps: DomainOps[E, D], element: E, isIncluding: Boolean): Upper[E, D] =
      new Upper(domainOps, Bound.Upper(element, isIncluding))
  }
}