package ordset.core.interval

import ordset.core.Bound
import ordset.core.domain.{Domain, DomainOps}

trait IntervalBuilder[E, D <: Domain[E]] {

  def apply(bound: Bound.Upper[E]): Interval[E, D]

  def apply(bound: Bound.Lower[E]): Interval[E, D]

  def apply(leftBound: Bound.Lower[E], rightBound: Bound.Upper[E]): Interval[E, D]

  def empty: Interval[E, D]

  def universal: Interval[E, D]

  def lessThen(element: E, isInclusive: Boolean): Interval[E, D]

  def greaterThen(element: E, isInclusive: Boolean): Interval[E, D]

  def between(leftElement: E, leftIncl: Boolean, rightElement: E, rightIncl: Boolean): Interval[E, D]
}

object IntervalBuilder {

  final class UnboundedBuilder[E, D <: Domain[E]](
    val domainOps: DomainOps[E, D]
  ) extends IntervalBuilder[E, D] {

    override lazy val empty: Interval.Empty[E, D] = Interval.Empty()(domainOps)

    override lazy val universal: Interval.Universal[E, D] = Interval.Universal()(domainOps)

    override def apply(bound: Bound.Upper[E]): Interval.Less[E, D] =
      Interval.Less(bound)(domainOps)

    override def apply(bound: Bound.Lower[E]): Interval.Greater[E, D] =
      Interval.Greater(bound)(domainOps)

    override def apply(leftBound: Bound.Lower[E], rightBound: Bound.Upper[E]): Interval[E, D] =
      if (domainOps.boundOrd.lt(rightBound, leftBound)) empty
      else Interval.Between(leftBound, rightBound)(domainOps)

    override def lessThen(element: E, isInclusive: Boolean): Interval.Less[E, D] =
      apply(Bound.Upper(element, isInclusive))

    override def greaterThen(element: E, isInclusive: Boolean): Interval.Greater[E, D] =
      apply(Bound.Lower(element, isInclusive))

    override def between(leftElement: E, leftIncl: Boolean, rightElement: E, rightIncl: Boolean): Interval[E, D] =
      apply(Bound.Lower(leftElement, leftIncl), Bound.Upper(rightElement, rightIncl))
  }
}