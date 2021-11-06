package ordset.core.interval

import ordset.core.{Bound, ExtendedBound}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.ExtendedBound.BelowAll

trait IntervalBuilder[E, D <: Domain[E]] {

  def domain: D

  def empty: Interval[E, D]

  def universal: Interval[E, D]

  def belowElement(element: E, isInclusive: Boolean): Interval[E, D]

  def aboveElement(element: E, isInclusive: Boolean): Interval[E, D]

  def betweenElements(leftElement: E, leftIncl: Boolean, rightElement: E, rightIncl: Boolean): Interval[E, D]

  def belowBound(bound: Bound.Upper[E]): Interval[E, D]

  def aboveBound(bound: Bound.Lower[E]): Interval[E, D]

  def betweenBounds(leftBound: Bound.Lower[E], rightBound: Bound.Upper[E]): Interval[E, D]

  def belowExtended(bound: ExtendedBound.Upper[E]): Interval[E, D] = bound match {
    case b: Bound.Upper[E] => belowBound(b)
    case _ => universal // bound is `ExtendedBound.AboveAll`
  }

  def aboveExtended(bound: ExtendedBound.Lower[E]): Interval[E, D] = bound match {
    case b: Bound.Lower[E] => aboveBound(b)
    case _ => universal // bound is `ExtendedBound.BelowAll`
  }

  def betweenExtended(leftBound: ExtendedBound.Lower[E], rightBound: ExtendedBound.Upper[E]): Interval[E, D] = 
    (leftBound, rightBound) match {
      case (l: Bound.Lower[E], u: Bound.Upper[E]) => betweenBounds(l, u)
      case (_, u: Bound.Upper[E]) => belowBound(u) // lower bound is `ExtendedBound.BelowAll`
      case (l: Bound.Lower[E], _) => aboveBound(l) // upper bound is `ExtendedBound.AboveAll`
      case _ => universal                          // bounds are `ExtendedBound.BelowAll` and `ExtendedBound.AboveAll`
    }
}

object IntervalBuilder {

  implicit def defaultBuilder[E, D <: Domain[E]](implicit domain: D): IntervalBuilder[E, D] =
    domain match {
      case d: Domain.Unbounded[E] => new UnboundedBuilder(d)
      // TODO: implement bounded builder
      case d: Domain.Bounded[E] => ???
    }

  final class UnboundedBuilder[E, D <: Domain[E]](
    override val domain: D & Domain.Unbounded[E]
  ) extends IntervalBuilder[E, D] {

    override lazy val empty: Interval.Empty[E, D] = Interval.Empty()(domain)

    override lazy val universal: Interval.Universal[E, D] = Interval.Universal()(domain)

    override def belowElement(element: E, isInclusive: Boolean): Interval.Less[E, D] =
      belowBound(Bound.Upper(element, isInclusive))

    override def aboveElement(element: E, isInclusive: Boolean): Interval.Greater[E, D] =
      aboveBound(Bound.Lower(element, isInclusive))

    override def betweenElements(leftElement: E, leftIncl: Boolean, rightElement: E, rightIncl: Boolean): Interval[E, D] =
      betweenBounds(Bound.Lower(leftElement, leftIncl), Bound.Upper(rightElement, rightIncl))

    override def belowBound(bound: Bound.Upper[E]): Interval.Less[E, D] = Interval.Less(bound)(domain)

    override def aboveBound(bound: Bound.Lower[E]): Interval.Greater[E, D] = Interval.Greater(bound)(domain)

    override def betweenBounds(leftBound: Bound.Lower[E], rightBound: Bound.Upper[E]): Interval[E, D] =
      if (domain.boundOrd.lt(rightBound, leftBound)) empty
      else Interval.Between(leftBound, rightBound)(domain)
  }
}