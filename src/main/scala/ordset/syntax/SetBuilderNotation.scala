package ordset.syntax

import ordset.domain.{Domain, DomainOps}
import ordset.{Interval, IntervalMapping}

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

object SetBuilderNotation {

  trait BoundBuilder[@sp(spNum) E, D <: Domain[E]] extends Any {

    def domainOps: DomainOps[E, D]

    def >(value: E): DomainBound.Lower[E, D] = DomainBound.Lower(domainOps, value, isInclusive = false)

    def >=(value: E): DomainBound.Lower[E, D] = DomainBound.Lower(domainOps, value, isInclusive = true)

    def <(value: E): DomainBound.Upper[E, D] = DomainBound.Upper(domainOps, value, isInclusive = false)

    def <=(value: E): DomainBound.Upper[E, D] = DomainBound.Upper(domainOps, value, isInclusive = true)
  }

  object BoundBuilder {

    def apply[E, D <: Domain[E]](implicit domainOps: DomainOps[E, D]): BoundBuilder[E, D] =
      new DefaultImpl[E, D](domainOps)

    class DefaultImpl[E, D <: Domain[E]](override val domainOps: DomainOps[E, D]) extends BoundBuilder[E, D]
  }

  implicit def boundBuilderToUniversal[E, D <: Domain[E]](builder: BoundBuilder[E, D]): Interval[E, D] =
    builder.domainOps.interval.universal

  implicit class BoundsToInterval[@sp(spNum) E, D <: Domain[E]](val lower: DomainBound.Lower[E, D]) {

    def &(upper: DomainBound.Upper[E, D]): Interval[E, D] = lower.domainOps.interval(lower.bound, upper.bound)
  }

  implicit def upperBoundToInterval[@sp(spNum) E, D <: Domain[E]](upper: DomainBound.Upper[E, D])(
    implicit domainOps: DomainOps[E, D]): Interval[E, D] =
    upper.domainOps.interval(upper.bound)

  implicit def lowerBoundToInterval[@sp(spNum) E, D <: Domain[E]](lower: DomainBound.Lower[E, D])(
    implicit domainOps: DomainOps[E, D]): Interval[E, D] =
    lower.domainOps.interval(lower.bound)

  implicit class ValueToIntervalMapping[@sp(Boolean) +V](val value: V) {

    def forAll[@sp(spNum) E, D <: Domain[E]](interval: Interval[E, D]): IntervalMapping[E, D, V] =
      IntervalMapping(interval, value)
  }
}
