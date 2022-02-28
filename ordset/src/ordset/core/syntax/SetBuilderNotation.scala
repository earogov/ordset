package ordset.core.syntax

import ordset.core.{Bound, ExtendedBound}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.interval.{Interval, IntervalRelation}
import ordset.core.segmentSeq.validation.ValidatingIterable
import ordset.core.segmentSeq.set.OrderedSetFactoryIterable
import ordset.core.segmentSeq.map.{BoundValue, OrderedMapFactoryIterable}

import scala.Specializable.{AllNumeric => spNum}
import scala.{specialized => sp}

object SetBuilderNotation {

  trait BoundBuilder[@sp(spNum) E, D[X] <: Domain[X]] extends Any {

    def domainOps: DomainOps[E, D]

    def >(element: E): DomainBound.Lower[E, D] = DomainBound.Lower(domainOps, element, isIncluding = false)

    def >=(element: E): DomainBound.Lower[E, D] = DomainBound.Lower(domainOps, element, isIncluding = true)

    def <(element: E): DomainBound.Upper[E, D] = DomainBound.Upper(domainOps, element, isIncluding = false)

    def <=(element: E): DomainBound.Upper[E, D] = DomainBound.Upper(domainOps, element, isIncluding = true)
  }

  object BoundBuilder {

    def apply[@sp(spNum) E, D[X] <: Domain[X]](implicit domainOps: DomainOps[E, D]): BoundBuilder[E, D] =
      new DefaultImpl[E, D](domainOps)

    class DefaultImpl[@sp(spNum) E, D[X] <: Domain[X]](
      override val domainOps: DomainOps[E, D]
    ) extends BoundBuilder[E, D]
  }

  implicit def boundBuilderToUniversal[E, D[X] <: Domain[X]](builder: BoundBuilder[E, D]): Interval[E, D] =
    builder.domainOps.intervals.factory.universal

  implicit class BoundsToInterval[E, D[X] <: Domain[X]](val lower: DomainBound.Lower[E, D]) {

    def &(upper: DomainBound.Upper[E, D]): Interval[E, D] = 
      lower.domainOps.intervals.factory.betweenBounds(lower.bound, upper.bound)
  }

  implicit def upperBoundToInterval[E, D[X] <: Domain[X]](
    upper: DomainBound.Upper[E, D]
  )(
    implicit domainOps: DomainOps[E, D]
  ): Interval[E, D] =
    upper.domainOps.intervals.factory.belowBound(upper.bound)

  implicit def lowerBoundToInterval[E, D[X] <: Domain[X]](
    lower: DomainBound.Lower[E, D]
  )(
    implicit domainOps: DomainOps[E, D]
  ): Interval[E, D] =
    lower.domainOps.intervals.factory.aboveBound(lower.bound)

  implicit def setValidatingIterable[E, D[X] <: Domain[X]](
    bounds: Iterable[Bound.Upper[E]]
  )(
    implicit domainOps: DomainOps[E, D]
  ): ValidatingIterable[Bound.Upper[E]] =
    OrderedSetFactoryIterable.default(bounds)(domainOps)

  implicit def mapValidatingIterable[E, D[X] <: Domain[X], V](
    boundValues: Iterable[BoundValue[E, V]]
  )(
    implicit 
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  ): ValidatingIterable[BoundValue[E, V]] =
    OrderedMapFactoryIterable.default(boundValues)(domainOps, valueOps)

  implicit class ValueToIntervalRelation[@sp(Boolean) +V](val value: V) {

    def forAll[E, D[X] <: Domain[X]](interval: Interval[E, D]): IntervalRelation[E, D, V] =
      IntervalRelation(interval, value)
  }
}
