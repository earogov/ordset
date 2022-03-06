package ordset.core.domain

import ordset.{Hash, Show}
import ordset.core.{Bound, ExtendedBound}
import ordset.core.segmentSeq.{SegmentT, Segment, SegmentSeqT, SegmentSeq}
import ordset.core.interval.{Interval, IntervalRelation, IntervalAlgebra, IntervalFactory}
import ordset.core.range.Range

object DomainOpsComponents {

  trait Intervals[E, D[X] <: Domain[X]] {

    implicit val hash: Hash[Interval[E, D]]

    implicit val alg: IntervalAlgebra[E, D]

    implicit val factory: IntervalFactory[E, D]

    def bounds: Interval.NonEmpty[E, D] = factory.universal
  }

  object Intervals {

    trait Unbounded[E, D[X] <: Domain[X]] extends Intervals[E, D] {

      override implicit val factory: IntervalFactory.UnboundedFactory[E, D]

      override def bounds: Interval.Unbounded[E, D] = factory.universal
    }

    object Unbounded {

      def default[E, D[X] <: Domain[X]](
        domain: D[E] & Domain.Unbounded[E]
      ): Unbounded[E, D] =
        new DefaultImpl(domain)

      class DefaultImpl[E, D[X] <: Domain[X]](
        domain: D[E] & Domain.Unbounded[E]
      ) extends Unbounded[E, D] {

        override implicit val hash: Hash[Interval[E, D]] = Interval.defaultHash(domain.boundOrd)

        override implicit val alg: IntervalAlgebra[E, D] = IntervalAlgebra.defaultAlgebra(domain)

        override implicit val factory: IntervalFactory.UnboundedFactory[E, D] = IntervalFactory.UnboundedFactory(domain)
      }
    }

    trait BoundedBelow[E, D[X] <: Domain[X]] extends Intervals[E, D] {

      override implicit val factory: IntervalFactory.BoundedBelowFactory[E, D]

      override def bounds: Interval.Greater[E, D] = factory.universal
    }

    object BoundedBelow {

      def default[E, D[X] <: Domain[X]](
        domain: D[E] & Domain.BoundedBelow[E]
      ): BoundedBelow[E, D] =
        new DefaultImpl(domain)

      class DefaultImpl[E, D[X] <: Domain[X]](
        domain: D[E] & Domain.BoundedBelow[E]
      ) extends BoundedBelow[E, D] {

        override implicit val hash: Hash[Interval[E, D]] = Interval.defaultHash(domain.boundOrd)

        override implicit val alg: IntervalAlgebra[E, D] = IntervalAlgebra.defaultAlgebra(domain)

        override implicit val factory: IntervalFactory.BoundedBelowFactory[E, D] = 
          IntervalFactory.BoundedBelowFactory(domain)
      }
    }

    trait BoundedAbove[E, D[X] <: Domain[X]] extends Intervals[E, D] {

      override implicit val factory: IntervalFactory.BoundedAboveFactory[E, D]

      override def bounds: Interval.Less[E, D] = factory.universal
    }

    object BoundedAbove {

      def default[E, D[X] <: Domain[X]](
        domain: D[E] & Domain.BoundedAbove[E]
      ): BoundedAbove[E, D] =
        new DefaultImpl(domain)

      class DefaultImpl[E, D[X] <: Domain[X]](
        domain: D[E] & Domain.BoundedAbove[E]
      ) extends BoundedAbove[E, D] {

        override implicit val hash: Hash[Interval[E, D]] = Interval.defaultHash(domain.boundOrd)

        override implicit val alg: IntervalAlgebra[E, D] = IntervalAlgebra.defaultAlgebra(domain)

        override implicit val factory: IntervalFactory.BoundedAboveFactory[E, D] = 
          IntervalFactory.BoundedAboveFactory(domain)
      }
    }

    trait Bounded[E, D[X] <: Domain[X]] extends Intervals[E, D] {

      override implicit val factory: IntervalFactory.BoundedFactory[E, D]

      override def bounds: Interval.Between[E, D] = factory.universal
    }

    object Bounded {

      def default[E, D[X] <: Domain[X]](
        domain: D[E] & Domain.Bounded[E]
      ): Bounded[E, D] =
        new DefaultImpl(domain)

      class DefaultImpl[E, D[X] <: Domain[X]](
        domain: D[E] & Domain.Bounded[E]
      ) extends Bounded[E, D] {

        override implicit val hash: Hash[Interval[E, D]] = Interval.defaultHash(domain.boundOrd)

        override implicit val alg: IntervalAlgebra[E, D] = IntervalAlgebra.defaultAlgebra(domain)

        override implicit val factory: IntervalFactory.BoundedFactory[E, D] = IntervalFactory.BoundedFactory(domain)
      }
    }
  }

  trait IntervalRelations[E, D[X] <: Domain[X]] {

    implicit def hash[V](implicit valueHash: Hash[V]): Hash[IntervalRelation[E, D, V]]
  }

  object IntervalRelations {

    def default[E, D[X] <: Domain[X]](intervalHash: Hash[Interval[E, D]]): IntervalRelations[E, D] =
      new DefaultImpl(intervalHash)

    class DefaultImpl[E, D[X] <: Domain[X]](
      intervalHash: Hash[Interval[E, D]]
    ) extends IntervalRelations[E, D] {

      override implicit def hash[V](implicit valueHash: Hash[V]): Hash[IntervalRelation[E, D, V]] = 
        IntervalRelation.defaultHash(intervalHash, valueHash)
    }
  }

  trait Segments[E, D[X] <: Domain[X]] {

    implicit val upperOrd: SegmentT.UpperBoundOrder[E, D]

    implicit val lowerOrd: SegmentT.LowerBoundOrder[E, D]
  }

  object Segments {

    def default[E, D[X] <: Domain[X]](domain: D[E]): Segments[E, D] = new DefaultImpl(domain)

    class DefaultImpl[E, D[X] <: Domain[X]](
      domain: D[E]
    ) extends Segments[E, D] {

      override implicit val upperOrd: SegmentT.UpperBoundOrder[E, D] = SegmentT.upperBoundOrder(domain)

      override implicit val lowerOrd: SegmentT.LowerBoundOrder[E, D] = SegmentT.lowerBoundOrder(domain)
    }
  }

  trait ShowOps[E, D[X] <: Domain[X]] {

    def elementShow: Show[E]

    def boundShow: Show[Bound[E]]

    def extendedShow: Show[ExtendedBound[E]]

    def domainShow: Show[D[E]]

    def rangeShow: Show[Range[ExtendedBound[E]]]

    def intervalShow: Show[Interval[E, D]]

    def intervalRelationShow[V](implicit valueShow: Show[V]): Show[IntervalRelation[E, D, V]]

    def segmentShow[V](implicit valueShow: Show[V]): Show[Segment[E, D, V]]

    def segmentSeqShow[V](implicit valueShow: Show[V]): Show[SegmentSeq[E, D, V]]
  }

  object ShowOps {

    implicit def default[E, D[X] <: Domain[X]](elementShow: Show[E]): ShowOps[E, D] = 
      new DefaultImpl(elementShow) 

    class DefaultImpl[E, D[X] <: Domain[X]](
      override val elementShow: Show[E]
    ) extends ShowOps[E, D] {

      override val boundShow: Show[Bound[E]] = Bound.defaultShow(elementShow)

      override val extendedShow: Show[ExtendedBound[E]] = ExtendedBound.defaultShow(elementShow)

      override val domainShow: Show[D[E]] = Domain.defaultShow(extendedShow)

      override val rangeShow: Show[Range[ExtendedBound[E]]] = Range.defaultShow(extendedShow)

      override val intervalShow: Show[Interval[E, D]] = Interval.defaultShow(elementShow)

      override def intervalRelationShow[V](implicit valueShow: Show[V]): Show[IntervalRelation[E, D, V]] =
        IntervalRelation.defaultShow(elementShow, valueShow)

      override def segmentShow[V](implicit valueShow: Show[V]): Show[Segment[E, D, V]] =
        SegmentT.defaultShow(elementShow, valueShow)

      override def segmentSeqShow[V](implicit valueShow: Show[V]): Show[SegmentSeq[E, D, V]] =
        SegmentSeqT.defaultShow(elementShow, valueShow)
    }
  }
}