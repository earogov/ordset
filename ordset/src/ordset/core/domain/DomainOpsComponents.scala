package ordset.core.domain

import ordset.Hash
import ordset.core.interval.{Interval, IntervalRelation, IntervalAlgebra, IntervalBuilder}
import ordset.core.{Bound, ExtendedBound, SegmentT, SeqValidationPredicate}

object DomainOpsComponents {

  trait Domains[E, D <: Domain[E]] {

    implicit val hash: Hash[D]
  }

  object Domains {

    def default[E, D <: Domain[E]]: Domains[E, D] = new DefaultImpl

    class DefaultImpl[E, D <: Domain[E]] extends Domains[E, D] {

      override implicit val hash: Hash[D] = Domain.defaultHash
    }
  }

  trait Intervals[E, D <: Domain[E]] {

    implicit val hash: Hash[Interval[E, D]]

    implicit val alg: IntervalAlgebra[E, D]

    implicit val builder: IntervalBuilder[E, D]

    def bounds: Interval.NonEmpty[E, D] = builder.universal
  }

  object Intervals {

    trait Unbounded[E, D <: Domain[E]] extends Intervals[E, D] {

      override implicit val hash: Hash[Interval[E, D]]

      override implicit val alg: IntervalAlgebra.UnboundedAlgebra[E, D]

      override implicit val builder: IntervalBuilder.UnboundedBuilder[E, D]

      override def bounds: Interval.Universal[E, D] = builder.universal
    }

    object Unbounded {

      def default[E, D <: Domain[E]](domain: D & Domain.Unbounded[E], domainHash: Hash[D]): Unbounded[E, D] =
        new DefaultImpl(domain, domainHash)

      class DefaultImpl[E, D <: Domain[E]](
        domain: D & Domain.Unbounded[E],
        domainHash: Hash[D]
      ) extends Unbounded[E, D] {

        override implicit val hash: Hash[Interval[E, D]] = Interval.defaultHash(domain.boundOrd, domainHash)

        override implicit val alg: IntervalAlgebra.UnboundedAlgebra[E, D] = IntervalAlgebra.unboundedAlgebra(domain)

        override implicit val builder: IntervalBuilder.UnboundedBuilder[E, D] = IntervalBuilder.UnboundedBuilder(domain)
      }
    }
  }

  trait IntervalRelations[E, D <: Domain[E]] {

    implicit def hash[V](implicit valueHash: Hash[V]): Hash[IntervalRelation[E, D, V]]
  }

  object IntervalRelations {

    def default[E, D <: Domain[E]](intervalHash: Hash[Interval[E, D]]): IntervalRelations[E, D] =
      new DefaultImpl(intervalHash)

    class DefaultImpl[E, D <: Domain[E]](
      intervalHash: Hash[Interval[E, D]]
    ) extends IntervalRelations[E, D] {

      override implicit def hash[V](implicit valueHash: Hash[V]): Hash[IntervalRelation[E, D, V]] = 
        IntervalRelation.defaultHash(intervalHash, valueHash)
    }
  }

  trait Segments[E, D <: Domain[E]] {

    implicit val upperOrd: SegmentT.UpperBoundOrder[E, D]

    implicit val lowerOrd: SegmentT.LowerBoundOrder[E, D]
  }

  object Segments {

    def default[E, D <: Domain[E]](domain: D): Segments[E, D] = new DefaultImpl(domain)

    class DefaultImpl[E, D <: Domain[E]](
      domain: D
    ) extends Segments[E, D] {

      override implicit val upperOrd: SegmentT.UpperBoundOrder[E, D] = SegmentT.upperBoundOrder(domain)

      override implicit val lowerOrd: SegmentT.LowerBoundOrder[E, D] = SegmentT.lowerBoundOrder(domain)
    }
  }

  trait Validation[E, D <: Domain[E]] {

    implicit val boundsSeq: SeqValidationPredicate[Bound[E]]

    implicit val extendedBoundsSeq: SeqValidationPredicate[ExtendedBound[E]]
  }

  object Validation {

    def default[E, D <: Domain[E]](domain: D): Validation[E, D] = new DefaultImpl(domain)

    class DefaultImpl[E, D <: Domain[E]](
      domain: D
    ) extends Validation[E, D] {

      implicit override val boundsSeq: SeqValidationPredicate[Bound[E]] = 
        (prev: Bound[E], next: Bound[E]) => domain.boundOrd.lt(prev, next)

      implicit override val extendedBoundsSeq: SeqValidationPredicate[ExtendedBound[E]] = 
        (prev: ExtendedBound[E], next: ExtendedBound[E]) => domain.extendedOrd.lt(prev, next)
    }
  }
}