package ordset.core.internal.lazySeq

import ordset.Hash
import ordset.random.RngManager
import ordset.core.domain.Domain
import ordset.core.value.{ValueOps, InclusionPredicate}
import ordset.core.{SegmentSeq, Bound, ExtendedBound}
import ordset.core.range.Range
import ordset.core.map.UniformOrderedMap

protected[ordset] sealed trait ControlValue[E, D <: Domain[E], V] {

  def isStable: Boolean

  def isUnstable: Boolean

  def isLazy: Boolean

  def isEager: Boolean

  def isEagerUnstable: Boolean = isEager && isUnstable

  def isLazyOrStable: Boolean = isStable || isLazy
}

protected[ordset] object ControlValue {

    sealed trait LazyValue[E, D <: Domain[E], V] extends ControlValue[E, D, V] {

      override def isStable: Boolean = false

      override def isUnstable: Boolean = true

      override def isLazy: Boolean = true

      override def isEager: Boolean = false

      override def isEagerUnstable: Boolean = false

      override def isLazyOrStable: Boolean = true

      def compute: SegmentSeq[E, D, V]

      def takeAboveBound(bound: Bound[E]): LazyValue[E, D, V]

      def takeBelowBound(bound: Bound[E]): LazyValue[E, D, V]
    }

    object LazyValue {

      final case class Default[E, D <: Domain[E], V](
        val seqFunc: () => SegmentSeq[E, D, V]
      )(
        implicit val domain: Domain[E]
      ) extends LazyValue[E, D, V] {

        override def compute: SegmentSeq[E, D, V] = 
          seqFunc().takeAboveExtended(domain.lowerExtendedBound).takeBelowExtended(domain.upperExtendedBound)

        override def takeAboveBound(bound: Bound[E]): LazyValue[E, D, V] =
          (domain.rangeFactory.between(bound, domain.upperExtendedBound): Range[ExtendedBound[E]]) match {
            //                  domain
            //        [------------------------]
            //               [///////////////////////////
            //             bound
            case r: Range.NonEmpty[ExtendedBound[E]] => new LazyValue.Bounded(seqFunc, r)
            //                  domain
            //        [------------------------]
            //                                     [/////
            //                                   bound
            case _ => new LazyValue.Single(seqFunc, domain.upperExtendedBound)
          }

        override def takeBelowBound(bound: Bound[E]): LazyValue[E, D, V] =
          (domain.rangeFactory.between(domain.lowerExtendedBound, bound): Range[ExtendedBound[E]]) match {
            //                  domain
            //        [------------------------]
            // ////////////////////////)
            //                       bound
            case r: Range.NonEmpty[ExtendedBound[E]] => new LazyValue.Bounded(seqFunc, r)
            //                  domain
            //        [------------------------]
            // /////)
            //     bound
            case _ => new LazyValue.Single(seqFunc, domain.lowerExtendedBound)
          }

        override def toString(): String = s"LazyValue.Default(seqFunc = $seqFunc)"
      }

      final case class Bounded[E, D <: Domain[E], V](
        val seqFunc: () => SegmentSeq[E, D, V],
        val bounds: Range.NonEmpty[ExtendedBound[E]]
      )(
        implicit val domain: Domain[E]
      ) extends LazyValue[E, D, V] {

        override def compute: SegmentSeq[E, D, V] = 
          seqFunc().takeAboveExtended(bounds.lower).takeBelowExtended(bounds.upper)

        override def takeAboveBound(bound: Bound[E]): LazyValue[E, D, V] = {
          val requestedRange= domain.rangeFactory.between(bound, domain.upperExtendedBound)
          val newRange: Range[ExtendedBound[E]] = domain.rangeAlgebra.cross(requestedRange, bounds)(domain.rangeFactory)
          newRange match {
            //                  bounds
            //        [------------------------]
            //               [///////////////////////////
            //             bound
            case r: Range.NonEmpty[ExtendedBound[E]] => new LazyValue.Bounded(seqFunc, r)
            //                  bounds
            //        [------------------------]
            //                                     [/////
            //                                   bound
            case _ => new LazyValue.Single(seqFunc, bounds.upper)
          }
        }

        override def takeBelowBound(bound: Bound[E]): LazyValue[E, D, V] = {
          val requestedRange = domain.rangeFactory.between(domain.lowerExtendedBound, bound)
          val newRange: Range[ExtendedBound[E]] = domain.rangeAlgebra.cross(requestedRange, bounds)(domain.rangeFactory)
          newRange match {
            //                  bounds
            //        [------------------------]
            // ////////////////////////)
            //                       bound
            case r: Range.NonEmpty[ExtendedBound[E]] => new LazyValue.Bounded(seqFunc, r)
            //                  bounds
            //        [------------------------]
            // /////)
            //     bound
            case _ => new LazyValue.Single(seqFunc, domain.lowerExtendedBound)
          }
        }

        override def toString(): String = s"LazyValue.Bounded(seqFunc = $seqFunc, bounds = $bounds)"
      }
      
      final case class Single[E, D <: Domain[E], V](
        val seqFunc: () => SegmentSeq[E, D, V],
        val bound: ExtendedBound[E]
      )(
        implicit val domain: Domain[E]
      ) extends LazyValue[E, D, V] {

        override def compute: SegmentSeq[E, D, V] = {
          val seq = seqFunc()
          val value = seq.getValueForExtended(bound)
          UniformOrderedMap.default(value)(seq.domainOps, seq.valueOps, seq.rngManager)
        }

        override def takeAboveBound(bound: Bound[E]): LazyValue[E, D, V] = this

        override def takeBelowBound(bound: Bound[E]): LazyValue[E, D, V] = this

        override def toString(): String = s"LazyValue.Single(seqFunc = $seqFunc, bound = $bound)"
      }
    }

    final case class EagerValue[E, D <: Domain[E], V] private (
      private val stable: Boolean
    ) extends ControlValue[E, D, V] {

      override def isStable: Boolean = stable

      override def isUnstable: Boolean = !stable

      override def isLazy: Boolean = false

      override def isEager: Boolean = true

      override def isEagerUnstable: Boolean = !stable

      override def isLazyOrStable: Boolean = stable

      override def toString: String = s"EagerValue(stable = $stable)"
    }

    object EagerValue {

      def cons[E, D <: Domain[E], V](isStable: Boolean): EagerValue[E, D, V] = if (isStable) stable else unstable

      def stable[E, D <: Domain[E], V]: EagerValue[E, D, V] = stableInstance.asInstanceOf

      def unstable[E, D <: Domain[E], V]: EagerValue[E, D, V] = unstableInstance.asInstanceOf

      // Private section ---------------------------------------------------------- //
      private lazy val stableInstance: EagerValue[Any, Domain[Any], Any] = new EagerValue(true)

      private lazy val unstableInstance: EagerValue[Any, Domain[Any], Any] = new EagerValue(false)
    }

    final class ControlValueHash[E, D <: Domain[E], V]
      extends Hash[ControlValue[E, D, V]] {

      import ordset.util.HashUtil._

      override def hash(x: ControlValue[E, D, V]): Int = x.##

      override def eqv(x: ControlValue[E, D, V], y: ControlValue[E, D, V]): Boolean = (x, y) match {
        case (x: LazyValue[_, _, _], y: LazyValue[_, _, _]) => x.eq(y)
        case (x: EagerValue[_, _, _], y: EagerValue[_, _, _]) => x.isStable == y.isStable
        case _ => false
      }
    }

    object ControlValueHash {

      def get[E, D <: Domain[E], V]: Hash[ControlValue[E, D, V]] = instance.asInstanceOf

      // Private section ---------------------------------------------------------- //
      private lazy val instance: ControlValueHash[Any, Domain[Any], Any] = new ControlValueHash
    }

    final class ControlValueOps[E, D <: Domain[E], V](
      override val unit: ControlValue[E, D, V] = EagerValue.stable[E, D, V],
      override val valueHash: Hash[ControlValue[E, D, V]] = ControlValueHash.get[E, D, V],
      override val valueIncl: InclusionPredicate[ControlValue[E, D, V]] = InclusionPredicate.alwaysIncluded
    ) extends ValueOps[ControlValue[E, D, V]]

    object ControlValueOps {

      def get[E, D <: Domain[E], V]: ValueOps[ControlValue[E, D, V]] = instance.asInstanceOf

      // Private section ---------------------------------------------------------- //
      private lazy val instance: ControlValueOps[Any, Domain[Any], Any] = new ControlValueOps()
    }
}