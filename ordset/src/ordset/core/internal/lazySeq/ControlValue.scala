package ordset.core.internal.lazySeq

import ordset.Hash
import ordset.random.RngManager
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.{ValueOps, InclusionPredicate}
import ordset.core.{SegmentSeq, Bound, ExtendedBound}
import ordset.core.value.InclusionPredicate
import ordset.core.interval.Interval
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

    // sealed trait LazyValue[E, D <: Domain[E], V] extends ControlValue[E, D, V] {

    //   override def isStable: Boolean = false

    //   override def isUnstable: Boolean = true

    //   override def isLazy: Boolean = true

    //   override def isEager: Boolean = false

    //   override def isEagerUnstable: Boolean = false

    //   override def isLazyOrStable: Boolean = true

    //   def compute: SegmentSeq[E, D, V]

    //   def takeAboveBound(bound: Bound[E])(implicit domainOps: DomainOps[E, D]): LazyValue[E, D, V]

    //   def takeBelowBound(bound: Bound[E])(implicit domainOps: DomainOps[E, D]): LazyValue[E, D, V]
    // }


    // object LazyValue {

    //   final case class Unbounded[E, D <: Domain[E], V](
    //     val seqFunc: () => SegmentSeq[E, D, V]
    //   ) extends LazyValue[E, D, V] {

    //     override def compute: SegmentSeq[E, D, V] = seqFunc()

    //     override def takeAboveBound(bound: Bound[E])(implicit domainOps: DomainOps[E, D]): LazyValue[E, D, V] = {
    //       val interval = domainOps.interval(bound.provideLower)
    //       interval match {
    //         case _: Interval.Empty[E, D] => LazyValue.Singular(seqFunc, bound)
    //         case i: Interval.NonEmpty[E, D] => LazyValue.Bounded(seqFunc, i)
    //       }
    //     }

    //     override def takeBelowBound(bound: Bound[E])(implicit domainOps: DomainOps[E, D]): LazyValue[E, D, V] = {
    //       val interval = domainOps.interval(bound.provideUpper)
    //       interval match {
    //         case _: Interval.Empty[E, D] => LazyValue.Singular(seqFunc, bound)
    //         case i: Interval.NonEmpty[E, D] => LazyValue.Bounded(seqFunc, i)
    //       }
    //     }
    //   }

    //   final case class Bounded[E, D <: Domain[E], V](
    //     val seqFunc: () => SegmentSeq[E, D, V],
    //     val interval: Interval.NonEmpty[E, D]
    //   ) extends LazyValue[E, D, V] {

    //     override def compute: SegmentSeq[E, D, V] = interval match {
    //       case i: Interval.Less[E, D] => ???
    //       case i: Interval.Greater[E, D] => ???
    //       case i: Interval.Between[E, D] => ???
    //       case i: Interval.Universal[E, D] => ???
    //     }
    //   }

    //   final case class Singular[E, D <: Domain[E], V](
    //     val seqFunc: () => SegmentSeq[E, D, V],
    //     val bound: ExtendedBound[E]
    //   ) extends LazyValue[E, D, V] {

    //     override def compute: SegmentSeq[E, D, V] = {
    //       val seq = seqFunc.apply()
    //       val value = seq.getValueForExtended(bound)
    //       UniformOrderedMap.default(value)(seq.domainOps, seq.valueOps, seq.rngManager)
    //     }

    //     override def takeAboveBound(bound: Bound[E])(implicit domainOps: DomainOps[E, D]): LazyValue[E, D, V] = this

    //     override def takeBelowBound(bound: Bound[E])(implicit domainOps: DomainOps[E, D]): LazyValue[E, D, V] = this
    //   }

    //   final case class AboveBound[E, D <: Domain[E], V](
    //     val lazyValue: Default[E, D, V],
    //     val bound: Bound[E]
    //   ) extends LazyValue[E, D, V] {

    //     override def compute: SegmentSeq[E, D, V] = lazyValue.compute.takeAboveBound(bound)

    //     override def takeAboveBound(
    //       otherBound: Bound[E]
    //     )(
    //       implicit 
    //       domainOps: DomainOps[E, D],
    //       valueOps: ValueOps[V],
    //       rngManager: RngManager
    //     ): LazyValue[E, D, V] =
    //       // Case 1:  otherBound ≤ bound
    //       //
    //       //     A         B         C                           B             C 
    //       // --------)[--------](---------             ------------------](---------  
    //       //              [///////////////     =>                   [///////////////  
    //       //            bound                                     bound     
    //       //      )///////////////////////               
    //       //  otherBound  
    //       //
    //       // Case 2:  otherBound > bound
    //       //                 
    //       //     A         B         C                                C      
    //       // --------)[--------](---------     =>      -----------------------------
    //       //              [///////////////                                    )/////
    //       //            bound                                             otherBound              
    //       //                        )/////             
    //       //                    otherBound                                             
    //       if (domainOps.boundOrd.lteqv(otherBound, bound)) this
    //       else AboveBound(lazyValue, otherBound)

    //     override def takeBelowBound(
    //       otherBound: Bound[E]
    //     )(
    //       implicit 
    //       domainOps: DomainOps[E, D],
    //       valueOps: ValueOps[V],
    //       rngManager: RngManager
    //     ): LazyValue[E, D, V] =
    //       // Case 1:  otherBound < bound
    //       //
    //       //     A         B         C                               B
    //       // --------)[--------](---------             -----------------------------  
    //       //              [///////////////     ===                empty  
    //       //            bound                                         
    //       // ///////)             
    //       //    otherBound  
    //       //
    //       // Case 2:  otherBound ≥ bound
    //       //     
    //       //     A         B         C                           B             C                          
    //       // --------)[--------](---------     ===     ------------------](---------
    //       //              [///////////////                          [///////////)
    //       //            bound                                    bound      otherBound              
    //       // /////////////////////////)                          
    //       //                    otherBound  
    //       if (domainOps.boundOrd.lt(otherBound, bound)) ???
    //       else                                           
    //   }

    //   final case class BetweenBounds[E, D <: Domain[E], V](
    //     val lazyValue: Default[E, D, V],
    //     val lowerBound: Bound[E],
    //     val upperBound: Bound[E]
    //   ) extends LazyValue[E, D, V] {

    //     override def compute: SegmentSeq[E, D, V] = 
    //       lazyValue.compute.takeAboveBound(lowerBound).takeBelowBound(upperBound)
    //   }
    // }

    final case class LazyValue[E, D <: Domain[E], V](
      private val seqFunc: () => SegmentSeq[E, D, V]
    ) extends ControlValue[E, D, V] {

      override def isStable: Boolean = false

      override def isUnstable: Boolean = true

      override def isLazy: Boolean = true

      override def isEager: Boolean = false

      override def isEagerUnstable: Boolean = false

      override def isLazyOrStable: Boolean = true

      def compute: SegmentSeq[E, D, V] = seqFunc()

      def map[E1, D1 <: Domain[E1], V1](mapFunc: SegmentSeq[E, D, V] => SegmentSeq[E1, D1, V1]): LazyValue[E1, D1, V1] =
          LazyValue(() => mapFunc(seqFunc()))
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

      override def toString: String = s"EagerValue(${if (stable) "stable" else "unstable"})"
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