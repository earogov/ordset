package ordset.core

import ordset.core.domain._
import ordset.util
import ordset.util.label.Label
import ordset.util.types.SingleValue

import scala.collection.{AbstractIterable, AbstractIterator}

/**
 * Segment is equivalent to interval with some value assigned to it.
 * The main property of segments is that they <u>cover ordered universal set without gaps and overlapping</u>.
 * So we can move from given segment to the next, previous, first or last segment of sequence.
 *
 * <h1>Basic Types</h1>
 *
 * Consider segment sequences.
 *
 * 1. General case.
 *
 * {{{
 *   Initial     Inner     Terminal
 *     v           v          v
 * X--------|-----------|-----------X
 * }}}
 *
 * 2. Special case (empty or universal sequence).
 *
 * {{{
 *               Single
 *                 v
 * X--------------------------------X
 * }}}
 * {{{
 * X - sequence bound;
 * | - segment bound.
 * }}}
 *
 * We can introduce next segment types.
 *
 * <h2>Initial</h2>
 *
 * First segment of sequence with properties:
 *
 * - doesn't have previous segment;
 *
 * - always has next segment.
 *
 * <h2>Terminal</h2>
 *
 * Last segment of sequence with properties:
 *
 * - doesn't have next segment;
 *
 * - always has previous segment.
 *
 * <h2>Inner</h2>
 *
 * Segment with properties:
 *
 * - always has next segment;
 *
 * - always has previous segment.
 *
 * <h2>Single</h2>
 *
 * The only segment in sequence (in case of empty or universal sequence) with properties:
 *
 * - doesn't have next segment;
 *
 * - doesn't have previous segment.
 *
 * <h3>
 * Each concrete class implementing segment MUST have ONE (and only one) of supertypes:
 * </h3>
 * <tr><b> - Initial  </b></tr>
 * <tr><b> - Terminal </b></tr>
 * <tr><b> - Inner    </b></tr>
 * <tr><b> - Single   </b></tr>
 *
 * <h1>Segment Hierarchy</h1>
 *
 * {{{
 *
 *                   Single
 *                 ↙        ↘
 *            First         Last
 *          ↗      ↘      ↙      ↖
 *     Initial      Segment       Terminal
 *          ↘      ↗      ↖      ↙
 *           WithNext      WithPrev
 *                 ↖      ↗
 *                   Inner
 *
 *    * subtype -> supertype
 * }}}
 *
 * General segment hierarchy include some auxiliary supertypes:
 *
 * <tr><b>Segment </b> - supertype for all segments.</tr>
 *
 * <tr><b>First   </b> - first segment of sequence. Doesn't have previous segment. May be Single or Initial.</tr>
 *
 * <tr><b>Last    </b> - last segment of sequence. Doesn't have next segment. May be Single or Terminal.</tr>
 *
 * <tr><b>WithPrev</b> - segment which has previous segment. May be Terminal or Inner.</tr>
 *
 * <tr><b>WithNext</b> - segment which has next segment. May be Initial or Inner.</tr>
 *
 * <h1>Notes</h1>
 * 
 * 1. In all ordering relations of bounds (like bound1 `>` bound2 etc.) we assume:
 * <tr>- upper bound of last segment has maximal value (equivalent to plus infinity);   </tr>
 * <tr>- lower bound of first segment has minimal value (equivalent to minus infinity). </tr>
 * <tr>
 * These properties MUST be provided by implementations of [[DomainOps.segmentUpperOrd]] and
 * [[DomainOps.segmentLowerOrd]].
 * </tr>
 * <tr></tr>
 *
 * 2. Definition of segment (traits) has forward/backward symmetry: for example if we have `moveNext` there is
 * also `movePrev` method. But its implementation may be optimized to move forward, as it's assumed this is
 * the basic use case of segments.
 * <tr></tr>
 * 
 * @tparam E type of element in ordered set
 * @tparam D type of domain
 * @tparam V type of value assigned to interval
 * @tparam S type of additional segment state
 */
sealed trait SegmentT[E, D <: Domain[E], V, +S] extends SegmentLikeT[E, D, V, S]

object SegmentT {

  type UpperBoundAscOrder[E, D <: Domain[E]] = UpperBoundOrder[E, D, AscDir]
  type UpperBoundDescOrder[E, D <: Domain[E]] = UpperBoundOrder[E, D, DescDir]

  type LowerBoundAscOrder[E, D <: Domain[E]] = LowerBoundOrder[E, D, AscDir]
  type LowerBoundDescOrder[E, D <: Domain[E]] = LowerBoundOrder[E, D, DescDir]

  implicit def upperBoundAscOrder[E, D <: Domain[E]]: UpperBoundAscOrder[E, D] = new UpperBoundOrder

  def upperBoundDescOrder[E, D <: Domain[E]]: UpperBoundDescOrder[E, D] = new UpperBoundOrder

  def lowerBoundAscOrder[E, D <: Domain[E]]: LowerBoundAscOrder[E, D] = new LowerBoundOrder

  def lowerBoundDescOrder[E, D <: Domain[E]]: LowerBoundDescOrder[E, D] = new LowerBoundOrder

  /**
   * Segment which has next segment. May be Initial or Inner.
   *
   * @see [[Segment]]
   */
  trait WithNext[E, D <: Domain[E], V, +S] extends SegmentT[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def hasNext: Boolean = true

    override def hasUpperBound(bound: Bound.Upper[E]): Boolean = domainOps.boundOrd.eqv(upperBound, bound)

    def upperBound: Bound.Upper[E]

    // Navigation --------------------------------------------------------------- //
    def moveNext: SegmentT.WithPrev[E, D, V, S] with S

    override def forwardLazyList: LazyList[SegmentT[E, D, V, S] with S] = LazyList.cons(self, moveNext.forwardLazyList)
  }

  /**
   * Segment which has previous segment. May be Terminal or Inner.
   *
   * @see [[Segment]]
   */
  trait WithPrev[E, D <: Domain[E], V, +S] extends SegmentT[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def hasPrev: Boolean = true

    override def hasLowerBound(bound: Bound.Lower[E]): Boolean = domainOps.boundOrd.eqv(lowerBound, bound)

    def lowerBound: Bound.Lower[E]
    
    // Navigation --------------------------------------------------------------- //
    def movePrev: SegmentT.WithNext[E, D, V, S] with S

    override def backwardLazyList: LazyList[SegmentT[E, D, V, S] with S] = LazyList.cons(self, movePrev.backwardLazyList)
  }

  /**
   * First segment of sequence. Doesn't have previous segment. May be Single or Initial.
   *
   * @see [[Segment]]
   */
  trait First[E, D <: Domain[E], V, +S] extends SegmentT[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isFirst: Boolean = true

    override def hasLowerBound(bound: Bound.Lower[E]): Boolean = false

    override def self: SegmentT.First[E, D, V, S] with S

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: SegmentT.First[E, D, V, S] with S = self

    override def backwardLazyList: LazyList[SegmentT[E, D, V, S] with S] = LazyList.cons(self, LazyList.empty)

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: SegmentSeq[E, D, V] = sequence
  }

  /**
   * Last segment of sequence. Doesn't have next segment. May be Single or Terminal.
   *
   * @see [[Segment]]
   */
  trait Last[E, D <: Domain[E], V, +S] extends SegmentT[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isLast: Boolean = true

    override def hasUpperBound(bound: Bound.Upper[E]): Boolean = false

    override def self: SegmentT.Last[E, D, V, S] with S

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: SegmentT.Last[E, D, V, S] with S = self

    override def forwardLazyList: LazyList[SegmentT[E, D, V, S] with S] = LazyList.cons(self, LazyList.empty)

    // Transformation ----------------------------------------------------------- //
    override def takenBelow: SegmentSeq[E, D, V] = sequence
  }

  /**
   * The only segment in sequence (in case of empty or universal sequence) with properties:
   * <tr>- doesn't have next segment;     </tr>
   * <tr>- doesn't have previous segment. </tr>
   * <tr>                                 </tr>
   * @see [[Segment]]
   */
  trait Single[E, D <: Domain[E], V, +S] extends SegmentT.First[E, D, V, S] with SegmentT.Last[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isSingle: Boolean = true

    override def contains(bound: Bound[E]): Boolean = true

    override def restrictBound(bound: Bound[E]): Bound[E] = bound

    override def interval: Interval[E, D] = domainOps.interval.universal

    override def toString: String = SetBuilderFormat.singleSegment(this)

    override def self: SegmentT.Single[E, D, V, S] with S

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: SegmentT.Single[E, D, V, S] with S = self

    override def moveToLast: SegmentT.Single[E, D, V, S] with S = self

    override def moveTo(bound: Bound[E]): SegmentT.Single[E, D, V, S] with S = self

    // Transformation ----------------------------------------------------------- //
    override def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = other
  }

  object Single {

    abstract class Truncation[E, D <: Domain[E], V, +S](
      override val segment: SegmentT.Single[E, D, V, S] with S,
      inputBound: Bound[E]
    ) extends SegmentLikeT.Truncation[E, D, V, S](
      segment,
      inputBound,
    ) {

      // Protected section -------------------------------------------------------- //

      protected override def getPrependedBoundSegment: SegmentT.Single[E, D, V, S] with S = segment

      protected override def getAppendedBoundSegment: SegmentT.Single[E, D, V, S] with S = segment
    }
  }

  /**
   * Initial segment of sequence with properties:
   * <tr>- doesn't have previous segment; </tr>
   * <tr>- always has next segment.       </tr>
   * <tr>                                 </tr>
   * @see [[Segment]]
   */
  trait Initial[E, D <: Domain[E], V, +S] extends SegmentT.WithNext[E, D, V, S] with SegmentT.First[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isInitial: Boolean = true

    override def contains(bound: Bound[E]): Boolean = domainOps.boundOrd.gteqv(upperBound, bound)

    override def restrictBound(bound: Bound[E]): Bound[E] =
      if (domainOps.boundOrd.gt(bound, upperBound)) upperBound
      else bound

    override def interval: Interval[E, D] = domainOps.interval(upperBound)

    override def toString: String = SetBuilderFormat.initialSegment(this)

    override def self: SegmentT.Initial[E, D, V, S] with S

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: SegmentT.Initial[E, D, V, S] with S = self
    
    // Transformation ----------------------------------------------------------- //
    override def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = moveNext.prepended(other)
  }

  object Initial {

    abstract class Truncation[E, D <: Domain[E], V, +S](
      override val segment: SegmentT.Initial[E, D, V, S] with S,
      inputBound: Bound[E],
    ) extends SegmentLikeT.Truncation[E, D, V, S](
      segment,
      inputBound,
    ) {

      // Protected section -------------------------------------------------------- //

      protected override def getPrependedBoundSegment: SegmentT[E, D, V, S] with S =
        if (segment.hasUpperBound(bound.provideUpper)) segment.moveNext
        else segment

      protected override def getAppendedBoundSegment: SegmentT[E, D, V, S] with S = segment
    }
  }

  /**
   * Terminal segment of sequence with properties:
   * <tr>- doesn't have next segment;   </tr>
   * <tr>- always has previous segment. </tr>
   * <tr>                               </tr>
   * @see [[Segment]]
   */
  trait Terminal[E, D <: Domain[E], V, +S] extends SegmentT.WithPrev[E, D, V, S] with SegmentT.Last[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isTerminal: Boolean = true

    override def contains(bound: Bound[E]): Boolean = domainOps.boundOrd.lteqv(lowerBound, bound)

    override def restrictBound(bound: Bound[E]): Bound[E] =
      if (domainOps.boundOrd.lt(bound, lowerBound)) lowerBound
      else bound

    override def interval: Interval[E, D] = domainOps.interval(lowerBound)

    override def toString: String = SetBuilderFormat.terminalSegment(this)

    override def self: SegmentT.Terminal[E, D, V, S] with S

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: SegmentT.Terminal[E, D, V, S] with S = self
    
    // Transformation ----------------------------------------------------------- //
    override def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = movePrev.appended(other)
  }

  object Terminal {

    abstract class Truncation[E, D <: Domain[E], V, +S](
      override val segment: SegmentT.Terminal[E, D, V, S] with S,
      inputBound: Bound[E]
    ) extends SegmentLikeT.Truncation[E, D, V, S](
      segment,
      inputBound
    ) {

      // Protected section -------------------------------------------------------- //

      protected override def getPrependedBoundSegment: SegmentT[E, D, V, S] with S = segment

      protected override def getAppendedBoundSegment: SegmentT[E, D, V, S] with S =
        if (segment.hasLowerBound(bound.provideLower)) segment.movePrev
        else segment
    }
  }

  /**
   * Segment with properties:
   * <tr>- always has next segment;     </tr>
   * <tr>- always has previous segment. </tr>
   * <tr>                               </tr>
   * @see [[Segment]]
   */
  trait Inner[E, D <: Domain[E], V, +S] extends SegmentT.WithNext[E, D, V, S] with SegmentT.WithPrev[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isInner: Boolean = true

    override def contains(bound: Bound[E]): Boolean = {
      val boundOrd = domainOps.boundOrd
      boundOrd.lteqv(lowerBound, bound) && boundOrd.gteqv(upperBound, bound)
    }

    override def restrictBound(bound: Bound[E]): Bound[E] = {
      val boundOrd = domainOps.boundOrd
      if (boundOrd.lt(bound, lowerBound)) lowerBound
      else if (boundOrd.gt(bound, upperBound)) upperBound
      else bound
    }

    override def interval: Interval[E, D] = domainOps.interval(lowerBound, upperBound)

    override def toString: String = SetBuilderFormat.innerSegment(this)

    override def self: SegmentT.Inner[E, D, V, S] with S

    // Transformation ----------------------------------------------------------- //
    override def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = moveNext.prepended(movePrev.appended(other))
  }

  object Inner {

    abstract class Truncation[E, D <: Domain[E], V, +S](
      segment: SegmentT.Inner[E, D, V, S] with S,
      inputBound: Bound[E],
    ) extends SegmentLikeT.Truncation[E, D, V, S](
      segment,
      inputBound
    ) {

      // Protected section -------------------------------------------------------- //

      protected override def getPrependedBoundSegment: SegmentT[E, D, V, S] with S =
        if (segment.hasUpperBound(bound.provideUpper)) segment.moveNext
        else segment

      protected override def getAppendedBoundSegment: SegmentT[E, D, V, S] with S =
        if (segment.hasLowerBound(bound.provideLower)) segment.movePrev
        else segment
      
    }
  }

  final class UpperBoundOrder[E, D <: Domain[E], Dir <: OrderDir]()(
    implicit dirValue: SingleValue[Dir]
  ) extends DirectedOrder.Abstract[Segment[E, D, ?], Dir] {

    import util.HashUtil._

    override val label: Label = OrderLabels.SegmentByUpperBound

    override def compare(x: Segment[E, D, ?], y: Segment[E, D, ?]): Int = (x, y) match {
      case (x: Segment.WithNext[E, D, ?], y: Segment.WithNext[E, D, ?]) =>
        x.domainOps.boundOrd.compare(x.upperBound, y.upperBound)
      case (_, _: Segment.WithNext[E, D, ?]) =>
        sign
      case (_: Segment.WithNext[E, D, ?], _) =>
        invertedSign
      case _ => 
        0
    }

    override def hash(x: Segment[E, D, ?]): Int = x match {
      case x: Segment.WithNext[E, D, ?] => product1Hash(x.domainOps.boundOrd.hash(x.upperBound))
      case _ => product1Hash(x.hashCode())
    }

    override def eqv(x: Segment[E, D, ?], y: Segment[E, D, ?]): Boolean = (x, y) match {
      case (x: Segment.WithNext[E, D, ?], y: Segment.WithNext[E, D, ?]) =>
        x.domainOps.boundOrd.eqv(x.upperBound, y.upperBound)
      case (_, _: Segment.WithNext[E, D, ?]) =>
        false
      case (_: Segment.WithNext[E, D, ?], _) =>
        false
      case _ => 
        true
    }
  }

  final class LowerBoundOrder[E, D <: Domain[E], Dir <: OrderDir]()(
    implicit dirValue: SingleValue[Dir]
  ) extends DirectedOrder.Abstract[Segment[E, D, ?], Dir] {

    import util.HashUtil._

    override def label: Label = OrderLabels.SegmentByLowerBound

    override def compare(x: Segment[E, D, ?], y: Segment[E, D, ?]): Int = (x, y) match {
      case (x: Segment.WithPrev[E, D, ?], y: Segment.WithPrev[E, D, ?]) =>
        x.domainOps.boundOrd.compare(x.lowerBound, y.lowerBound)
      case (_, _: Segment.WithPrev[E, D, ?]) =>
        invertedSign
      case (_: Segment.WithPrev[E, D, ?], _) =>
        sign
      case _ => 0
    }

    override def hash(x: Segment[E, D, ?]): Int = x match {
      case x: Segment.WithPrev[E, D, ?] => product1Hash(x.domainOps.boundOrd.hash(x.lowerBound))
      case _ => product1Hash(x.hashCode())
    }

    override def eqv(x: Segment[E, D, ?], y: Segment[E, D, ?]): Boolean = (x, y) match {
      case (x: Segment.WithPrev[E, D, ?], y: Segment.WithPrev[E, D, ?]) =>
        x.domainOps.boundOrd.eqv(x.lowerBound, y.lowerBound)
      case (_, _: Segment.WithPrev[E, D, ?]) =>
        false
      case (_: Segment.WithPrev[E, D, ?], _) =>
        false
      case _ => 
        true
    }
  }
}
