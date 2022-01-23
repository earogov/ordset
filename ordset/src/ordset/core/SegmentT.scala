package ordset.core

import ordset.{Order, Hash}
import ordset.core.domain.*
import ordset.core.interval.*
import ordset.core.map.{LazyTreapOrderedMap, UniformOrderedMap, TreapOrderedMap}
import ordset.util

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
 * These properties MUST be provided by implementations of [[DomainOps.segments.upperOrd]] and
 * [[DomainOps.segments.lowerOrd]].
 * </tr>
 * <tr></tr>
 *
 * 2. Definition of segment (traits) has forward/backward symmetry: for example if we have `moveNext` there is
 * also `movePrev` method. But its implementation may be optimized to move forward, as it's assumed this is
 * the basic use case of segments.
 * <tr></tr>
 * 
 * @tparam E type of element in ordered set
 * @tparam D type of elements domain
 * @tparam V type of value assigned to interval of elements
 * @tparam S type of additional segment state
 */
sealed trait SegmentT[E, D[X] <: Domain[X], V, +S] extends SegmentLikeT[E, D, V, S] {

  override def truncation(bound: ExtendedBound[E]): SegmentTruncationT[E, D, V, S, this.type]

  override def lowerTruncation: SegmentTruncationT[E, D, V, S, this.type]

  override def upperTruncation: SegmentTruncationT[E, D, V, S, this.type]
}

object SegmentT {

  implicit def upperBoundOrder[E, D[X] <: Domain[X]](implicit domain: D[E]): UpperBoundOrder[E, D] =
    new UpperBoundOrderImpl(domain)

  implicit def lowerBoundOrder[E, D[X] <: Domain[X]](implicit domain: D[E]): LowerBoundOrder[E, D] =
    new LowerBoundOrderImpl(domain)

  /**
   * Segment which has next segment. May be [[Initial]] or [[Inner]].
   *
   * @see [[SegmentT]]
   */
  trait WithNext[E, D[X] <: Domain[X], V, +S] extends SegmentT[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def hasNext: Boolean = true

    override def hasNextSuchThat(p: SegmentT.WithPrev[E, D, V, S] with S => Boolean): Boolean = p.apply(moveNext)

    override def hasUpperBound(bound: Bound.Upper[E]): Boolean = domainOps.boundOrd.eqv(upper, bound)

    override def upper: Bound.Upper[E]

    override def self: SegmentT.WithNext[E, D, V, S] with S

    // Navigation --------------------------------------------------------------- //
    /**
     * @return next segment.
     */
    def moveNext: SegmentT.WithPrev[E, D, V, S] with S

    override def forwardLazyList: LazyList[SegmentT[E, D, V, S] with S] = LazyList.cons(self, moveNext.forwardLazyList)
  }

  /**
   * Segment which has previous segment. May be [[Terminal]] or [[Inner]].
   *
   * @see [[SegmentT]]
   */
  trait WithPrev[E, D[X] <: Domain[X], V, +S] extends SegmentT[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def hasPrev: Boolean = true

    override def hasPrevSuchThat(p: SegmentT.WithNext[E, D, V, S] with S => Boolean): Boolean = p.apply(movePrev)

    override def hasLowerBound(bound: Bound.Lower[E]): Boolean = domainOps.boundOrd.eqv(lower, bound)

    override def lower: Bound.Lower[E]

    override def self: SegmentT.WithPrev[E, D, V, S] with S

    // Navigation --------------------------------------------------------------- //
    /**
     * @return previous segment.
     */
    def movePrev: SegmentT.WithNext[E, D, V, S] with S

    override def backwardLazyList: LazyList[SegmentT[E, D, V, S] with S] = LazyList.cons(self, movePrev.backwardLazyList)
  }

  /**
   * First segment of sequence. Doesn't have previous segment. May be [[Single]] or [[Initial]].
   *
   * @see [[SegmentT]]
   */
  trait First[E, D[X] <: Domain[X], V, +S] extends SegmentT[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isFirst: Boolean = true

    override def hasLowerBound(bound: Bound.Lower[E]): Boolean = false

    override def lower: ExtendedBound.BelowAll.type = ExtendedBound.BelowAll

    override def self: SegmentT.First[E, D, V, S] with S

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: SegmentT.First[E, D, V, S] with S = self

    override def backwardLazyList: LazyList[SegmentT[E, D, V, S] with S] = LazyList.cons(self, LazyList.empty)

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: SegmentSeq[E, D, V] = sequence
  }

  /**
   * Last segment of sequence. Doesn't have next segment. May be [[Single]] or [[Terminal]].
   *
   * @see [[SegmentT]]
   */
  trait Last[E, D[X] <: Domain[X], V, +S] extends SegmentT[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isLast: Boolean = true

    override def hasUpperBound(bound: Bound.Upper[E]): Boolean = false

    override def upper: ExtendedBound.AboveAll.type = ExtendedBound.AboveAll

    override def self: SegmentT.Last[E, D, V, S] with S

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: SegmentT.Last[E, D, V, S] with S = self

    override def forwardLazyList: LazyList[SegmentT[E, D, V, S] with S] = LazyList.cons(self, LazyList.empty)

    // Transformation ----------------------------------------------------------- //
    override def takeBelow: SegmentSeq[E, D, V] = sequence
  }

  /**
   * The only segment in sequence (in case of empty or universal sequence) with properties:
   * <tr>- doesn't have next segment;     </tr>
   * <tr>- doesn't have previous segment. </tr>
   * <tr>                                 </tr>
   * @see [[SegmentT]]
   */
  trait Single[E, D[X] <: Domain[X], V, +S] extends SegmentT.First[E, D, V, S] with SegmentT.Last[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isSingle: Boolean = true

    override def containsBound(bound: Bound[E]): Boolean = true

    override def containsExtended(bound: ExtendedBound[E]): Boolean = true

    override def restrictBound(bound: Bound[E]): Bound[E] = bound

    override def interval: Interval[E, D] = domainOps.intervals.factory.universal

    override def toString: String = SetBuilderFormat.singleSegment(this)

    override def self: SegmentT.Single[E, D, V, S] with S

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: SegmentT.Single[E, D, V, S] with S = self

    override def moveToLast: SegmentT.Single[E, D, V, S] with S = self

    override def moveToBound(bound: Bound[E]): SegmentT.Single[E, D, V, S] with S = self

    override def moveToExtended(bound: ExtendedBound[E]): SegmentT.Single[E, D, V, S] with S = self

    override def moveToElement(element: E): SegmentT.Single[E, D, V, S] with S = self

    // Transformation ----------------------------------------------------------- //
    override def patch(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = other

    override def flatMap(mapFunc: () => SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
      val lazySeq = UniformOrderedMap.default(
        Some(mapFunc)
      )(
        domainOps,
        SeqSupplier.ValueOpsImpl.get,
        rngManager
      )
      LazyTreapOrderedMap.apply(sequence, lazySeq)(domainOps, valueOps, rngManager)
    }
  }

  object Single {

    abstract class Truncation[E, D[X] <: Domain[X], V, +S, +Seg <: SegmentT.Single[E, D, V, S] with S](
      override val segment: Seg,
      inputBound: ExtendedBound[E],
    ) extends SegmentTruncationT[E, D, V, S, Seg](
      segment,
      inputBound,
    ) {

      // Protected section -------------------------------------------------------- //
      protected override def getSegmentForPrepending: SegmentT.Single[E, D, V, S] with S = segment

      protected override def getSegmentForAppending: SegmentT.Single[E, D, V, S] with S = segment
    }
  }

  /**
   * Initial segment of sequence with properties:
   * <tr>- doesn't have previous segment; </tr>
   * <tr>- always has next segment.       </tr>
   * <tr>                                 </tr>
   * @see [[SegmentT]]
   */
  trait Initial[E, D[X] <: Domain[X], V, +S] extends SegmentT.WithNext[E, D, V, S] with SegmentT.First[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isInitial: Boolean = true

    override def containsBound(bound: Bound[E]): Boolean = domainOps.boundOrd.gteqv(upper, bound)

    override def containsExtended(bound: ExtendedBound[E]): Boolean =
      bound match {
        case b: Bound[E] => containsBound(b)
        case ExtendedBound.BelowAll => true
        case ExtendedBound.AboveAll => false
      }

    override def restrictBound(bound: Bound[E]): Bound[E] =
      if (domainOps.boundOrd.gt(bound, upper)) upper
      else bound

    override def interval: Interval[E, D] = domainOps.intervals.factory.belowBound(upper)

    override def toString: String = SetBuilderFormat.initialSegment(this)

    override def self: SegmentT.Initial[E, D, V, S] with S

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: SegmentT.Initial[E, D, V, S] with S = self
    
    // Transformation ----------------------------------------------------------- //
    override def patch(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = moveNext.prepend(other)

    override def flatMap(mapFunc: () => SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
      val lazySeq = TreapOrderedMap.getFactory.unsafeBuildAsc(
        List((upper, Some(mapFunc)), (ExtendedBound.AboveAll, None)),
        domainOps,
        SeqSupplier.ValueOpsImpl.get
      )(
        SeqValidationPredicate.alwaysTrue,
        SeqValidationPredicate.alwaysTrue
      )(
        rngManager
      )
      LazyTreapOrderedMap.apply(sequence, lazySeq)(domainOps, valueOps, rngManager)
    }
  }

  object Initial {

    abstract class Truncation[E, D[X] <: Domain[X], V, +S, +Seg <: SegmentT.Initial[E, D, V, S] with S](
      override val segment: Seg,
      inputBound: ExtendedBound[E],
    ) extends SegmentTruncationT[E, D, V, S, Seg](
      segment,
      inputBound,
    ) {

      // Protected section -------------------------------------------------------- //
      protected override def getSegmentForPrepending: SegmentT[E, D, V, S] with S =
        SegmentTruncationT.getSegmentForPrependingCaseSegmentWithNext(bound, segment)

      protected override def getSegmentForAppending: SegmentT[E, D, V, S] with S = segment
    }
  }

  /**
   * Terminal segment of sequence with properties:
   * <tr>- doesn't have next segment;   </tr>
   * <tr>- always has previous segment. </tr>
   * <tr>                               </tr>
   * @see [[SegmentT]]
   */
  trait Terminal[E, D[X] <: Domain[X], V, +S] extends SegmentT.WithPrev[E, D, V, S] with SegmentT.Last[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isTerminal: Boolean = true

    override def containsBound(bound: Bound[E]): Boolean = domainOps.boundOrd.lteqv(lower, bound)

    override def containsExtended(bound: ExtendedBound[E]): Boolean =
      bound match {
        case b: Bound[E] => containsBound(b)
        case ExtendedBound.BelowAll => false
        case ExtendedBound.AboveAll => true
      }

    override def restrictBound(bound: Bound[E]): Bound[E] =
      if (domainOps.boundOrd.lt(bound, lower)) lower
      else bound

    override def interval: Interval[E, D] = domainOps.intervals.factory.aboveBound(lower)

    override def toString: String = SetBuilderFormat.terminalSegment(this)

    override def self: SegmentT.Terminal[E, D, V, S] with S

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: SegmentT.Terminal[E, D, V, S] with S = self
    
    // Transformation ----------------------------------------------------------- //
    override def patch(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = movePrev.append(other)

    override def flatMap(mapFunc: () => SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
      val lazySeq = TreapOrderedMap.getFactory.unsafeBuildAsc(
        List((lower.flipLower, None), (ExtendedBound.AboveAll, Some(mapFunc))),
        domainOps,
        SeqSupplier.ValueOpsImpl.get
      )(
        SeqValidationPredicate.alwaysTrue,
        SeqValidationPredicate.alwaysTrue
      )(
        rngManager
      )
      LazyTreapOrderedMap.apply(sequence, lazySeq)(domainOps, valueOps, rngManager)
    }
  }

  object Terminal {

    abstract class Truncation[E, D[X] <: Domain[X], V, +S, +Seg <: SegmentT.Terminal[E, D, V, S] with S](
      override val segment: Seg,
      inputBound: ExtendedBound[E]
    ) extends SegmentTruncationT[E, D, V, S, Seg](
      segment,
      inputBound
    ) {

      // Protected section -------------------------------------------------------- //
      protected override def getSegmentForPrepending: SegmentT[E, D, V, S] with S = segment

      protected override def getSegmentForAppending: SegmentT[E, D, V, S] with S =
        SegmentTruncationT.getSegmentForAppendingCaseSegmentWithPrev(bound, segment)
    }
  }

  /**
   * Segment with properties:
   * <tr>- always has next segment;     </tr>
   * <tr>- always has previous segment. </tr>
   * <tr>                               </tr>
   * @see [[SegmentT]]
   */
  trait Inner[E, D[X] <: Domain[X], V, +S] extends SegmentT.WithNext[E, D, V, S] with SegmentT.WithPrev[E, D, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isInner: Boolean = true

    override def containsBound(bound: Bound[E]): Boolean = {
      val boundOrd = domainOps.boundOrd
      boundOrd.lteqv(lower, bound) && boundOrd.gteqv(upper, bound)
    }

    override def containsExtended(bound: ExtendedBound[E]): Boolean =
      bound match {
        case b: Bound[E] => containsBound(b)
        case _ => false
      }

    override def restrictBound(bound: Bound[E]): Bound[E] = {
      val boundOrd = domainOps.boundOrd
      if (boundOrd.lt(bound, lower)) lower
      else if (boundOrd.gt(bound, upper)) upper
      else bound
    }

    override def interval: Interval[E, D] = domainOps.intervals.factory.betweenBounds(lower, upper)

    override def toString: String = SetBuilderFormat.innerSegment(this)

    override def self: SegmentT.Inner[E, D, V, S] with S

    // Transformation ----------------------------------------------------------- //
    override def patch(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = moveNext.prepend(movePrev.append(other))

    override def flatMap(mapFunc: () => SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
      val lazySeq = TreapOrderedMap.getFactory.unsafeBuildAsc(
        List((lower.flipLower, None), (upper, Some(mapFunc)), (ExtendedBound.AboveAll, None)),
        domainOps,
        SeqSupplier.ValueOpsImpl.get
      )(
        SeqValidationPredicate.alwaysTrue,
        SeqValidationPredicate.alwaysTrue
      )(
        rngManager
      )
      LazyTreapOrderedMap.apply(sequence, lazySeq)(domainOps, valueOps, rngManager)
    }
  }

  object Inner {

    abstract class Truncation[E, D[X] <: Domain[X], V, +S, +Seg <: SegmentT.Inner[E, D, V, S] with S](
      segment: Seg,
      inputBound: ExtendedBound[E],
    ) extends SegmentTruncationT[E, D, V, S, Seg](
      segment,
      inputBound
    ) {

      // Protected section -------------------------------------------------------- //
      protected override def getSegmentForPrepending: SegmentT[E, D, V, S] with S =
        SegmentTruncationT.getSegmentForPrependingCaseSegmentWithNext(bound, segment)

      protected override def getSegmentForAppending: SegmentT[E, D, V, S] with S =
        SegmentTruncationT.getSegmentForAppendingCaseSegmentWithPrev(bound, segment)
    }
  }

  /**
   * Order of segments by their upper bounds.
   *
   * The order enforces condition:
   * <tr>
   *   - [[Segment.Last]] is maximal segment (i.e. upper bound of last segment is maximal in domain).
   * </tr>
   */
  trait UpperBoundOrder[E, D[X] <: Domain[X]] extends Order[Segment[E, D, ?]] with Hash[Segment[E, D, ?]] {

    import util.HashUtil._

    def domain: D[E]

    final override def compare(x: Segment[E, D, ?], y: Segment[E, D, ?]): Int = 
      domain.extendedOrd.compare(x.upper, y.upper)

    final override def hash(x: Segment[E, D, ?]): Int = 
      product1Hash(domain.extendedOrd.hash(x.upper))  

    final override def eqv(x: Segment[E, D, ?], y: Segment[E, D, ?]): Boolean =
      domain.extendedOrd.eqv(x.upper, y.upper)
  }

  /**
   * Order of segments by their lower bounds.
   *
   * The order enforces condition:
   * <tr>
   *   - [[Segment.First]] is minimal segment (i.e. lower bound of first segment is minimal in domain).
   * </tr>
   */
  trait LowerBoundOrder[E, D[X] <: Domain[X]] extends Order[Segment[E, D, ?]] with Hash[Segment[E, D, ?]] {

    import util.HashUtil._

    def domain: D[E]

    final override def compare(x: Segment[E, D, ?], y: Segment[E, D, ?]): Int = 
      domain.extendedOrd.compare(x.lower, y.lower)

    final override def hash(x: Segment[E, D, ?]): Int =
      product1Hash(domain.extendedOrd.hash(x.lower))  

    final override def eqv(x: Segment[E, D, ?], y: Segment[E, D, ?]): Boolean = 
      domain.extendedOrd.eqv(x.lower, y.lower)
  }

  // Private section ---------------------------------------------------------- //
  private final class UpperBoundOrderImpl[E, D[X] <: Domain[X]](override val domain: D[E]) extends UpperBoundOrder[E, D]

  private final class LowerBoundOrderImpl[E, D[X] <: Domain[X]](override val domain: D[E]) extends LowerBoundOrder[E, D]
}
