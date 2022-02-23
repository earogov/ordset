package ordset.core.segmentSeq

import ordset.core.{Bound, ExtendedBound}
import ordset.core.range.Range
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.segmentSeq.internal.SegmentSeqExceptionUtil
import ordset.core.segmentSeq.map.{MappedOrderedMap, MappedValueOrderedMap}
import ordset.core.value.ValueOps
import ordset.core.interval.{Interval, IntervalRelation}
import ordset.random.RngManager

import scala.Specializable.AllNumeric as spNum
import scala.collection.{AbstractIterable, AbstractIterator}
import scala.specialized as sp

/**
 * Non-sealed superclass of [[SegmentT]] 
 *
 * @tparam E type of elements on ordered domain
 * @tparam D type of ordered domain
 * @tparam V type of new value assigned to interval of elements
 * @tparam S type of range state
 */
trait SegmentLikeT[@sp(spNum) E, D[X] <: Domain[X], @sp(Boolean) V, +S] extends Range.NonEmpty[ExtendedBound[E]] {

  // Inspection --------------------------------------------------------------- //
  /** Domain operations. */
  implicit def domainOps: DomainOps[E, D] = sequence.domainOps

  /** Value operations (equality type class, etc). */
  implicit def valueOps: ValueOps[V] = sequence.valueOps

  /** Random numbers generator. */
  implicit def rngManager: RngManager = sequence.rngManager

  /** Sequence to which segment belongs. */
  def sequence: SegmentSeqT[E, D, V, S]

  /** Value associated with segment. */
  def value: V

  /**
   * Returns `true` if elements of segment are included in set represented by `sequence`.
   * {{{
   *
   * sequence:
   *
   *        true            false             true             - segment.isIncluded
   * X---------------)[--------------](----------------X
   *      segment0       segment1          segment2
   *
   * set represented by sequence:
   *
   * X---------------)                (----------------X
   * }}}
   */
  def isIncluded: Boolean = valueOps.isIncluded(value)

  /** @return `true` if `bound` is between segment bounds. */
  def containsBound(bound: Bound[E]): Boolean

  /** @return `true` if `bound` is between segment bounds. */
  def containsExtended(bound: ExtendedBound[E]): Boolean

  /** @return `true` if `element` is between segment bounds. */
  def containsElement(element: E): Boolean = containsBound(Bound.Upper.including(element))

  /**
   * Get extended lower bound of segment:
   * <tr>- if segment is first, returns [[ExtendedBound.BelowAll]];</tr>
   * <tr>- if segment has previous segment, returns its lower bound.</tr>
   */
  override def lower: ExtendedBound.Lower[E]

  /**
   * Get extended upper bound of segment:
   * <tr>- if segment is last, returns [[ExtendedBound.AboveAll]];</tr>
   * <tr>- if segment has next segment, returns its upper bound.</tr>
   */
  override def upper: ExtendedBound.Upper[E]

  /** @return `true` if segment has specified lower bound. */
  def hasLowerBound(bound: Bound.Lower[E]): Boolean

  /** @return `true` if segment has specified extended lower bound. */
  def hasLowerExtended(bound: ExtendedBound.Lower[E]): Boolean =
    bound match {
      case b: Bound.Lower[E] => hasLowerBound(b)
      case ExtendedBound.BelowAll => isFirst
    }

  /** @return `true` if segment has specified upper bound. */
  def hasUpperBound(bound: Bound.Upper[E]): Boolean

  /** @return `true` if segment has specified extended upper bound. */
  def hasUpperExtended(bound: ExtendedBound.Upper[E]): Boolean =
    bound match {
      case b: Bound.Upper[E] => hasUpperBound(b)
      case ExtendedBound.AboveAll => isLast
    }

  /**
   * Returns input `bound`, if it is inside segment, otherwise returns segment bound closest to input `bound` 
   * (either lower or upper).
   *
   * <h3>Note</h3>
   *
   * The formula below seems to be equivalent to method definition:
   *
   * output bound = min(max(`bound`, lower bound of segment), upper bound of segment)     (1)
   *
   * But there is a subtle difference: according to bound ordering defined by domain two bounds,
   * for example, 5`]` and `[`5 are equal. So min() and max() operators can return any of them.
   *
   * Consider the case.
   * {{{
   *       bound
   *         ]
   * -------)[-----------)[--------
   *         5     ^     10
   *            segment
   * }}}
   * [[restrictBound]] must return `bound` = 5`]`.
   * But implementation based on formula (1) can return either 5`]` or 5`[`.
   */
  def restrictBound(bound: Bound[E]): Bound[E]

  /**
   * Returns input extended `bound`, if it is inside segment, otherwise returns segment bound closest to input `bound` 
   * (either lower or upper).
   *
   * @see [[restrictBound]]
   */
  def restrictExtended(bound: ExtendedBound[E]): ExtendedBound[E] =
    bound match {
      case b: Bound[E] => restrictBound(b)
      case ExtendedBound.BelowAll => lower
      case ExtendedBound.AboveAll => upper
    }

  /** @return `true` if segment has specified value. */
  def hasValue(v: V): Boolean = valueOps.eqv(value, v)

  /** @return `true` if there is next segment after current, i.e. if it's [[SegmentT.WithNext]]. */
  def hasNext: Boolean = false

  /**
   * @return `true` if there is next segment after current that satisfies specified predicate.
   */
  def hasNextSuchThat(p: SegmentT.WithPrev[E, D, V, S] with S => Boolean): Boolean = false

  /** @return `true` if there is previous segment before current, i.e. if it's [[SegmentT.WithPrev]]. */
  def hasPrev: Boolean = false
  
  /**
   * @return `true` if there is previous segment before current that satisfies specified predicate.
   */
  def hasPrevSuchThat(p: SegmentT.WithNext[E, D, V, S] with S => Boolean): Boolean = false
  
  /** @return `true` if segment is [[SegmentT.First]]. */
  def isFirst: Boolean = false

  /** @return `true` if segment is [[SegmentT.Last]]. */
  def isLast: Boolean = false

  /** @return `true` if segment is [[SegmentT.Inner]]. */
  def isInner: Boolean = false

  /** @return `true` if segment is [[SegmentT.Single]]. */
  def isSingle: Boolean = false

  /** @return `true` if segment is [[SegmentT.Initial]]. */
  def isInitial: Boolean = false

  /** @return `true` if segment is [[SegmentT.Terminal]]. */
  def isTerminal: Boolean = false

  /** @return interval that corresponds to the segment in given domain. */
  def interval: Interval[E, D]

  /** @return tuple of segment value and interval that corresponds to the segment in given domain. */
  def intervalRelation: IntervalRelation[E, D, V] = IntervalRelation(interval, value)

  override def toString: String =
    SetBuilderFormat.segment(self, SetBuilderFormat.toStringFunc[E], SetBuilderFormat.toStringFunc[V])

  /**
   * Returns current instance with a more precise type.
   *
   * It's may be convenient to pass arguments of type [[SegmentT]] (without S) and later restore full type
   * if needed.
   */
  def self: SegmentT[E, D, V, S] with S

  // Navigation --------------------------------------------------------------- //
  /** @return first segment of sequence. */
  def moveToFirst: SegmentT.First[E, D, V, S] with S = sequence.firstSegment

  /** @return last segment of sequence. */
  def moveToLast: SegmentT.Last[E, D, V, S] with S = sequence.lastSegment

  /** @return segment that contains specified bound. */
  def moveToBound(bound: Bound[E]): SegmentT[E, D, V, S] with S = sequence.getSegmentForBound(bound)

  /** @return segment that contains specified bound. */
  def moveToExtended(bound: ExtendedBound[E]): SegmentT[E, D, V, S] with S =
    bound match {
      case b: Bound[E] => moveToBound(b)
      case ExtendedBound.BelowAll => moveToFirst
      case ExtendedBound.AboveAll => moveToLast
    }

  /** @return segment that contains specified element. */
  def moveToElement(element: E): SegmentT[E, D, V, S] with S = moveToBound(Bound.Upper.including(element))

  /** @return [[Iterable]] of all next segments of sequence starting from current. */
  def forwardIterable: Iterable[SegmentT[E, D, V, S] with S] = new AbstractIterable {

    override def iterator: Iterator[SegmentT[E, D, V, S] with S] = forwardIterator
  }

  /** @return [[Iterator]] of all next segments of sequence starting from current. */
  def forwardIterator: Iterator[SegmentT[E, D, V, S] with S] = new AbstractIterator {

    import scala.language.unsafeNulls

    private var current: SegmentT[E, D, V, S] with S | Null = _

    override def hasNext: Boolean = current == null || !current.isLast

    override def next(): SegmentT[E, D, V, S] with S = (current: SegmentT[E, D, V, S] | Null) match {
      case null =>
        current = self
        self
      case s: SegmentT.WithNext[E, D, V, S] =>
        val n = s.moveNext
        current = n
        n
      case _ =>
        SegmentSeqExceptionUtil.throwNoNextSegment(current)
    }
  }

  /** @return [[Iterable]] of all previous segments of sequence starting from current. */
  def backwardIterable: Iterable[SegmentT[E, D, V, S] with S] = new AbstractIterable {

    override def iterator: Iterator[SegmentT[E, D, V, S] with S] = backwardIterator
  }

  /** @return [[Iterator]] of all previous segments of sequence starting from current. */
  def backwardIterator: Iterator[SegmentT[E, D, V, S] with S] = new AbstractIterator {

    import scala.language.unsafeNulls

    private var current: SegmentT[E, D, V, S] with S | Null = _

    override def hasNext: Boolean = current == null || !current.isFirst

    override def next(): SegmentT[E, D, V, S] with S = (current: SegmentT[E, D, V, S]) match {
      case null =>
        current = self
        self
      case s: SegmentT.WithPrev[E, D, V, S] =>
        val p = s.movePrev
        current = p
        p
      case _ =>
        SegmentSeqExceptionUtil.throwNoPrevSegment(current)
    }
  }

  /** @return [[LazyList]] of all next segments of sequence starting from current. */
  def forwardLazyList: LazyList[SegmentT[E, D, V, S] with S]

  /** @return [[LazyList]] of all previous segments of sequence starting from current. */
  def backwardLazyList: LazyList[SegmentT[E, D, V, S] with S]

  // Transformation ----------------------------------------------------------- //
  /**
   * Returns sequence containing
   * <tr>- segment ([[ExtendedBound.BelowAll]], upper) -> value</tr>
   * <tr>- segments {(l,,i,,, u,,i,,) -> v,,i,,} of original sequence for which l,,i,, `>` upper</tr>
   * <tr>where</tr>
   * <tr>upper - upper bound of current segment;</tr>
   * <tr>value - value of current segment;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,.</tr>
   *
   * <h3>Example</h3>
   * {{{
   * original:
   *               segment
   *                  v
   *   X--------](---------)[--------)[---------X
   *        A         B    ^    C         D        - values
   *                     upper
   *
   * segment.takeAbove:
   *
   *   X-------------------)[--------)[---------X
   *            B               C         D        - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.getSegmentForBound(bound).takeAbove == sequence.takeAboveBound(bound)
   *   for any bound
   *
   *   2. segment.sequence == segment.takeBelow.appendAboveBound(bound, segment.takeAbove)
   *   for any bound such that segment.containsBound(bound) == true
   *
   *   3. segment.sequence == segment.takeAbove.prependBelowBound(bound, segment.takeBelow)
   *   for any bound such that segment.containsBound(bound) == true
   * }}}
   */
  def takeAbove: SegmentSeq[E, D, V]

  /**
   * Returns sequence containing
   * <tr>- segments {(l,,i,,, u,,i,,) -> v,,i,,} of original sequence for which u,,i,, `<` lower</tr>
   * <tr>- segment (lower, [[ExtendedBound.AboveAll]]) -> value</tr>
   * <tr>where</tr>
   * <tr>lower - lower bound of current segment;</tr>
   * <tr>value - value of current segment;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,.</tr>
   *
   * <h3>Example</h3>
   * {{{
   * original:
   *                segment
   *                   v
   *   X--------](---------)[--------)[---------X
   *        A    ^    B         C         D        - values
   *           lower
   * 
   * segment.takeBelow:
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.getSegmentForBound(bound).takeBelow == sequence.takeBelowBound(bound)
   *   for any bound
   *
   *   2. segment.sequence == segment.takeBelow.appendAboveBound(bound, segment.takeAbove)
   *   for any bound such that segment.containsBound(bound) == true
   *
   *   3. segment.sequence == segment.takeAbove.prependBelowBound(bound, segment.takeBelow)
   *   for any bound such that segment.containsBound(bound) == true
   * }}}
   */
  def takeBelow: SegmentSeq[E, D, V]

  /**
   * Returns tuple of sequences: ([[takeBelow]], [[takeAbove]]).
   *
   * <h3>Example</h3>
   * {{{
   * original:
   *               segment
   *                  v
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * segment.slice._1:
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   *
   * segment.slice._2:
   *
   *   X-------------------)[--------)[---------X
   *            B               C         D        - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.getSegmentForBound(bound).slice == sequence.sliceAtBound(bound)
   *   for any bound
   *
   *   2. segment.sequence == segment.slice._1.appendAboveBound(bound, segment.slice._2)
   *   for any bound such that segment.containsBound(bound) == true
   *
   *   3. segment.sequence == segment.slice._2.prependBelowBound(bound, segment.slice._1)
   *   for any bound such that segment.containsBound(bound) == true
   * }}}
   */
  def slice: (SegmentSeq[E, D, V], SegmentSeq[E, D, V])

  /**
   * Returns sequence containing
   * <tr>
   *   - segments {(l,,i,,, min(u,,i,,, F(lower))) -> v,,i,,} of `other` sequence for which l,,i,, `<` lower
   * </tr>
   * <tr>
   *   - segments {(l,,i,,, u,,i,,) -> v,,i,,} of original sequence for which l,,i,, `≥` lower
   * </tr>
   * <tr>where</tr>
   * <tr>lower - lower bound of current segment;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,;</tr>
   * <tr>F - flip operator (see [[Bound.flip]])</tr>
   *
   * <h3>Example</h3>
   * {{{
   * original:
   *                                   segment
   *                                      v
   *   X--------](------------------)[---------X
   *        A              B         ^    C        - values
   *                               lower
   *
   * other:
   *
   *   X---)[-----------)[---)[-----------)[---X
   *     D        E        F        G        H     - values
   *
   * segment.prepend(other):
   *
   *   X---)[-----------)[---)[-----)[---------X
   *     D        E        F     G        C        - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. If segment has previous segment:
   *   segment.prepend(other) == segment.sequence.prependBelowBound(segment.lower, other)
   *   for any `other` sequence
   *
   *   2. If segment is first:
   *   segment.prepend(other) == segment.sequence
   *   for any `other` sequence
   * }}}
   */
  def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V]

  /**
   * Returns sequence containing
   * <tr>
   *   - segments {(l,,i,,, u,,i,,) -> v,,i,,} of original sequence for which u,,i,, `≤` upper
   * </tr>
   * <tr>
   *   - segments {(max(l,,i,,, F(upper)), u,,i,,) -> v,,i,,} of `other` sequence for which u,,i,, `>` upper
   * </tr>
   * <tr>where</tr>
   * <tr>upper - upper bound of current segment;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,.</tr>
   * <tr>F - flip operator (see [[Bound.flip]])</tr>
   *
   * <h3>Example</h3>
   * {{{
   * original:
   *       segment
   *          v
   *   X--------](------------------)[---------X
   *        A   ^          B              C        - values
   *          upper
   *
   * other:
   *
   *   X---)[-----------)[---)[-----------)[---X
   *     D        E        F        G        H     - values
   *
   * segment.append(other):
   *
   *   X--------](------)[---)[-----------)[---X
   *        A        E     F        G        H     - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. If segment has next segment:
   *   segment.append(other) == segment.sequence.appendAboveBound(segment.upper, other)
   *   for any `other` sequence
   *
   *   2. If segment is last:
   *   segment.append(other) == segment.sequence
   *   for any `other` sequence
   * }}}
   */
  def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V]

  /**
   * Returns sequence containing
   * <tr>
   *   - segments {(l,,i,,, u,,i,,) -> v,,i,,} 
   *   of original sequence for which u,,i,, `<` lower
   * </tr>
   * <tr>
   *   - segments {(max(lower, l,,i,,), min(upper, u,,i,,)) -> v,,i,,}
   *   of `other` sequence for which l,,i,, `≤` upper and u,,i,, `≥` lower
   * </tr>
   * <tr>
   *   - segments {(l,,i,,, u,,i,,) -> v,,i,,}
   *   of original sequence for which l,,i,, `>` upper
   * </tr>
   * <tr>where</tr>
   * <tr>lower - lower bound of current segment;</tr>
   * <tr>upper - upper bound of current segment;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,.</tr>
   *
   * <h3>Example</h3>
   * {{{
   * original:
   *                    segment
   *                       v
   *   X--------](------------------)[---------X
   *        A    ^         B        ^     C        - values
   *           lower              upper
   *
   * other:
   *
   *   X---)[-----------)[---)[-----------)[---X
   *     D        E        F        G        H     - values
   *
   * segment.patch(other):
   *
   *   X--------](------)[---)[-----)[---------X
   *        A       E      F     G        C        - values
   * }}}
   */
  def patch(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V]

  /**
   * Returns lazy segment sequence which is equivalent to the result of [[patch]] operation.
   * Unlike [[patch]] it doesn't requires inserted sequence to be known immediately. 
   * Execution of `mapFunc` function is delayed until corresponding segment is requested.
   */
  def flatMap(mapFunc: () => SegmentSeq[E, D, V]): SegmentSeq[E, D, V]

  /**
   * Returns instance that captures current segment and specified bound to perform further operations.
   */
  def truncation(bound: ExtendedBound[E]): SegmentTruncationT[E, D, V, S, SegmentT[E, D, V, S]]

  /**
   * Returns truncation of segment at its extended lower bound:
   * {{{
   *   segment.lowerTruncation == segment.truncation(segment.extendedLower)
   * }}}
   */
  def lowerTruncation: SegmentTruncationT[E, D, V, S, SegmentT[E, D, V, S]]

  /**
   * Returns truncation of segment at its extended upper bound:
   * {{{
   *   segment.upperTruncation == segment.truncation(segment.extendedUpper)
   * }}}
   */
  def upperTruncation: SegmentTruncationT[E, D, V, S, SegmentT[E, D, V, S]]

  /**
   * Maps values of original sequence (see [[SegmentSeqT.mapSegments]]) and returns mapped segment that spans
   * current segment.
   *
   * @see [[map]]
   */
  def mapSegments[U, S1 >: S](
    mapFunc: Segment[E, D, V] => U
  )(
    implicit valueOps: ValueOps[U]
  ): MappedSegment[E, D, V, U, S1] = {
    val originalSeq = sequence
    MappedOrderedMap.mapSegment(
      self, mapFunc
    )(
      originalSeq.domainOps, valueOps, originalSeq.rngManager
    )
  }

  /**
   * Maps values of original sequence (see [[SegmentSeqT.mapSegments]]) and returns mapped segment that spans
   * current segment.
   *
   * @see [[mapSegments]]
   */
  def map[U, S1 >: S](
    mapFunc: V => U
  )(
    implicit valueOps: ValueOps[U]
  ): MappedSegment[E, D, V, U, S1] = {
    val originalSeq = sequence
    MappedValueOrderedMap.mapSegment(
      self, mapFunc
    )(
      originalSeq.domainOps, valueOps, originalSeq.rngManager
    )
  }
}