package ordset.core

import ordset.core
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.internal.SegmentSeqExceptionUtil
import ordset.core.value.ValueOps

import scala.Specializable.{AllNumeric => spNum}
import scala.collection.{AbstractIterable, AbstractIterator}
import scala.{specialized => sp}

/**
 * Non-sealed superclass of [[SegmentT]] 
 *
 * @tparam E type of element in ordered set
 * @tparam D type of elements domain
 * @tparam V type of new value assigned to interval of elements
 * @tparam S type of additional segment state
 */
trait SegmentLikeT[@sp(spNum) E, D <: Domain[E], @sp(Boolean) V, +S] {

  // Inspection --------------------------------------------------------------- //
  /** Sequence to which segment belongs. */
  def sequence: SegmentSeqT[E, D, V, S]

  /** Domain operations. */
  def domainOps: DomainOps[E, D] = sequence.domainOps

  /** Value operations (equality type class, etc). */
  def valueOps: ValueOps[V] = sequence.valueOps

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
  def containsElement(element: E): Boolean = containsBound(Bound.Upper.inclusive(element))

  /**
   * Get extended lower bound of segment:
   * <tr>- if segment is first returns [[ExtendedBound.BelowAll]];</tr>
   * <tr>- if segment has next segment returns its standard (limited) upper bound.</tr>
   */
  def lowerExtended: ExtendedBound.Lower[E]

  /**
   * Get extended upper bound of segment:
   * <tr>- if segment is last returns [[ExtendedBound.AboveAll]];</tr>
   * <tr>- if segment has previous segment returns its standard (limited) lower bound.</tr>
   */
  def upperExtended: ExtendedBound.Upper[E]

  /** @return `true` if segment has specified upper bound. */
  def hasUpperBound(bound: Bound.Upper[E]): Boolean

  /** @return `true` if segment has specified upper bound. */
  def hasUpperExtended(bound: ExtendedBound.Upper[E]): Boolean =
    bound match {
      case b: Bound.Upper[E] => hasUpperBound(b)
      case ExtendedBound.AboveAll => isLast
    }

  /** @return `true` if segment has specified lower bound. */
  def hasLowerBound(bound: Bound.Lower[E]): Boolean

  /** @return `true` if segment has specified lower bound. */
  def hasLowerExtended(bound: ExtendedBound.Lower[E]): Boolean =
    bound match {
      case b: Bound.Lower[E] => hasLowerBound(b)
      case ExtendedBound.BelowAll => isFirst
    }

  /**
   * If `bound` is outside of segment returns closest bound of segment (either lower or upper).
   * Otherwise returns `bound`.
   *
   * <h3>Note</h3>
   *
   * The formula below seems to be equivalent to method definition:
   *
   * output bound = min(max(`bound`, lower bound of segment), upper bound of segment)     (1)
   *
   * But there is a subtle difference: according to bound ordering defined for segment two bounds,
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
   * [[restrictBound]] must return `bound` = 5`]`
   * But implementation based on formula (1) can return either 5`]` or 5`[`.
   */
  def restrictBound(bound: Bound[E]): Bound[E]

  /**
   * If `bound` is outside of segment returns closest bound of segment (either lower or upper).
   * Otherwise returns `bound`.
   *
   * @see [[restrictBound]]
   */
  def restrictExtended(bound: ExtendedBound[E]): ExtendedBound[E] =
    bound match {
      case b: Bound[E] => restrictBound(b)
      case ExtendedBound.BelowAll => lowerExtended
      case ExtendedBound.AboveAll => upperExtended
    }

  /** @return `true` if segment has specified value. */
  def hasValue(v: V): Boolean = valueOps.eqv(value, v)

  /** @return `true` if there is next segment after current, i.e. if it's [[SegmentT.WithNext]]. */
  def hasNext: Boolean = false

  /** @return `true` if there is previous segment before current, i.e. if it's [[SegmentT.WithPrev]]. */
  def hasPrev: Boolean = false

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
  def moveToElement(element: E): SegmentT[E, D, V, S] with S = moveToBound(Bound.Upper.inclusive(element))

  /** @return [[Iterable]] of all next segments of sequence starting from current. */
  def forwardIterable: Iterable[SegmentT[E, D, V, S] with S] = new AbstractIterable {

    override def iterator: Iterator[SegmentT[E, D, V, S] with S] = forwardIterator
  }

  /** @return [[Iterator]] of all next segments of sequence starting from current. */
  def forwardIterator: Iterator[SegmentT[E, D, V, S] with S] = new AbstractIterator {

    private var current: SegmentT[E, D, V, S] with S = _

    override def hasNext: Boolean = current == null || !current.isLast

    override def next(): SegmentT[E, D, V, S] with S = (current: SegmentT[E, D, V, S]) match {
      case null =>
        current = self
        current
      case s: SegmentT.WithNext[E, D, V, S] =>
        current = s.moveNext
        current
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

    private var current: SegmentT[E, D, V, S] with S = _

    override def hasNext: Boolean = current == null || !current.isFirst

    override def next(): SegmentT[E, D, V, S] with S = (current: SegmentT[E, D, V, S]) match {
      case null =>
        current = self
        current
      case s: SegmentT.WithPrev[E, D, V, S] =>
        current = s.movePrev
        current
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
   * <tr>- segment (minBound, u,,0,,) -> v,,0,,</tr>
   * <tr>- segments {i > 0: (l,,i,,, u,,i,,) -> v,,i,,} of original sequence for which l,,i,, `>` u,,0,,</tr>
   * <tr>where</tr>
   * <tr>minBound - minimal bound of domain;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,;</tr>
   * <tr>S,,0,, - current segment.</tr>
   * {{{
   *
   * original:
   *                segment
   *                   v
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
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
   * <tr>- segments {i ∈ [0, N-1]: (l,,i,,, u,,i,,) -> v,,i,,} of original sequence for which u,,i,, `<` l,,N,,</tr>
   * <tr>- segment (l,,N,,, maxBound) -> v,,N,,</tr>
   * <tr>where</tr>
   * <tr>maxBound - maximal bound of domain;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,;</tr>
   * <tr>S,,N,, - current segment.</tr>
   * {{{
   *
   * original:
   *                segment
   *                   v
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
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
   * {{{
   *
   * original:
   *                segment
   *                   v
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
   *   - segments {i ∈ [0, M]: (l,,i,,, u,,i,,) -> v,,i,,}
   *   of `other` sequence for which u,,i,, `<` lowerBound
   * </tr>
   * <tr>
   *   - segments {i ∈ [M+1, N]: (l,,i,,, u,,i,,) -> v,,i,,}
   *   of original sequence for which l,,i,, `≥` lowerBound
   * </tr>
   * <tr>where</tr>
   * <tr>lowerBound - lower bound of current segment;</tr>
   * <tr>upperBound - upper bound of current segment;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,.</tr>
   * {{{
   *
   * original:
   *                                    segment
   *                                      v
   *   X--------](------------------)[---------X
   *        A              B         ^    C        - values
   *                             lowerBound
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
   *   segment.prepend(other) == segment.sequence.prependBelowBound(segment.lowerBound, other)
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
   *   - segments {i ∈ [0, M]: (l,,i,,, u,,i,,) -> v,,i,,}
   *   of original sequence for which u,,i,, `≤` upperBound
   * </tr>
   * <tr>
   *   - segments {i ∈ [M+1, N]: (l,,i,,, u,,i,,) -> v,,i,,}
   *   of `other` sequence for which l,,i,, `>` upperBound
   * </tr>
   * <tr>where</tr>
   * <tr>lowerBound - lower bound of current segment;</tr>
   * <tr>upperBound - upper bound of current segment;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,.</tr>
   * {{{
   *
   * original:
   *       segment
   *          v
   *   X--------](------------------)[---------X
   *        A   ^          B              C        - values
   *        upperBound
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
   *   segment.append(other) == segment.sequence.appendAboveBound(segment.upperBound, other)
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
   *   - segments {i ∈ [0, L-1]: (l,,i,,, u,,i,,) -> v,,i,,}
   *   of original sequence for which u,,i,, `<` lowerBound
   * </tr>
   * <tr>
   *   - segments {i ∈ [L, M-1]: (max(lowerBound, l,,i,,), min(upperBound, u,,i,,)) -> v,,i,,}
   *   of `other` sequence for which l,,i,, `≤` upperBound and u,,i,, `≥` lowerBound
   * </tr>
   * <tr>
   *   - segments {i ∈ [M, N-1]: (l,,i,,, u,,i,,) -> v,,i,,}
   *   of original sequence for which l,,i,, `>` upperBound
   * </tr>
   * <tr>where</tr>
   * <tr>lowerBound - lower bound of current segment;</tr>
   * <tr>upperBound - upper bound of current segment;</tr>
   * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
   * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
   * <tr>v,,i,, - value of segment S,,i,,.</tr>
   * {{{
   *
   * original:
   *                    segment
   *                       v
   *   X--------](------------------)[---------X
   *        A    ^         B        ^     C        - values
   *         lowerBound         upperBound
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
   * Returns instance that captures current segment and specified bound to perform further operations.
   */
  def truncation(bound: ExtendedBound[E]): SegmentTruncationT[E, D, V, S, this.type]
}