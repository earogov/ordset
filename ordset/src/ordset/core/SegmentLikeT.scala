package ordset.core

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps

import scala.Specializable.{AllNumeric => spNum}
import scala.collection.{AbstractIterable, AbstractIterator}
import scala.{specialized => sp}

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
  def isIncluded: Boolean

  /** @return `true` if `bound` is between segment bounds. */
  def contains(bound: Bound[E]): Boolean = this match {
    case s: Segment.Inner[E, D, V] =>
      domainOps.boundOrd.lteqv(s.lowerBound, bound) && domainOps.boundOrd.gteqv(s.upperBound, bound)
    case s: Segment.WithPrev[E, D, V] =>
      domainOps.boundOrd.lteqv(s.lowerBound, bound)
    case s: Segment.WithNext[E, D, V] =>
      domainOps.boundOrd.gteqv(s.upperBound, bound)
    case _ =>
      true
  }

  /** @return `true` if `element` is between segment bounds. */
  def containsElement(element: E): Boolean = contains(Bound.Upper.inclusive(element))

  /** @return `true` if there is next segment after current. */
  def hasNext: Boolean = false

  /** @return `true` if there is previous segment before current. */
  def hasPrev: Boolean = false

  /** @return `true` if segment has specified upper bound. */
  def hasUpperBound(bound: Bound.Upper[E]): Boolean = false

  /** @return `true` if segment has specified lower bound. */
  def hasLowerBound(bound: Bound.Lower[E]): Boolean = false

  /** @return `true` if segment has specified value. */
  def hasValue(v: V): Boolean = valueOps.eqv(value, v)

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
  def interval: Interval[E, D] = this match {
    case s: Segment.Inner[E, D, V]    => domainOps.interval(s.lowerBound, s.upperBound)
    case s: Segment.WithPrev[E, D, V] => domainOps.interval(s.lowerBound)
    case s: Segment.WithNext[E, D, V] => domainOps.interval(s.upperBound)
    case _                            => domainOps.interval.universal
  }

  /** @return tuple of segment value and interval that corresponds to the segment in given domain. */
  def intervalRelation: IntervalRelation[E, D, V] = IntervalRelation(interval, value)

  override def toString: String = SetBuilderFormat.segment(self, (e: E) => e.toString, (v: V) => v.toString)

  // Navigation --------------------------------------------------------------- //

  /** @return first segment of sequence. */
  def moveToFirst: SegmentT.First[E, D, V, S] with S = sequence.firstSegment

  /** @return last segment of sequence. */
  def moveToLast: SegmentT.Last[E, D, V, S] with S = sequence.lastSegment

  /** @return segment which contains specified bound. */
  def moveTo(bound: Bound[E]): SegmentT[E, D, V, S] with S = sequence.getSegment(bound)

  /** @return [[Iterable]] of all next segments of sequence starting from current. */
  def forwardIterable: Iterable[SegmentT[E, D, V, S] with S] = new AbstractIterable {

    override def iterator: Iterator[SegmentT[E, D, V, S] with S] = forwardIterator
  }

  /** @return [[Iterator]] of all next segments of sequence starting from current. */
  def forwardIterator: Iterator[SegmentT[E, D, V, S] with S] = new AbstractIterator {

    private var current: SegmentT[E, D, V, S] with S = _

    override def hasNext: Boolean = current == null || !current.isLast

    override def next(): SegmentT[E, D, V, S] with S = current match {
      case null =>
        current = self
        current
      case s: SegmentT.WithNext[E, D, V, S] @unchecked =>
        current = s.moveNext
        current
      case _ =>
        throw new NoSuchElementException(s"Segment $current doesn't have next segment.")
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

    override def next(): SegmentT[E, D, V, S] with S = current match {
      case null =>
        current = self
        current
      case s: SegmentT.WithPrev[E, D, V, S] @unchecked =>
        current = s.movePrev
        current
      case _ =>
        throw new NoSuchElementException(s"Segment $current doesn't have previous segment.")
    }
  }

  /** @return [[LazyList]] of all next segments of sequence starting from current. */
  def forwardLazyList: LazyList[SegmentT[E, D, V, S] with S] = this match {
    case s: SegmentT.WithNext[E, D, V, S] => LazyList.cons(self, s.moveNext.forwardLazyList)
    case _                                => LazyList.cons(self, LazyList.empty)
  }

  /** @return [[LazyList]] of all previous segments of sequence starting from current. */
  def backwardLazyList: LazyList[SegmentT[E, D, V, S] with S] = this match {
    case s: SegmentT.WithPrev[E, D, V, S] => LazyList.cons(self, s.movePrev.backwardLazyList)
    case _                                => LazyList.cons(self, LazyList.empty)
  }

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
   * segment.takenAbove:
   *
   *   X-------------------)[--------)[---------X
   *            B               C         D        - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.getSegment(bound).takenAbove == sequence.takenAbove(bound)
   *   for any bound
   *
   *   2. segment.sequence == segment.takenBelow.appended(bound, segment.takenAbove)
   *   for any bound such that segment.contains(bound) == true
   *
   *   3. segment.sequence == segment.takenAbove.prepended(bound, segment.takenBelow)
   *   for any bound such that segment.contains(bound) == true
   * }}}
   */
  def takenAbove: SegmentSeq[E, D, V]

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
   * segment.takenBelow:
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.getSegment(bound).takenBelow == sequence.takenBelow(bound)
   *   for any bound
   *
   *   2. segment.sequence == segment.takenBelow.appended(bound, segment.takenAbove)
   *   for any bound such that segment.contains(bound) == true
   *
   *   3. segment.sequence == segment.takenAbove.prepended(bound, segment.takenBelow)
   *   for any bound such that segment.contains(bound) == true
   * }}}
   */
  def takenBelow: SegmentSeq[E, D, V]

  /**
   * Returns tuple of sequences: ([[takenBelow]], [[takenAbove]]).
   * {{{
   *
   * original:
   *                segment
   *                   v
   *   X--------](---------)[--------)[---------X
   *        A         B         C         D        - values
   *
   * segment.sliced._1:
   *
   *   X--------](------------------------------X
   *        A               B                      - values
   *
   * segment.sliced._2:
   *
   *   X-------------------)[--------)[---------X
   *            B               C         D        - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. sequence.getSegment(bound).sliced == sequence.sliced(bound)
   *   for any bound
   *
   *   2. segment.sequence == segment.sliced._1.appended(bound, segment.sliced._2)
   *   for any bound such that segment.contains(bound) == true
   *
   *   3. segment.sequence == segment.sliced._2.prepended(bound, segment.sliced._1)
   *   for any bound such that segment.contains(bound) == true
   * }}}
   */
  def sliced: (SegmentSeq[E, D, V], SegmentSeq[E, D, V])

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
   * segment.prepended(other):
   *
   *   X---)[-----------)[---)[-----)[---------X
   *     D        E        F     G        C        - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. If segment has previous segment:
   *   segment.prepended(other) == segment.sequence.prepended(segment.lowerBound, other)
   *   for any `other` sequence
   *
   *   2. If segment is first:
   *   segment.prepended(other) == segment.sequence
   *   for any `other` sequence
   * }}}
   */
  def prepended(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

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
   * segment.appended(other):
   *
   *   X--------](------)[---)[-----------)[---X
   *        A        E     F        G        H     - values
   * }}}
   * Methods definitions provide invariants:
   * {{{
   *   1. If segment has next segment:
   *   segment.appended(other) == segment.sequence.appended(segment.upperBound, other)
   *   for any `other` sequence
   *
   *   2. If segment is last:
   *   segment.appended(other) == segment.sequence
   *   for any `other` sequence
   * }}}
   */
  def appended(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

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
   * segment.patched(other):
   *
   *   X--------](------)[---)[-----)[---------X
   *        A       E      F     G        C        - values
   * }}}
   */
  def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = this match {
    case s: Segment.Inner[E, D, V]    => s.moveNext.prepended(s.movePrev.appended(other))
    case s: Segment.WithNext[E, D, V] => s.moveNext.prepended(other)
    case s: Segment.WithPrev[E, D, V] => s.movePrev.appended(other)
    case _                            => other
  }

  // Protected section -------------------------------------------------------- //
  protected def self: SegmentT[E, D, V, S] with S
}