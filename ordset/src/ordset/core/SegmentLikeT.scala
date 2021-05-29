package ordset.core

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps

import scala.Specializable.{AllNumeric => spNum}
import scala.collection.{AbstractIterable, AbstractIterator}
import scala.{specialized => sp}

trait SegmentLikeT[@sp(spNum) E, D <: Domain[E], @sp(Boolean) V, +S] {

  import SegmentLikeT._

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
  def contains(bound: Bound[E]): Boolean

  /** @return `true` if `element` is between segment bounds. */
  def containsElement(element: E): Boolean = contains(Bound.Upper.inclusive(element))

  /** @return `true` if segment has specified upper bound. */
  def hasUpperBound(bound: Bound.Upper[E]): Boolean

  /** @return `true` if segment has specified lower bound. */
  def hasLowerBound(bound: Bound.Lower[E]): Boolean

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

  override def toString: String = SetBuilderFormat.segment(self, (e: E) => e.toString, (v: V) => v.toString)

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

    override def next(): SegmentT[E, D, V, S] with S = (current: SegmentT[E, D, V, S]) match {
      case null =>
        current = self
        current
      case s: SegmentT.WithNext[E, D, V, S] =>
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

    override def next(): SegmentT[E, D, V, S] with S = (current: SegmentT[E, D, V, S]) match {
      case null =>
        current = self
        current
      case s: SegmentT.WithPrev[E, D, V, S] =>
        current = s.movePrev
        current
      case _ =>
        throw new NoSuchElementException(s"Segment $current doesn't have previous segment.")
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
  def patched(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V]

  /**
   * Returns instance that captures current segment and specified bound to perform further operations.
   */
  def truncation(bound: Bound[E]): Truncation[E, D, V, S, SegmentSeq[E, D, V]] = ???
}

object SegmentLikeT {

  /**
   * Captures segment and bound to perform further operations.
   */
  abstract class Truncation[E, D <: Domain[E], V, +S, +SSeq](
    val segment: SegmentT[E, D, V, S] with S,
    inputBound: Bound[E],
  ) {

    /**
     * Truncation bound.
     *
     * Truncation bound equals to input bound limited by [[restrictBound]]. So invariant is always provided:
     * {{{
     *   segment.contains(bound) == true
     * }}}
     */
    final val bound: Bound[E] = restrictBound(inputBound)

    /**
     * Same as [[SegmentSeqT.prepended]] applied to truncation bound and `other` sequence:
     * {{{
     *   truncation.prepended(other) == truncation.segment.sequence.prepended(truncation.bound, other)
     * }}}
     * If segment is already known current method allows to avoid its repeated search in contrast to
     * [[SegmentSeqT.prepended]].
     * {{{
     *
     * original:
     *                              bound   segment
     *                                )       v
     *   X--------](-----------)[----------------X
     *        A          B      ^        C           - values
     *                      lowerBound
     *
     * other:
     *
     *   X---)[----------------)[-----------)[---X
     *     D           E              F        G     - values
     *
     * segment.truncation(bound).prepended(other):
     *
     *                              bound
     *                                v
     *   X---)[----------------)[-----)[---------X
     *     D           E           F        C        - values
     * }}}
     *
     * <h3>Degenerate case</h3>
     *
     * If truncation bound equals to upper bound of [[segment]] the result is equivalent to [[Truncation.prepended]]
     * applied to the next segment:
     * {{{
     *   segment.truncation(bound).prepended(other) == segment.moveNext.truncation(bound).prepended(other)
     *   if segment.hasUpperBound(bound)
     * }}}
     * {{{
     *                    segment    bound   next segment
     *                       v        )        v
     *   X------------](--------------)[---------X
     *         A               B      ^     C        - values
     *                            upperBound
     * }}}
     *
     * [[Truncation.getPrependedBoundSegment]] returns next segment in degenerate case and segment itself otherwise.
     */
    def prepended(other: SegmentSeq[E, D, V]): SSeq = ???

    /**
     * Same as [[SegmentSeqT.appended]] applied to truncation bound and `other` sequence:
     * {{{
     *   truncation.appended(other) == truncation.segment.sequence.appended(truncation.bound, other)
     * }}}
     * If segment is already known current method allows to avoid its repeated search in contrast to
     * [[SegmentSeqT.appended]].
     * {{{
     *
     * original:
     *
     *   segment  bound
     *       v     (
     *   X------------](--------------)[---------X
     *         A      ^        B            C        - values
     *            upperBound
     *
     * other:
     *
     *   X---)[----------------)[-----------)[---X
     *     D           E              F        G     - values
     *
     * segment.truncation(bound).appended(other):
     *
     *           bound
     *             v
     *   X--------](-----------)[-----------)[---X
     *        A           E           F        G    - values
     * }}}
     *
     * <h3>Degenerate case</h3>
     *
     * If truncation bound equals to lower bound of [[segment]] the result is equivalent to [[Truncation.appended]]
     * applied to the previous segment:
     * {{{
     *   segment.truncation(bound).appended(other) == segment.movePrev.truncation(bound).appended(other)
     *   if segment.hasLowerBound(bound)
     * }}}
     * {{{
     *   prev. segment   bound   segment
     *         v          (        v
     *   X---------------](-----------)[---------X
     *         A          ^     B           C       - values
     *                lowerBound
     * }}}
     *
     * [[Truncation.getAppendedBoundSegment]] returns previous segment in degenerate case and segment itself otherwise.
     */
    def appended(other: SegmentSeq[E, D, V]): SSeq = ???

    // Protected section -------------------------------------------------------- //

    /**
     * Returns next segment in degenerate case (see [[Truncation.prepended]]) or segment itself otherwise.
     */
    protected def getPrependedBoundSegment: SegmentT[E, D, V, S] with S

    /**
     * Returns previous segment in degenerate case (see [[Truncation.appended]]) or segment itself otherwise.
     */
    protected def getAppendedBoundSegment: SegmentT[E, D, V, S] with S

    /**
     * Returns the bound:
     *
     * min(max(`bnd`, lower bound of [[segment]]), upper bound of [[segment]])
     *
     * I.e. if `bnd` is inside [[segment]] it's returned unchanged,
     * else closest bound of [[segment]] is returned (either lower or upper).
     */
    protected def restrictBound(bnd: Bound[E]): Bound[E]
  }
}