package ordset.core

import ordset.core.domain.Domain

/**
 * Captures segment and bound to perform further operations.
 */
abstract class SegmentTruncationT[E, D <: Domain[E], V, +S, +Seg <: SegmentLikeT[E, D, V, S]](
  val segment: Seg,
  inputBound: Bound[E],
) {

  /**
   * Truncation bound.
   *
   * Truncation bound equals to input bound limited by segment (see [[SegmentLikeT.restrictBound]]).
   * So invariant is always provided:
   * {{{
   *   segment.contains(bound) == true
   * }}}
   */
  final val bound: Bound[E] = segment.restrictBound(inputBound)

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
   * If truncation bound equals to upper bound of [[segment]] the result is equivalent to [[prepended]]
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
   * [[getPrependedBoundSegment]] returns next segment in degenerate case and segment itself otherwise.
   */
  def prepended(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V]

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
   * If truncation bound equals to lower bound of [[segment]] the result is equivalent to [[appended]]
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
   * [[getAppendedBoundSegment]] returns previous segment in degenerate case and segment itself otherwise.
   */
  def appended(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V]

  override def toString: String = s"$segment.truncation($bound)"

  // Protected section -------------------------------------------------------- //
  /**
   * Returns next segment in degenerate case (see [[prepended]]) or segment itself otherwise.
   */
  protected def getPrependedBoundSegment: SegmentT[E, D, V, S] with S

  /**
   * Returns previous segment in degenerate case (see [[appended]]) or segment itself otherwise.
   */
  protected def getAppendedBoundSegment: SegmentT[E, D, V, S] with S
}
