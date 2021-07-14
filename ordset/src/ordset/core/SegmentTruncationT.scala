package ordset.core

import ordset.core.domain.Domain

/**
 * Captures segment and bound to perform further operations.
 */
abstract class SegmentTruncationT[E, D <: Domain[E], V, +S, +Seg <: SegmentLikeT[E, D, V, S]](
  val segment: Seg,
  inputBound: ExtendedBound[E],
) {

  /**
   * Truncation bound.
   *
   * Truncation bound equals to input bound limited by segment (see [[SegmentLikeT.restrictExtended]]).
   * So invariant is always provided:
   * {{{
   *   segment.containsExtended(bound) == true
   * }}}
   */
  final val bound: ExtendedBound[E] = segment.restrictExtended(inputBound)

  /**
   * Same as [[SegmentSeqT.prependBelowExtended]] applied to truncation bound and `other` sequence:
   * {{{
   *   truncation.prepend(other) == truncation.segment.sequence.prependBelowExtended(truncation.bound, other)
   * }}}
   * If segment is already known current method allows to avoid its repeated search in contrast to
   * [[SegmentSeqT.prependBelowExtended]].
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
   * segment.truncation(bound).prepend(other):
   *
   *                              bound
   *                                v
   *   X---)[----------------)[-----)[---------X
   *     D           E           F        C        - values
   * }}}
   *
   * <h3>Special case</h3>
   *
   * If truncation bound equals to the upper bound of [[segment]] the result is equivalent to [[prepend]]
   * applied to the next segment:
   * {{{
   *   segment.truncation(bound).prepend(other) == segment.moveNext.truncation(bound).prepend(other)
   *   if segment.hasUpperExtended(bound)
   * }}}
   * {{{
   *                    segment    bound   next segment
   *                       v        )        v
   *   X------------](--------------)[---------X
   *         A               B      ^     C        - values
   *                            upperBound
   * }}}
   *
   * [[getSegmentForPrepending]] returns next segment in special case and segment itself otherwise.
   */
  def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V]

  /**
   * Same as [[SegmentSeqT.appendAboveExtended]] applied to truncation bound and `other` sequence:
   * {{{
   *   truncation.append(other) == truncation.segment.sequence.appendAboveExtended(truncation.bound, other)
   * }}}
   * If segment is already known current method allows to avoid its repeated search in contrast to
   * [[SegmentSeqT.appendAboveExtended]].
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
   * segment.truncation(bound).append(other):
   *
   *           bound
   *             v
   *   X--------](-----------)[-----------)[---X
   *        A           E           F        G    - values
   * }}}
   *
   * <h3>Special case</h3>
   *
   * If truncation bound equals to the lower bound of [[segment]] the result is equivalent to [[append]]
   * applied to the previous segment:
   * {{{
   *   segment.truncation(bound).append(other) == segment.movePrev.truncation(bound).append(other)
   *   if segment.hasLowerExtended(bound)
   * }}}
   * {{{
   *   prev. segment   bound   segment
   *         v          (        v
   *   X---------------](-----------)[---------X
   *         A          ^     B           C       - values
   *                lowerBound
   * }}}
   *
   * [[getSegmentForAppending]] returns previous segment in special case and segment itself otherwise.
   */
  def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V]

  override def toString: String = s"$segment.truncation($bound)"

  // Protected section -------------------------------------------------------- //
  /**
   * Returns next segment in special case of [[prepend]] or segment itself otherwise.
   */
  protected def getSegmentForPrepending: SegmentT[E, D, V, S] with S

  /**
   * Returns previous segment in special of [[append]] or segment itself otherwise.
   */
  protected def getSegmentForAppending: SegmentT[E, D, V, S] with S
}

object SegmentTruncationT {
  
  // Internal utility methods ------------------------------------------------- //
  /**
   * Implementation of [[SegmentTruncationT.getSegmentForPrepending]] for case when [[SegmentTruncationT.segment]]
   * has next segment.
   */
  protected [ordset] final def getSegmentForPrependingCaseSegmentWithNext[E, D <: Domain[E], V, S](
    bound: ExtendedBound[E], 
    segment: SegmentT.WithNext[E, D, V, S] with S
  ): SegmentT[E, D, V, S] with S =
    bound match {
      case bound: Bound.Upper[E] if segment.hasUpperBound(bound.provideUpper) => segment.moveNext
      case _ => segment
    }

  /**
   * Implementation of [[SegmentTruncationT.getSegmentForAppending]] for case when [[SegmentTruncationT.segment]]
   * has previous segment.
   */
  protected [ordset] final def getSegmentForAppendingCaseSegmentWithPrev[E, D <: Domain[E], V, S](
    bound: ExtendedBound[E],
    segment: SegmentT.WithPrev[E, D, V, S] with S 
  ): SegmentT[E, D, V, S] with S =
    bound match {
      case bound: Bound.Lower[E] if segment.hasLowerBound(bound) => segment.movePrev
      case _ => segment
    }
}
