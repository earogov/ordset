package ordset.core

import ordset.core.domain.Domain
import ordset.core.map.ZippedOrderedMap
import ordset.core.value.{InclusionPredicate, ValueOps}
import ordset.util.BooleanUtil

/**
 * Captures segment and bound to perform further operations.
 */
abstract class SegmentTruncationT[E, D <: Domain[E], V, +S, +Seg <: SegmentT[E, D, V, S]](
  val segment: Seg,
  inputBound: ExtendedBound[E],
) {

  // Inspection --------------------------------------------------------------- //
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
   * Segment sequence to which truncation belongs.
   */
  def sequence: SegmentSeqT[E, D, V, S] = segment.sequence

  override def toString: String = s"$segment.truncation($bound)"

  // Transformation ----------------------------------------------------------- //
  /**
   * Same as [[SegmentSeqT.prependBelowExtended]] applied to truncation bound and `other` sequence:
   * {{{
   *   truncation.prepend(other) == truncation.segment.sequence.prependBelowExtended(truncation.bound, other)
   * }}}
   * If segment is already known current method allows to avoid its repeated search in contrast to
   * [[SegmentSeqT.prependBelowExtended]].
   *
   * <h3>Example</h3>
   * {{{
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
   *
   * <h3>Example</h3>
   * {{{
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

  /**
   * Zips original sequence with `other` (see [[SegmentSeqT.zipOptimized]]) and returns truncation of zipped sequence
   * at bound [[bound]].
   *
   * @see [[zip]]
   */
  def zipOptimized[U, W, S1 >: S, S2](
    other: SegmentSeqT[E, D, U, S2],
    zipFunc: (V, U) => W,
    invariantFuncV: V => Boolean,
    invariantFuncU: U => Boolean
  )(
    implicit valueOps: ValueOps[W]
  ): ZippedTruncationT[E, D, V, U, W, S1, S2] = {
    val firstSeq = segment.sequence
    ZippedOrderedMap.zipFirstMapTruncation[E, D, V, U, W, S1, S2](
      this, other, zipFunc, invariantFuncV, invariantFuncU
    )(
      firstSeq.domainOps, valueOps, firstSeq.rngManager
    )
  }

  /**
   * Zips original sequence with `other` (see [[SegmentSeqT.zip]]) and returns truncation of zipped sequence
   * at bound [[bound]].
   *
   * Method is a simplified version of [[zipOptimized]] that doesn't require to specify invariant functions
   * using `false` predicate instead of them.
   *
   * @see [[zipOptimized]]
   */
  def zip[U, W, S1 >: S, S2](
    other: SegmentSeqT[E, D, U, S2],
    zipFunc: (V, U) => W,
  )(
    implicit valueOps: ValueOps[W]
  ): ZippedTruncationT[E, D, V, U, W, S1, S2] =
    zipOptimized(
      other, zipFunc, BooleanUtil.falsePredicate1, BooleanUtil.falsePredicate1
    )(
      valueOps
    )

  /**
   * Zips original sequence with `other` (see [[SegmentSeqT.zipIntoTuple]]) and returns truncation of zipped sequence
   * at bound [[bound]].
   *
   * Note that each segment of output sequence is considered to be included in set (see [[ValueOps.valueIncl]]).
   * <tr></tr>
   *
   * @see [[zip]]
   */
  def zipIntoTuple[U, W, S1 >: S, S2](other: SegmentSeqT[E, D, U, S2]): ZippedTruncationT[E, D, V, U, (V, U), S1, S2] =
    zip(
      other, (_, _)
    )(
      new ValueOps.Tuple2Impl(InclusionPredicate.alwaysIncluded, sequence.valueOps, other.valueOps)
    )

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
  @inline
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
  @inline
  protected [ordset] final def getSegmentForAppendingCaseSegmentWithPrev[E, D <: Domain[E], V, S](
    bound: ExtendedBound[E],
    segment: SegmentT.WithPrev[E, D, V, S] with S 
  ): SegmentT[E, D, V, S] with S =
    bound match {
      case bound: Bound.Lower[E] if segment.hasLowerBound(bound) => segment.movePrev
      case _ => segment
    }

  /**
   * Implementation of [[SegmentT.lowerTruncation]].
   */
  @inline
  protected [ordset] final def lowerTruncation[E, D <: Domain[E], V, S](
    segment: SegmentT[E, D, V, S]
  ): SegmentTruncationT[E, D, V, S, segment.type] =
    segment.truncation(segment.lowerExtended)

  /**
   * Implementation of [[SegmentT.upperTruncation]].
   */
  @inline
  protected [ordset] final def upperTruncation[E, D <: Domain[E], V, S](
    segment: SegmentT[E, D, V, S]
  ): SegmentTruncationT[E, D, V, S, segment.type] =
    segment.truncation(segment.upperExtended)
}
