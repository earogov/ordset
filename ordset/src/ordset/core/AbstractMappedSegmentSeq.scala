package ordset.core

import ordset.core.domain.Domain
import ordset.core.internal.SegmentSeqExceptionUtil.*
import ordset.core.internal.mappedSeq.{MappedSegmentLikeT, MappedSegmentT}
import AbstractMappedSegmentSeq._

/**
 * {{{
 *         A              B                C
 * X-------------](--------------)[---------------X
 *         |              |               |
 *         V              V               V
 *
 *         A              A               D
 * X-----------------------------)[---------------X
 * }}}
 */
abstract class AbstractMappedSegmentSeq[E, D <: Domain[E], U, V, S]
  extends AbstractSegmentSeq[E, D, V, MappedSegmentBase[E, D, U, V, S]] {

  // Inspection --------------------------------------------------------------- //
  /** Original sequence to which mapping is applied. */
  val originalSeq: SegmentSeqT[E, D, U, S]

  /** Mapping function for segment. */
  val segmentMapFunc: Segment[E, D, U] => V

  final override def isEmpty: Boolean = isUniform && !isValueIncluded(firstSegmentInstance.value)

  final override def isUniversal: Boolean = isUniform && isValueIncluded(firstSegmentInstance.value)

  final override def isUniform: Boolean = firstSegmentInstance.isSingle

  final override def includesBound(bound: Bound[E]): Boolean = super.includesBound(bound)

  final override def includesExtended(bound: ExtendedBound[E]): Boolean = super.includesExtended(bound)

  final override def includesElement(element: E): Boolean = super.includesElement(element)

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = super.upperBounds

  final override def extendedUpperBounds: Iterable[ExtendedBound.Upper[E]] = super.extendedUpperBounds

  final override def firstSegment: MappedFirstSegment[E, D, U, V, S] =
    firstSegmentInstance

  final override def lastSegment: MappedLastSegment[E, D, U, V, S] =
    frontMapperLast(originalSeq.lastSegment)

  final override def getSegmentForBound(bound: Bound[E]): MappedSegment[E, D, U, V, S] =
    searchFrontMapper(frontMapperGeneral, originalSeq.getSegmentForBound(bound))

  final override def getSegmentForExtended(bound: ExtendedBound[E]): MappedSegment[E, D, U, V, S] =
    super.getSegmentForExtended(bound)

  final override def getSegmentForElement(element: E): MappedSegment[E, D, U, V, S] =
    super.getSegmentForElement(element)

  override def getValueForBound(bound: Bound[E]): V =
    segmentMapFunc(originalSeq.getSegmentForBound(bound))

  override def getValueForExtended(bound: ExtendedBound[E]): V =
    segmentMapFunc(originalSeq.getSegmentForExtended(bound))

  override def getValueForElement(element: E): V =
    segmentMapFunc(originalSeq.getSegmentForElement(element))

  // Transformation ----------------------------------------------------------- //
  final override def takeAboveBound(bound: Bound[E]): SegmentSeq[E, D, V] = ???

  final override def takeAboveExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V] = ???

  final override def takeBelowBound(bound: Bound[E]): SegmentSeq[E, D, V] = ???

  final override def takeBelowExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V] = ???

  final override def sliceAtBound(bound: Bound[E]): (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) = ???

  final override def sliceAtExtended(bound: ExtendedBound[E]): (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) = ???

  final override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  final override def prependBelowBound(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  final override def prependBelowExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  final override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  final override def appendAboveBound(bound: Bound[E], other: SegmentSeq[E, D, V]): LazySegmentSeq[E, D, V] = ???

  final override def appendAboveExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  final override def patchLazy(supplierSeq: SupplierSegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    patchLazyDelayedInternal(supplierSeq)

  // Protected section -------------------------------------------------------- //
  /**
   * Returns `true` if segment with given value is considered to be included in set.
   *
   * For example, if `V` = `Option[AnyType]`, then we assume `None` is not included and `Some(anyValue)` - is included.
   */
  protected def isValueIncluded(value: V): Boolean

  /**
   * Creates mapped segment sequence.
   */
  protected def cons(original: SegmentSeq[E, D, U]): SegmentSeq[E, D, V]

  protected override def consUniform(value: V): SegmentSeq[E, D, V]

  /**
   * First segment of sequence. It's either initial ot single.
   */
  protected final lazy val firstSegmentInstance: MappedFirstSegment[E, D, U, V, S] =
    searchFrontMapper(frontMapperFirst, originalSeq.firstSegment)

  /**
   * Returns mapped segment for segment of original sequence.
   */
  protected def mapSegment(segment: SegmentT[E, D, U, S]): MappedSegment[E, D, U, V, S] = 
    searchFrontMapper(frontMapperGeneral, segment)
  
  /**
   * Returns mapped truncation for truncation of original sequence.
   */
  protected def mapTruncation(
    truncation: SegmentTruncationT[E, D, U, S, SegmentT[E, D, U, S]]
  ): MappedTruncation[E, D, U, V, S] =
    mapSegment(truncation.segment).truncation(truncation.bound)

  /**
   * Preconditions:
   *
   * 1. `original` segment belongs to `sequence.originalSeq`.
   *
   * 2. `original` segment must define front bound of mapped segment
   *    (mapped value must change for next original segment).
   *
   * @return mapped segment for specified `original` segment.
   */
  protected final def frontMapperGeneral(
    original: SegmentT[E, D, U, S]
  ): MappedSegment[E, D, U, V, S] =
    if (firstSegmentInstance.isSpecifiedBy(original))
      firstSegmentInstance
    else
      // `original` defines front bound of mapped segment which:
      // - isn't first segment (condition was checked above);
      // => `original` has previous segment => cast is safe.
      frontMapperWithPrev(original.asInstanceOf[SegmentT.WithPrev[E, D, U, S]])

  /**
   * Preconditions:
   *
   * 1. All preconditions of [[frontMapperGeneral]] method.
   *
   * 2. `original` segment must belong to first mapped segment, i.e. it
   *    must define its front bound.
   *
   * @return first mapped segment for specified `original` segment.
   */
  protected final def frontMapperFirst(
    original: SegmentT[E, D, U, S]
  ): MappedFirstSegment[E, D, U, V, S] =
    original match {
      case n: SegmentT.WithNext[E, D, U, S] => MappedInitialSegment(this, n)
      case l: SegmentT.Last[E, D, U, S] => MappedSingleSegment(this, l)
      case _ => throwSegmentMustBeLastOrWithNext(original) // just to remove warning
    }

  /**
   * Preconditions:
   *
   * 1. All preconditions of [[frontMapperGeneral]] method.
   *
   * 2. `original` segment must be last segment of original sequence.
   *
   * @return last mapped segment for specified `original` segment.
   */
  protected final def frontMapperLast(
    original: SegmentT.Last[E, D, U, S]
  ): MappedLastSegment[E, D, U, V, S] =
    if (firstSegmentInstance.isSpecifiedBy(original))
      // First mapped segment is single if it's specified by last original segment => cast is safe.
      firstSegmentInstance.asInstanceOf[MappedSingleSegment[E, D, U, V, S]]
    else
      // `original` defines front bound of mapped segment which:
      // - isn't single segment (condition was checked above);
      // => `original` is terminal segment => cast is safe.
      MappedTerminalSegment(this, original.asInstanceOf[SegmentT.Terminal[E, D, U, S]])

  /**
   * Preconditions:
   *
   * 1. All preconditions of [[frontMapperGeneral]] method.
   *
   * 2. `original` segment must define front bound of mapped segment
   *    which is not first segment of mapped sequence.
   *
   * @return mapped segment which has previous mapped segment for specified
   *         `original` segment.
   */
  protected final def frontMapperWithPrev(
    original: SegmentT.WithPrev[E, D, U, S]
  ): MappedSegmentWithPrev[E, D, U, V, S] =
    original match {
      case n: SegmentT.Inner[E, D, U, S] => MappedInnerSegment(this, n)
      case l: SegmentT.Terminal[E, D, U, S] => MappedTerminalSegment(this, l)
      case _ => throwSegmentMustBeLastOrWithNext(original) // just to remove warning
    }

  /**
   * Preconditions:
   *
   * 1. All preconditions of [[frontMapperGeneral]] method.
   *
   * 2. `original` segment must define front bound of mapped segment
   *    which is not last segment of mapped sequence.
   *
   * @return mapped segment which has next mapped segment for specified
   *         `original` segment.
   */
  protected final def frontMapperWithNext(
    original: SegmentT.WithNext[E, D, U, S]
  ): MappedSegmentWithNext[E, D, U, V, S] =
    if (firstSegmentInstance.isSpecifiedBy(original))
      // First mapped segment is initial if original segment has next segment => cast is safe.
      firstSegmentInstance.asInstanceOf[MappedInitialSegment[E, D, U, V, S]]
    else
      // `original` defines front bound of mapped segment which:
      // - isn't first segment (condition was checked above);
      // - isn't last segment (by preconditions);
      // => `original` is inner segment => cast is safe.
      MappedInnerSegment(this, original.asInstanceOf[SegmentT.Inner[E, D, U, S]])

  @inline
  protected final def supplyMapper[Seg <: SegmentT[E, D, U, S], R](
    supplied: Seg => R,
    composed: (Seg => R, Seg) => R
  ): Seg => R =
    original => composed(supplied, original)

  /**
   * Preconditions:
   *
   * 1. `original` segment belongs to `sequence.originalSeq`.
   *
   * Starting from `original` segment function moves forward (getting next segments of original sequence) until
   * meets change of the mapped value. In that case it stops and builds mapped segment with `mapper` function
   * for last original segment before value change.
   * {{{
   *
   *              original       ->     output original
   *                 V                        V
   *       -------------|-------------|-------------|-------------
   *              A           B              C      |     D         - segment value
   *                                                |
   *                               output mapped    |     next mapped
   *                                     V          |         V
   *       -----------------------------------------|-------------
   *                           B                    |      C        - mapped value
   *                                           upper bound
   * }}}
   */
  protected def searchFrontMapper[Seg >: SegmentT.WithPrev[E, D, U, S] <: SegmentT[E, D, U, S], R](
    mapper: Seg => R,
    original: Seg
  ): R =
    if (original.isLast) mapper(original)
    else {
      var run = true
      var currOriginal = original
      var currValue: V = segmentMapFunc(original)
      while (run) {
        currOriginal match {
          case s: SegmentT.WithNext[E, D, U, S] =>
            val nextOriginal = s.moveNext
            val nextValue = segmentMapFunc(nextOriginal)
            run = valueOps.eqv(currValue, nextValue)
            if (run) {
              currOriginal = nextOriginal
              currValue = nextValue
            }
          case _ =>
            run = false
        }
      }
      mapper(currOriginal)
    }

  /**
   * Preconditions:
   *
   * 1. `original` segment belongs to `sequence.originalSeq`.
   *
   * Starting from `original` segment function moves backward (getting previous segments of original sequence) until
   * meets change of the mapped value. In that case it stops and builds mapped segment with `mapper` function
   * for last original segment before value change.
   * {{{
   *
   *          output original       <-     original
   *                 V                        V
   *       -------------|-------------|-------------|-------------
   *              A     |     B              C            D         - segment value
   *                    |
   *       prev mapped  |          output mapped
   *                    |                V
   *       -------------------------------------------------------
   *          A         |                    B                      - mapped value
   *               lower bound
   * }}}
   */
  protected final def searchBackMapper[Seg >: SegmentT.WithNext[E, D, U, S] <: SegmentT[E, D, U, S], R](
    mapper: Seg => R,
    original: Seg
  ): R =
    if (original.isFirst) mapper(original)
    else {
      var run = true
      var currOriginal = original
      var currValue: V = segmentMapFunc(original)
      while (run) {
        currOriginal match {
          case s: SegmentT.WithPrev[E, D, U, S] =>
            val prevOriginal = s.movePrev
            val prevValue = segmentMapFunc(prevOriginal)
            run = valueOps.eqv(currValue, prevValue)
            if (run) {
              currOriginal = prevOriginal
              currValue = prevValue
            }
          case _ =>
            run = false
        }
      }
      mapper(currOriginal)
    }

  /**
   * Preconditions:
   *
   * 1. `original` segment belongs to `sequence.originalSeq`.
   *
   * Starting from `original` segment function get next segment of original sequence and builds mapped segment
   * with `mapper` function.
   *
   * If `original` is the last segment of sequence then `mapper` function is applied to it.
   */
  protected final def stepForwardMapper[R](
    mapper: Mapper[E, D, U, V, S, R],
    original: SegmentT[E, D, U, S]
  ): R =
    original match {
      case n: SegmentT.WithNext[E, D, U, S] => stepForwardWithNextMapper(mapper, n)
      case _ => mapper(original)
    }

  /**
   * Preconditions:
   *
   * 1. `original` segment belongs to `sequence.originalSeq`.
   *
   * Starting from `original` segment function get previous segment of original sequence and builds mapped segment
   * with `mapper` function.
   *
   * If `original` is the first segment of sequence then `mapper` function is applied to it.
   */
  protected final def stepBackwardMapper[R](
    mapper: Mapper[E, D, U, V, S, R],
    original: SegmentT[E, D, U, S]
  ): R =
    original match {
      case p: SegmentT.WithPrev[E, D, U, S] => stepBackwardWithPrevMapper(mapper, p)
      case _ => mapper(original)
    }

  /**
   * Same as [[stepForwardMapper]] function but with stricter input types.
   */
  @inline
  protected final def stepForwardWithNextMapper[R](
    mapper: WithPrevMapper[E, D, U, V, S, R],
    original: SegmentT.WithNext[E, D, U, S]
  ): R =
    mapper(original.moveNext)

  /**
   * Same as [[stepBackwardMapper]] function but with stricter input types.
   */
  @inline
  protected final def stepBackwardWithPrevMapper[R](
    mapper: WithNextMapper[E, D, U, V, S, R],
    original: SegmentT.WithPrev[E, D, U, S]
  ): R =
    mapper(original.movePrev)
}

object AbstractMappedSegmentSeq {

  type MappedSegment[E, D <: Domain[E], U, V, S] =
    SegmentT[E, D, V, MappedSegmentBase[E, D, U, V, S]] with MappedSegmentBase[E, D, U, V, S]

  type MappedFirstSegment[E, D <: Domain[E], U, V, S] =
    SegmentT.First[E, D, V, MappedSegmentBase[E, D, U, V, S]] with MappedSegmentBase[E, D, U, V, S]

  type MappedLastSegment[E, D <: Domain[E], U, V, S] =
    SegmentT.Last[E, D, V, MappedSegmentBase[E, D, U, V, S]] with MappedSegmentBase[E, D, U, V, S]

  type MappedTruncation[E, D <: Domain[E], U, V, S] =
    SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], MappedSegment[E, D, U, V, S]]
  
  /**
   * Base trait for mapped segments.
   *
   * Upper and lower bounds of mapped segments are defined by change of new (mapped) value.
   * Mapped segment is specified by 'front' segment that corresponds to the upper bound.
   * {{{
   *
   *                                      front
   *                                        V
   *       -------------|-------------|-------------|-------------
   *              A     |     B              C      |     D         - original value
   *                    |                           |
   *       prev mapped  |          mapped (this)    |     next mapped
   *           V        |              V            |         V
   *       -------------|---------------------------|-------------
   *             A      |              B            |      C        - mapped value
   *                lower bound                upper bound
   * }}}
   *
   * Preconditions:
   *
   * 1. `front` segment belongs to `sequence.originalSeq`.
   */
  sealed trait MappedSegmentBase[E, D <: Domain[E], U, V, S]
    extends MappedSegmentLikeT[E, D, U, V, S, MappedSegmentBase[E, D, U, V, S]] {

    // Inspection --------------------------------------------------------------- //
    override def sequence: MappedSegmentSeq[E, D, U, V, S]

    override def isIncluded: Boolean = sequence.isValueIncluded(value)

    override def segmentMapFunc: Segment[E, D, U] => V = sequence.segmentMapFunc

    /**
     * @return `true` if given `segment` is the `front` segment of current mapped segment.
     */
    def isSpecifiedBy(segment: SegmentT[E, D, U, S]): Boolean =
      sequence.originalSeq.eq(segment.sequence) && sequence.domainOps.segmentUpperOrd.eqv(front, segment)

    // Navigation --------------------------------------------------------------- //
    /**
     * Returns front original segment.
     * {{{
     *
     *   front == S2
     *
     *        S1          S2
     *   ----------](-----------)[-------- original seq
     *                          |
     *   -----------------------)[-------- mapped seq
     *       current segment
     * }}}
     */
    def front: SegmentT[E, D, U, S]

    /**
     * Returns back original segment.
     * {{{
     *
     *   back == S1
     *
     *                    S1         S2
     *   ----------](-----------)[-------- original seq
     *              |
     *   ----------](--------------------- mapped seq
     *                   current segment
     * }}}
     */
    def back: SegmentT[E, D, U, S]

    override def moveToFirst: MappedFirstSegment[E, D, U, V, S] = sequence.firstSegment

    override def moveToLast: MappedLastSegment[E, D, U, V, S] = sequence.lastSegment

    override def moveToBound(bound: Bound[E]): MappedSegment[E, D, U, V, S] =
      sequence.searchFrontMapper(sequence.frontMapperGeneral, front.moveToBound(bound))

    override def moveToExtended(bound: ExtendedBound[E]): MappedSegment[E, D, U, V, S] =
      sequence.getSegmentForExtended(bound)

    override def moveToElement(element: E): MappedSegment[E, D, U, V, S] =
      sequence.getSegmentForElement(element)

    /**
     * Returns [[Iterable]] of all segments of original sequence starting from [[front]] to [[back]].
     * I.e. iterates over segments of original sequence which are inside current mapped segment.
     * {{{
     *
     *   iterable == {S1, S2, S3}
     *
     *              S1        S2        S3
     *   -----)[--------)[---------)[--------](------- original seq
     *
     *   -----)[-----------------------------](------- mapped seq
     *                 current segment
     * }}}
     */
    def originalForwardIterable: Iterable[SegmentT[E, D, U, S]] = {
      val b = back
      val ord = domainOps.segmentUpperOrd
      front.forwardIterable.takeWhile(ord.lteqv(_, b))
    }

    /**
     * Returns [[LazyList]] of all segments of original sequence starting from [[front]] to [[back]].
     *
     * @see [[originalForwardIterable]]
     */
    def originalForwardLazyList: LazyList[SegmentT[E, D, U, S]] = {
      val b = back
      val ord = domainOps.segmentUpperOrd
      front.forwardLazyList.takeWhile(ord.lteqv(_, b))
    }

    /**
     * Returns [[Iterable]] of all segments of original sequence starting from [[back]] to [[front]].
     * I.e. iterates in inverse order over segments of original sequence which are inside current
     * mapped segment.
     * {{{
     *
     *   iterable == {S3, S2, S1}
     *
     *              S1        S2        S3
     *   -----)[--------)[---------)[--------](------- original seq
     *
     *   -----)[-----------------------------](------- mapped seq
     *                 current segment
     * }}}
     */
    def originalBackwardIterable: Iterable[SegmentT[E, D, U, S]] = {
      val f = front
      val ord = domainOps.segmentUpperOrd
      back.backwardIterable.takeWhile(ord.gteqv(_, f))
    }

    /**
     * Returns [[Iterable]] of all segments of original sequence starting from [[back]] to [[front]].
     *
     * @see [[originalBackwardLazyList]]
     */
    def originalBackwardLazyList: LazyList[SegmentT[E, D, U, S]] = {
      val f = front
      val ord = domainOps.segmentUpperOrd
      back.backwardLazyList.takeWhile(ord.gteqv(_, f))
    }

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: SegmentSeq[E, D, V] = ???

    override def takeBelow: SegmentSeq[E, D, V] = ???

    override def slice: (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) = ???

    override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

    override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

    /**
     * Returns truncation of original sequence at lower bound of mapped segment.
     * {{{
     *
     *   originalLowerTruncation - truncation with segment S1 and bound B1;
     *   originalUpperTruncation - truncation with segment S3 and bound B2.
     *
     *         B1                            B2
     *         [    S1        S2        S3   ]
     *   -----)[--------)[---------)[--------](------- original seq
     *
     *   -----)[-----------------------------](------- mapped seq
     *                 current segment
     * }}}
     */
    def originalLowerTruncation: SegmentTruncationT[E, D, U, S, SegmentT[E, D, U, S] with S] =
      back.self.lowerTruncation

    /**
     * Returns truncation of original sequence at upper bound of mapped segment.
     * {{{
     *
     *   originalLowerTruncation - truncation with segment S1 and bound B1;
     *   originalUpperTruncation - truncation with segment S3 and bound B2.
     *
     *         B1                            B2
     *         [    S1        S2        S3   ]
     *   -----)[--------)[---------)[--------](------- original seq
     *
     *   -----)[-----------------------------](------- mapped seq
     *                 current segment
     * }}}
     */
    def originalUpperTruncation: SegmentTruncationT[E, D, U, S, SegmentT[E, D, U, S] with S] =
      front.self.upperTruncation

    /**
     * Applies patch operation to original sequence within current mapped segment.
     *
     * Returns sequence containing
     * <tr>
     *   - segments {i ∈ [0, L-1]: (l,,i,,, u,,i,,) -> v,,i,,} of original sequence for which u,,i,, `<` lowerBound
     * </tr>
     * <tr>
     *   - segments {i ∈ [L, M-1]: (max(lowerBound, l,,i,,), min(upperBound, u,,i,,)) -> v,,i,,}
     *   of `other` sequence for which l,,i,, `≤` upperBound and u,,i,, `≥` lowerBound
     * </tr>
     * <tr>
     *   - segments {i ∈ [M, N-1]: (l,,i,,, u,,i,,) -> v,,i,,} of original sequence for which l,,i,, `>` upperBound
     * </tr>
     * <tr>where</tr>
     * <tr>lowerBound - lower bound of current mapped segment;</tr>
     * <tr>upperBound - upper bound of current mapped segment;</tr>
     * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
     * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
     * <tr>v,,i,, - value of segment S,,i,,.</tr>
     *
     * {{{
     *   original sequence:
     *
     *   X-----)[-------------)[--------------](-------X
     *      A          B               C           D
     *
     *   mapped sequence:
     *
     *   X-----)[-----------------------------](-------X
     *      A   ^               B             ^    D
     *         lower bound           upper bound
     *
     *   mapping function:
     *   A -> A    C -> B
     *   B -> B    D -> D
     *
     *   other sequence:
     *
     *   X---)[------------)[-------------------](-----X
     *     E         F                G             H
     *
     *   segment.patchOriginal(other):
     *
     *   X-----)[----------)[-----------------](-------X
     *      A         F              G             D
     * }}}
     */
    def patchOriginal(other: SegmentSeq[E, D, U]): SegmentSeq[E, D, U]

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT[E, D, U, S] = front
  }

  object MappedSegmentBase {

    trait TruncationBase[E, D <: Domain[E], U, V, S] {
      self: SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], MappedSegment[E, D, U, V, S]] =>

      override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

      override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???
    }
  }

  /**
   * Mapped segment with next segment.
   *
   * Segment is specified by 'front' segment that corresponds to the upper bound
   * (see preconditions of [[MappedSegmentBase]]).
   */
  sealed trait MappedSegmentWithNext[E, D <: Domain[E], U, V, S]
    extends MappedSegmentT.WithNext[E, D, U, V, S, MappedSegmentBase[E, D, U, V, S]]
      with MappedSegmentBase[E, D, U, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def self: MappedSegmentWithNext[E, D, U, V, S]

    override def upperBound: Bound.Upper[E] = front.upperBound

    // Navigation --------------------------------------------------------------- //
    override def front: SegmentT.WithNext[E, D, U, S]

    override def moveNext: MappedSegmentWithPrev[E, D, U, V, S] =
      sequence.stepForwardWithNextMapper(
        sequence.supplyMapper(
          sequence.frontMapperWithPrev,
          sequence.searchFrontMapper
        ),
        front
      )

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.WithNext[E, D, U, S] = front
  }

  /**
   * Mapped segment with previous segment.
   *
   * Segment is specified by 'front' segment that corresponds to the upper bound
   * (see preconditions of [[MappedSegmentBase]]).
   *
   * Lower bound is defined by `back` segment and is searched lazily.
   * {{{
   *
   *                          back         front
   *                           V             V
   *       -------------|-------------|-------------|-------------
   *              A     |     B              C      |     D         - original value
   *                    |                           |
   *                    |          mapped (this)    |
   *                    |              V            |
   *       -------------|---------------------------|-------------
   *             A                     B                  C         - mapped value
   * }}}
   *
   * Preconditions:
   *
   * 1. 'back' segment must have previous segment.
   *    This condition is equivalent to: mapped segment has previous segment.
   */
  sealed trait MappedSegmentWithPrev[E, D <: Domain[E], U, V, S]
    extends MappedSegmentT.WithPrev[E, D, U, V, S, MappedSegmentBase[E, D, U, V, S]]
      with MappedSegmentBase[E, D, U, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def self: MappedSegmentWithPrev[E, D, U, V, S]

    override def lowerBound: Bound.Lower[E] = back.lowerBound

    // Navigation --------------------------------------------------------------- //
    override def front: SegmentT.WithPrev[E, D, U, S]

    override lazy val back: SegmentT.WithPrev[E, D, U, S] = {
      val back = sequence.searchBackMapper[SegmentT[E, D, U, S], SegmentT[E, D, U, S]](
        identity, front
      )
      // Cast is safe if precondition 1 is provided.
      back.asInstanceOf[SegmentT.WithPrev[E, D, U, S]]
    }

    override def movePrev: MappedSegmentWithNext[E, D, U, V, S] =
      sequence.stepBackwardWithPrevMapper(sequence.frontMapperWithNext, back)

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.WithPrev[E, D, U, S] = front
  }

  /**
   * Initial segment of mapped sequence.
   *
   * Segment is specified by 'front' segment that corresponds to the upper bound
   * (see preconditions of [[MappedSegmentBase]]).
   */
  final case class MappedInitialSegment[E, D <: Domain[E], U, V, S](
    override val sequence: MappedSegmentSeq[E, D, U, V, S],
    override val front: SegmentT.WithNext[E, D, U, S]
  ) extends MappedSegmentT.Initial[E, D, U, V, S, MappedSegmentBase[E, D, U, V, S]]
    with MappedSegmentWithNext[E, D, U, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def self: MappedInitialSegment[E, D, U, V, S] = this

    // Navigation --------------------------------------------------------------- //
    override lazy val back: SegmentT.Initial[E, D, U, S] =
      // `back` is the first segment of original sequence. Let's prove that it's also initial segment
      // (has next segment). Two cases are possible:
      // 1. `front` is the same as `back`, i.e. `front` is the first segment of original sequence.
      //    By type definition `front` has next segment => cast is safe.
      // 2. `front` is ahead of `back` => `back` has next segment => cast is safe.
      front.moveToFirst.asInstanceOf[SegmentT.Initial[E, D, U, S]]

    override def moveToFirst: MappedInitialSegment[E, D, U, V, S] = this

    // Transformation ----------------------------------------------------------- //
    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], this.type ] =
      new MappedInitialSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], this.type] =
      SegmentTruncationT.upperTruncation(this)

    override def patchOriginal(other: SegmentSeq[E, D, U]): SegmentSeq[E, D, U] =
      originalUpperTruncation.prepend(other)
  }

  object MappedInitialSegment {

    final class Truncation[E, D <: Domain[E], U, V, S, +Seg <: MappedInitialSegment[E, D, U, V, S]](
      override val segment: Seg,
      inputBound: ExtendedBound[E]
    ) extends SegmentT.Initial.Truncation[E, D, V, MappedSegmentBase[E, D, U, V, S], Seg](
      segment,
      inputBound
    ) with MappedSegmentBase.TruncationBase[E, D, U, V, S]
  }

  /**
   * Terminal segment of mapped sequence.
   *
   * Segment is specified by 'front' segment that must be the terminal segment of original sequence
   * (see preconditions of [[MappedSegmentBase]]).
   */
  final case class MappedTerminalSegment[E, D <: Domain[E], U, V, S](
    override val sequence: MappedSegmentSeq[E, D, U, V, S],
    override val front: SegmentT.Terminal[E, D, U, S]
  ) extends MappedSegmentT.Terminal[E, D, U, V, S, MappedSegmentBase[E, D, U, V, S]]
    with MappedSegmentWithPrev[E, D, U, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isSpecifiedBy(segment: SegmentT[E, D, U, S]): Boolean =
      sequence.originalSeq.eq(segment.sequence) && segment.isLast

    override def self: MappedTerminalSegment[E, D, U, V, S] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: MappedTerminalSegment[E, D, U, V, S] = this

    // Transformation ----------------------------------------------------------- //
    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], this.type ] =
      new MappedTerminalSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], this.type] =
      SegmentTruncationT.upperTruncation(this)

    override def patchOriginal(other: SegmentSeq[E, D, U]): SegmentSeq[E, D, U] =
      originalLowerTruncation.append(other)
  }

  object MappedTerminalSegment {

    final class Truncation[E, D <: Domain[E], U, V, S, +Seg <: MappedTerminalSegment[E, D, U, V, S]](
      override val segment: Seg,
      inputBound: ExtendedBound[E]
    ) extends SegmentT.Terminal.Truncation[E, D, V, MappedSegmentBase[E, D, U, V, S], Seg](
      segment,
      inputBound
    ) with MappedSegmentBase.TruncationBase[E, D, U, V, S]
  }

  /**
   * Inner segment of mapped sequence.
   *
   * Segment is specified by 'front' segment that corresponds to the upper bound
   * (see preconditions of [[MappedSegmentBase]]).
   */
  final case class MappedInnerSegment[E, D <: Domain[E], U, V, S](
    override val sequence: MappedSegmentSeq[E, D, U, V, S],
    override val front: SegmentT.Inner[E, D, U, S]
  ) extends MappedSegmentT.Inner[E, D, U, V, S, MappedSegmentBase[E, D, U, V, S]]
    with MappedSegmentWithPrev[E, D, U, V, S]
    with MappedSegmentWithNext[E, D, U, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def self: MappedInnerSegment[E, D, U, V, S] = this

    // Transformation ----------------------------------------------------------- //
    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], this.type ] =
      new MappedInnerSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], this.type] =
      SegmentTruncationT.upperTruncation(this)

    override def patchOriginal(other: SegmentSeq[E, D, U]): SegmentSeq[E, D, U] =
      originalUpperTruncation.prepend(originalLowerTruncation.append(other))

    // Protected section -------------------------------------------------------- //
    protected override def original: SegmentT.Inner[E, D, U, S] = front
  }

  object MappedInnerSegment {

    final class Truncation[E, D <: Domain[E], U, V, S, +Seg <: MappedInnerSegment[E, D, U, V, S]](
      override val segment: Seg,
      inputBound: ExtendedBound[E]
    ) extends SegmentT.Inner.Truncation[E, D, V, MappedSegmentBase[E, D, U, V, S], Seg](
      segment,
      inputBound
    ) with MappedSegmentBase.TruncationBase[E, D, U, V, S]
  }

  /**
   * Single segment of mapped sequence.
   *
   * Segment is specified by 'front' segment that must be the last segment of original sequence
   * (see preconditions of [[MappedSegmentBase]]).
   */
  final case class MappedSingleSegment[E, D <: Domain[E], U, V, S](
    override val sequence: MappedSegmentSeq[E, D, U, V, S],
    override val front: SegmentT.Last[E, D, U, S]
  ) extends MappedSegmentT.Single[E, D, U, V, S, MappedSegmentBase[E, D, U, V, S]]
    with MappedSegmentBase[E, D, U, V, S] {

    // Inspection --------------------------------------------------------------- //
    override def isSpecifiedBy(segment: SegmentT[E, D, U, S]): Boolean =
      sequence.originalSeq.eq(segment.sequence) && segment.isLast

    override def self: MappedSingleSegment[E, D, U, V, S] = this

    // Navigation --------------------------------------------------------------- //
    override lazy val back: SegmentT.First[E, D, U, S] = front.moveToFirst

    override def moveToFirst: MappedSingleSegment[E, D, U, V, S] = this

    override def moveToLast: MappedSingleSegment[E, D, U, V, S] = this

    override def moveToBound(bound: Bound[E]): MappedSingleSegment[E, D, U, V, S] = this

    override def moveToExtended(bound: ExtendedBound[E]): MappedSingleSegment[E, D, U, V, S] = this

    override def moveToElement(element: E): MappedSingleSegment[E, D, U, V, S] = this

    // Transformation ----------------------------------------------------------- //
    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], this.type ] =
      new MappedSingleSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, MappedSegmentBase[E, D, U, V, S], this.type] =
      SegmentTruncationT.upperTruncation(this)

    override def patchOriginal(other: SegmentSeq[E, D, U]): SegmentSeq[E, D, U] = other
  }

  object MappedSingleSegment {

    final class Truncation[E, D <: Domain[E], U, V, S, +Seg <: MappedSingleSegment[E, D, U, V, S]](
      override val segment: Seg,
      inputBound: ExtendedBound[E]
    ) extends SegmentT.Single.Truncation[E, D, V, MappedSegmentBase[E, D, U, V, S], Seg](
      segment,
      inputBound
    ) with MappedSegmentBase.TruncationBase[E, D, U, V, S]
  }

  // Protected section -------------------------------------------------------- //
  protected type Mapper[E, D <: Domain[E], U, V, S, +R] =
    SegmentT[E, D, U, S] => R

  protected type WithNextMapper[E, D <: Domain[E], U, V, S, +R] =
    SegmentT.WithNext[E, D, U, S] => R

  protected type WithPrevMapper[E, D <: Domain[E], U, V, S, +R] =
    SegmentT.WithPrev[E, D, U, S] => R

  protected type ComposedMapper[E, D <: Domain[E], U, V, S, R] =
    (Mapper[E, D, U, V, S, R], SegmentT[E, D, U, S]) => R
}