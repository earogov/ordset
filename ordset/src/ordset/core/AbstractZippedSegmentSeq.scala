package ordset.core

import ordset.Order
import ordset.core.value.ValueOps
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.internal.lazySeq.LazySegmentSeqBuilder
import ordset.core.internal.SegmentSeqExceptionUtil.*
import AbstractZippedSegmentSeq.*

// TODO: class description.
abstract class AbstractZippedSegmentSeq[E, D <: Domain[E], U1, U2, V, S1, S2]
  extends AbstractSegmentSeq[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2]] {
  
  // Inspection --------------------------------------------------------------- //
  /** Original sequence to which zipping is applied. */
  val firstSeq: SegmentSeqT[E, D, U1, S1]

  /** Original sequence to which zipping is applied. */
  val secondSeq: SegmentSeqT[E, D, U2, S2]

  /**
   * Function combining values of `firstSeq` and `secondSeq` sequences and returning value of zipped sequence.
   */
  def operator(first: U1, second: U2): V

  /**
   * Function that returns `true` for value `x` of `firstSeq` sequence iff
   * {{{
   * Ǝ C ∀ y: operator(x, y) = C.
   * }}}
   * I.e. input value `x` is invariant if `operator` output doesn't depend on second argument.
   */
  def firstInvariant(x: U1): Boolean

  /**
   * Function that returns `true` for value `y` of `secondSeq` sequence iff
   * {{{
   * Ǝ C ∀ x: operator(x, y) = C.
   * }}}
   * I.e. input value `y` is invariant if `operator` output doesn't depend on first argument.
   */
  def secondInvariant(x: U2): Boolean

  final override def isEmpty: Boolean =
    firstSegmentInstance.isSingle && !isValueIncluded(firstSegmentInstance.value)

  final override def isUniversal: Boolean =
    firstSegmentInstance.isSingle && isValueIncluded(firstSegmentInstance.value)

  final override def isUniform: Boolean =
    firstSegmentInstance.isSingle

  final override def includesBound(bound: Bound[E]): Boolean =
    isValueIncluded(getSegmentValue(firstSeq.getSegmentForBound(bound), secondSeq.getSegmentForBound(bound)))

  final override def includesExtended(bound: ExtendedBound[E]): Boolean = super.includesExtended(bound)

  final override def includesElement(element: E): Boolean = super.includesElement(element)

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = super.upperBounds

  final override def extendedUpperBounds: Iterable[ExtendedBound.Upper[E]] = super.extendedUpperBounds

  final override def firstSegment: ZippedFirstSegment[E, D, U1, U2, V, S1, S2] = 
    firstSegmentInstance

  final override def lastSegment: ZippedLastSegment[E, D, U1, U2, V, S1, S2] =
    frontZipperLast(firstSeq.lastSegment, secondSeq.lastSegment)

  final override def getSegmentForBound(bound: Bound[E]): ZippedSegment[E, D, U1, U2, V, S1, S2] =
    searchFrontZipper(frontZipperGeneral, firstSeq.getSegmentForBound(bound), secondSeq.getSegmentForBound(bound))

  final override def getSegmentForExtended(bound: ExtendedBound[E]): ZippedSegment[E, D, U1, U2, V, S1, S2] =
    super.getSegmentForExtended(bound)

  final override def getSegmentForElement(element: E): ZippedSegment[E, D, U1, U2, V, S1, S2] =
    super.getSegmentForElement(element)

  final override def getValueForBound(bound: Bound[E]): V =
    operator(firstSeq.getValueForBound(bound), secondSeq.getValueForBound(bound))

  final override def getValueForExtended(bound: ExtendedBound[E]): V =
    operator(firstSeq.getValueForExtended(bound), secondSeq.getValueForExtended(bound))

  final override def getValueForElement(element: E): V =
    operator(firstSeq.getValueForElement(element), secondSeq.getValueForElement(element))

  // Transformation ----------------------------------------------------------- //
  final override def takeAboveBound(bound: Bound[E]): SegmentSeq[E, D, V] =
    cons(firstSeq.takeAboveBound(bound), secondSeq.takeAboveBound(bound))
    
  final override def takeAboveExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V] =
    super.takeAboveExtended(bound)

  final override def takeBelowBound(bound: Bound[E]): SegmentSeq[E, D, V] =
    cons(firstSeq.takeBelowBound(bound), secondSeq.takeBelowBound(bound))

  final override def takeBelowExtended(bound: ExtendedBound[E]): SegmentSeq[E, D, V] =
    super.takeBelowExtended(bound)

  final override def sliceAtBound(bound: Bound[E]): (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) =
    (takeBelowBound(bound), takeAboveBound(bound))

  final override def sliceAtExtended(bound: ExtendedBound[E]): (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) =
    super.sliceAtExtended(bound)

  final override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    prependBelowExtended(firstSegment.upper, other)

  final override def prependBelowBound(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    LazySegmentSeqBuilder.appendSeq(bound, other, this)(domainOps, valueOps, rngManager)

  final override def prependBelowExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    super.prependBelowExtended(bound, other)

  final override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    appendAboveExtended(lastSegment.lower, other)

  final override def appendAboveBound(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    LazySegmentSeqBuilder.appendSeq(bound, this, other)(domainOps, valueOps, rngManager)

  final override def appendAboveExtended(bound: ExtendedBound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    super.appendAboveExtended(bound, other)

  final override def patchLazy(supplierSeq: SupplierSegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
    patchLazyDelayedInternal(supplierSeq)

  // Protected section -------------------------------------------------------- //
  protected final override type SegmentInternal = ZippedSegment[E, D, U1, U2, V, S1, S2]

  protected override def isValueIncluded(value: V): Boolean

  /**
   * Creates zipped segment sequence.
   */
  protected def cons(first: SegmentSeq[E, D, U1], second: SegmentSeq[E, D, U2]): SegmentSeq[E, D, V]

  protected override def consUniform(value: V): UniformSegmentSeq[E, D, V]

  /**
   * First segment of sequence. It's either initial ot single.
   */
  protected final lazy val firstSegmentInstance: ZippedFirstSegment[E, D, U1, U2, V, S1, S2] =
    searchFrontZipper(frontZipperFirst, firstSeq.firstSegment, secondSeq.firstSegment)

  protected final override def prependBelowBoundInternal(
    bound: Bound[E],
    originalBoundSegment: ZippedSegment[E, D, U1, U2, V, S1, S2],
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] = {
    val otherBoundSegment = other.getSegmentForBound(bound.provideUpper)
    LazySegmentSeqBuilder.appendSegment(bound, otherBoundSegment, originalBoundSegment)(domainOps, valueOps, rngManager)
  }

  protected final override def prependBelowExtendedInternal(
    bound: ExtendedBound[E],
    originalBoundSegment: ZippedSegment[E, D, U1, U2, V, S1, S2],
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] =
    super.prependBelowExtendedInternal(bound, originalBoundSegment, other)

  protected def appendAboveBoundInternal(
    bound: Bound[E],
    originalBoundSegment: ZippedSegment[E, D, U1, U2, V, S1, S2],
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] = {
    val otherBoundSegment = other.getSegmentForBound(bound.provideLower)
    LazySegmentSeqBuilder.appendSegment(bound, originalBoundSegment, otherBoundSegment)(domainOps, valueOps, rngManager)
  }

  protected final override def appendAboveExtendedInternal(
    bound: ExtendedBound[E],
    originalBoundSegment: ZippedSegment[E, D, U1, U2, V, S1, S2],
    other: SegmentSeq[E, D, V]
  ): SegmentSeq[E, D, V] =
    super.appendAboveExtendedInternal(bound, originalBoundSegment, other)

  /**
   * Returns zipped truncation for truncation of first sequence.
   */
  protected def zipFirstSeqTruncation(
    firstTruncation: SegmentTruncationT[E, D, U1, S1, SegmentT[E, D, U1, S1]]
  ): ZippedTruncation[E, D, U1, U2, V, S1, S2] = {
    val zippedSegment = searchFrontZipper(
      frontZipperGeneral,
      firstTruncation.segment,
      secondSeq.getSegmentForExtended(firstTruncation.bound)
    )
    zippedSegment.truncation(firstTruncation.bound)
  }

  /**
   * Returns zipped truncation for truncation of second sequence.
   */
  protected def zipSecondSeqTruncation(
    secondTruncation: SegmentTruncationT[E, D, U2, S2, SegmentT[E, D, U2, S2]]
  ): ZippedTruncation[E, D, U1, U2, V, S1, S2] = {
    val zippedSegment = searchFrontZipper(
      frontZipperGeneral,
      firstSeq.getSegmentForExtended(secondTruncation.bound),
      secondTruncation.segment,
    )
    zippedSegment.truncation(secondTruncation.bound)
  }

  /**
   * Preconditions:
   *
   * 1. `left` segment belongs to `firstSeq` or `secondSeq` sequence.
   *    `right` segment belongs to `firstSeq` or `secondSeq` sequence other then sequence of `left`.
   *
   * @return value of zipped segment defined by `left` and `right` subsegments.
   */
  protected final def getSegmentValue(
    left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): V = {
    (castSegmentToFirstSeq(left), castSegmentToSecondSeq(right)) match {
      case (firstSegment: SegmentT[E, D, U1, S1], secondSegment: SegmentT[E, D, U2, S2]) =>
        operator(firstSegment.value, secondSegment.value)
      case _ =>
        (castSegmentToFirstSeq(right), castSegmentToSecondSeq(left)) match {
          case (firstSegment: SegmentT[E, D, U1, S1], secondSegment: SegmentT[E, D, U2, S2]) =>
            operator(firstSegment.value, secondSegment.value)
          case _ =>
            throwSegmentsMustBelongToOriginalSeqs(left, right)
        }
    }
  }

  /**
   * Preconditions:
   *
   * 1. `segment` belongs to `firstSeq` or `secondSeq` sequence.
   *
   * @return `true` if `segment` value is invariant. It restores type of segment value (`U1` or `U2`) and calls either
   * [[firstInvariant]] or [[secondInvariant]] function.
   */
  protected final def isInvariantSegment(segment: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]): Boolean = {
    val firstSegment = castSegmentToFirstSeq(segment)
    if (firstSegment != null) firstInvariant(firstSegment.value)
    else {
      val secondSegment = castSegmentToSecondSeq(segment)
      if (secondSegment != null) secondInvariant(secondSegment.value)
      else throwSegmentsMustBelongToOriginalSeqs(segment)
    }
  }

  /**
   * Preconditions:
   *
   * 1. `segment` belongs to `firstSeq` or `secondSeq` sequence.
   *
   * Restores segment value type if segment belongs to [[firstSeq]], otherwise returns `null`.
   */
  protected final def castSegmentToFirstSeq(
    segment: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): SegmentT[E, D, U1, S1] | Null = {
    // We can use reference equality to check whether segment belongs to `firstSeq`. If `true` cast is safe.
    if (segment.sequence.eq(firstSeq)) segment.asInstanceOf[SegmentT[E, D, U1, S1]]
    else null
  }

  /**
   * Preconditions:
   *
   * 1. `segment` belongs to `firstSeq` or `secondSeq` sequence.
   *
   * Restores segment value type if segment belongs to [[secondSeq]], otherwise returns `null`.
   */
  protected final def castSegmentToSecondSeq(
    segment: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): SegmentT[E, D, U2, S2] | Null = {
    // We can use reference equality to check whether segment belongs to `secondSeq`. If `true` cast is safe.
    if (segment.sequence.eq(secondSeq)) segment.asInstanceOf[SegmentT[E, D, U2, S2]]
    else null
  }

  /**
   * Preconditions:
   *
   * 1. `left` segment belongs to `firstSeq` or `secondSeq` sequence.
   *    `right` segment belongs to `firstSeq` or `secondSeq` sequence other then sequence of `left`.
   *
   * 2. `left` and `right` subsegments must define front bound of zipped segment
   *     (operator must change its value for next pair of subsegments).
   *
   * @return zipped segment for `left` and `right` subsegments.
   */
  protected final def frontZipperGeneral(
    left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): ZippedSegment[E, D, U1, U2, V, S1, S2] =
    if (firstSegmentInstance.isSpecifiedBy(left, right)) firstSegmentInstance
    else frontZipperWithPrev(left, right)

  /**
   * Preconditions:
   *
   * 1. All preconditions of [[frontZipperGeneral]] method.
   *
   * 2. `left` and `right` subsegment must belong to first zipped segment.
   *    I.e. they must define its front bound.
   *
   * @return first zipped segment for `left` and `right` subsegments.
   */
  protected final def frontZipperFirst(
    left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): ZippedFirstSegment[E, D, U1, U2, V, S1, S2] =
    (left, right) match {
      case (ln: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2], rn: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2]) =>
        if (domainOps.boundOrd.compare(ln.upper, rn.upper) >= 0)
          ZippedInitialSegment[E, D, U1, U2, V, S1, S2](this, rn, ln)
        else
          ZippedInitialSegment[E, D, U1, U2, V, S1, S2](this, ln, rn)
      case (ln: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2], _) =>
        ZippedInitialSegment[E, D, U1, U2, V, S1, S2](this, ln, right)
      case (_, rn: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2]) =>
        ZippedInitialSegment[E, D, U1, U2, V, S1, S2](this, rn, left)
      case (ll: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2], rl: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2]) =>
        ZippedSingleSegment[E, D, U1, U2, V, S1, S2](this, ll, rl)
      case _ =>
        throwSegmentsMustBeLastOrWithNext(left, right) // just to remove warning
    }

  /**
   * Preconditions:
   *
   * 1. All preconditions of [[frontZipperGeneral]] method.
   *
   * 2. `left` and `right` must be last segments of original sequences.
   *
   * @return last zipped segment for `left` and `right` subsegments.
   */
  protected final def frontZipperLast(
    left: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): ZippedLastSegment[E, D, U1, U2, V, S1, S2] =
    if (firstSegmentInstance.isSpecifiedBy(left, right))
      // First zipped segment is single if it's specified by two last subsegments => cast is safe.
      firstSegmentInstance.asInstanceOf[ZippedSingleSegment[E, D, U1, U2, V, S1, S2]]
    else
      ZippedTerminalSegment[E, D, U1, U2, V, S1, S2](this, left, right)

  /**
   * Preconditions:
   *
   * 1. All preconditions of [[frontZipperGeneral]] method.
   *
   * 2. `left` and `right` must define front bound of zipped segment which is not first
   *    segment of zipped sequence.
   *
   * @return zipped segment which has previous zipped segment for `left` and `right` subsegments.
   */
  protected final def frontZipperWithPrev(
    left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): ZippedSegmentWithPrev[E, D, U1, U2, V, S1, S2] =
    (left, right) match {
      case (ln: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2], rn: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2]) =>
        if (domainOps.boundOrd.compare(ln.upper, rn.upper) >= 0)
          ZippedInnerSegment[E, D, U1, U2, V, S1, S2](this, rn, ln)
        else
          ZippedInnerSegment[E, D, U1, U2, V, S1, S2](this, ln, rn)
      case (ln: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2], _) =>
        ZippedInnerSegment[E, D, U1, U2, V, S1, S2](this, ln, right)
      case (_, rn: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2]) =>
        ZippedInnerSegment[E, D, U1, U2, V, S1, S2](this, rn, left)
      case (ll: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2], rl: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2]) =>
        ZippedTerminalSegment[E, D, U1, U2, V, S1, S2](this, ll, rl)
      case _ =>
        throwSegmentsMustBeLastOrWithNext(left, right) // just to remove warning
    }

  /**
   *  Preconditions:
   *
   * 1. All preconditions of [[frontZipperGeneral]] method.
   *
   * 2. `left` and `right` must define front bound of zipped segment which is not last
   *     segment of zipped sequence.
   *
   * @return zipped segment which has next zipped segment for `left` and `right` subsegments.
   */
  protected final def frontZipperWithNext(
    left: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): ZippedSegmentWithNext[E, D, U1, U2, V, S1, S2] =
    if (firstSegmentInstance.isSpecifiedBy(left, right))
      // First zipped segment is initial if one of its subsegments has next segment => cast is safe.
      firstSegmentInstance.asInstanceOf[ZippedInitialSegment[E, D, U1, U2, V, S1, S2]]
    else right match {
      case rn: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2] =>
        if (domainOps.boundOrd.compare(left.upper, rn.upper) >= 0)
          ZippedInnerSegment[E, D, U1, U2, V, S1, S2](this, rn, left)
        else
          ZippedInnerSegment[E, D, U1, U2, V, S1, S2](this, left, rn)
      case _ =>
        ZippedInnerSegment[E, D, U1, U2, V, S1, S2](this, left, right)
    }

  @inline
  protected final def supplyZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    supplied: Zipper[E, D, U1, U2, V, S1, S2, R],
    composed: ComposedZipper[E, D, U1, U2, V, S1, S2, R]
  ): Zipper[E, D, U1, U2, V, S1, S2, R] =
    (left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2], right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]) =>
      composed(supplied, left, right)

  /**
   * Preconditions:
   *
   * 1. `left` segment belongs to `firstSeq` or `secondSeq` sequence.
   *    `right` segment belongs to `firstSeq` or `secondSeq` sequence other then sequence of `left`.
   *
   * Starting from `left` and `right` subsegments function moves forward (getting next pairs of subsegments) until
   * meets change of the operator value. In that case it stops and builds zipped segment with `zipper` function
   * for last pair of subsegments before value change.
   * {{{
   *
   *                left         ->       output left
   *                 V                        V
   *       -------------|-------------|-------------|-------------
   *              A           B              C      |     D         - segment value
   *                                                |
   *                   right     ->    output right |
   *                    V                  V        |
   *       -------|------------|---------------------------|------
   *          A            B                  C     |          D    - segment value
   *                                                |
   *                               output zipped    |     next zipped
   *                                     V          |         V
   *       -----------------------------------------|-------------
   *                           B                    |      C        - segment value
   *                                           upper bound
   * }}}
   */
  protected final def searchFrontZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    zipper: Zipper[E, D, U1, U2, V, S1, S2, R],
    left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): R =
    if (left.isLast && right.isLast) zipper(left, right)
    else {
      var stop = false
      var nextZipped: ZippedTuple[E, D, U1, U2, V, S1, S2] | Null = null
      var currZipped: ZippedTuple[E, D, U1, U2, V, S1, S2] = DefaultZippedTuple(this, left, right)
      while (!stop) {
        nextZipped = stepForwardZipper(DefaultZippedTuple.zipper(this), currZipped.left, currZipped.right)
        if (valueOps.neqv(currZipped.value, nextZipped.value)) {
          // We have found a bound where operator change its value => return 'currZipped'.
          stop = true
        } else {
          // 'currZipped' and 'nextZipped' have same value => accept 'nextZipped' and try to move next.
          currZipped = nextZipped
          stop = currZipped.left.isLast && currZipped.right.isLast
        }
      }
      // Create proper zipped segment from temporary.
      zipper(currZipped.left, currZipped.right)
    }

  /**
   * Preconditions:
   *
   * 1. `left` segment belongs to `firstSeq` or `secondSeq` sequence.
   *    `right` segment belongs to `firstSeq` or `secondSeq` sequence other then sequence of `left`.
   *
   * Starting from `left` and `right` subsegments function moves backward (getting previous pairs of subsegments) until
   * meets change of the operator value. In that case it stops and builds zipped segment with `zipper` function
   * for last pair of subsegments before value change.
   * {{{
   *
   *             output left         <-      left
   *                 V                        V
   *       -------------|-------------|-------------|-------------
   *              A     |     B              C            D         - segment value
   *                    |
   *              output right     <-     right
   *                    V                  V
   *       -------|------------|---------------------------|------
   *          A         |  B                  C                D    - segment value
   *                    |
   *       prev zipped  |          output zipped
   *                    |                V
   *       -------------------------------------------------------
   *          A         |                    B                      - segment value
   *               lower bound
   * }}}
   */
  protected final def searchBackZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    zipper: Zipper[E, D, U1, U2, V, S1, S2, R],
    left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): R =
    if (left.isFirst && right.isFirst) zipper(left, right)
    else {
      var stop = false
      var prevZipped: ZippedTuple[E, D, U1, U2, V, S1, S2] | Null = null
      var currZipped: ZippedTuple[E, D, U1, U2, V, S1, S2] = DefaultZippedTuple(this, left, right)
      while (!stop) {
        prevZipped = stepBackwardZipper(DefaultZippedTuple.zipper(this), currZipped.left, currZipped.right)
        if (valueOps.neqv(currZipped.value, prevZipped.value)) {
          // We have found a bound where operator change its value => return 'currZipped'.
          stop = true
        } else {
          // 'currZipped' and 'prevZipped' have same value => accept 'prevZipped' and try to move back.
          currZipped = prevZipped
          stop = currZipped.left.isFirst && currZipped.right.isFirst
        }
      }
      // Create proper zipped segment from temporary tuple.
      zipper(currZipped.left, currZipped.right)
    }

  /**
   * Preconditions:
   *
   * 1. `left` segment belongs to `firstSeq` or `secondSeq` sequence.
   *    `right` segment belongs to `firstSeq` or `secondSeq` sequence other then sequence of `left`.
   *
   * Starting from `left` and `right` subsegments function get next pair of subsegments and builds zipped segment
   * with `zipper` function.
   *
   * If `left` and `right` are both last segments of original sequences then `zipper` function is applied to them.
   */
  protected final def stepForwardZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    zipper: Zipper[E, D, U1, U2, V, S1, S2, R],
    left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): R =
    left match {
      case ln: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2] => stepForwardNextGenZipper(zipper, ln, right)
      // left:  ?---------------X
      // right: ?----------?
      case ll: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2] => right match {
        // left:  ?---------------X
        // right: ?----------|
        case rn: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2] => stepForwardNextLastZipper(zipper, rn, ll)
        // left:  ?---------------X
        // right: ?---------------X
        // Unable to make step forward => return zipped segment for current left and right values.
        case _ => zipper(left, right)
      }
      case _ => throwSegmentsMustBeLastOrWithNext(left) // just to remove warning
    }

  /**
   * Preconditions:
   *
   * 1. `left` segment belongs to `firstSeq` or `secondSeq` sequence.
   *    `right` segment belongs to `firstSeq` or `secondSeq` sequence other then sequence of `left`.
   *
   * Starting from `left` and `right` subsegments function get previous pair of subsegments and builds zipped segment
   * with `zipper` function.
   *
   * If `left` and `right` are both first segments of original sequences then `zipper` function is applied to them.
   */
  protected final def stepBackwardZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    zipper: Zipper[E, D, U1, U2, V, S1, S2, R],
    left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): R =
    left match {
      case lp: SegmentT.WithPrev[E, D, ? <: U1 | U2, ? <: S1 | S2] => stepBackwardPrevGenZipper(zipper, lp, right)
      // left:  X---------------?
      // right:      ?----------?
      case lf: SegmentT.First[E, D, ? <: U1 | U2, ? <: S1 | S2] => right match {
        // left:  X---------------?
        // right:      |----------?
        case rp: SegmentT.WithPrev[E, D, ? <: U1 | U2, ? <: S1 | S2] => stepBackwardFirstPrevZipper(zipper, lf, rp)
        // left:  X---------------?
        // right: X---------------?
        // Unable to make step backward => return zipped segment for current left and right values.
        case _ => zipper(left, right)
      }
      case _ => throwSegmentsMustBeFirstOrWithPrev(left) // just to remove warning
    }

  /** Same as [[stepForwardZipper]] function but with stricter input types. */
  protected final def stepForwardNextGenZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    zipper: Zipper[E, D, U1, U2, V, S1, S2, R],
    left: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): R =
    right match {
      case rn: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2] => stepForwardNextNextZipper(zipper, left, rn)
      case rl: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2] => stepForwardNextLastZipper(zipper, left, rl)
      case _ => throwSegmentsMustBeLastOrWithNext(right) // just to remove warning
    }

  /** Same as [[stepBackwardZipper]] function but with stricter input types. */
  protected final def stepBackwardPrevGenZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    zipper: NextGenZipper[E, D, U1, U2, V, S1, S2, R],
    left: SegmentT.WithPrev[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): R =
    right match {
      case rp: SegmentT.WithPrev[E, D, ? <: U1 | U2, ? <: S1 | S2] => stepBackwardPrevPrevZipper(zipper, rp, left)
      case rf: SegmentT.First[E, D, ? <: U1 | U2, ? <: S1 | S2] => stepBackwardFirstPrevZipper(zipper, rf, left)
      case _ => throwSegmentMustBeFirstOrWithPrev(right) // just to remove warning
    }

  /** Same as [[stepForwardZipper]] function but with stricter input types. */
  protected final def stepForwardNextLastZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    zipper: Zipper[E, D, U1, U2, V, S1, S2, R],
    backward: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2],
    forward: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): R =
    // backward: ?----------|
    // forward:  ?---------------X
    // If value of forward segment is invariant (i.e. value of backward segment doesn't affect on operator),
    // we can immediately define operator value up to upper bound of forward segment skipping segments of
    // backward sequence below it.
    if (isInvariantSegment(forward))
      zipper(backward.moveToLast, forward)
    else
      zipper(backward.moveNext, forward)

  /** Same as [[stepBackwardZipper]] function but with stricter input types. */
  protected final def stepBackwardFirstPrevZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    zipper: NextGenZipper[E, D, U1, U2, V, S1, S2, R],
    backward: SegmentT.First[E, D, ? <: U1 | U2, ? <: S1 | S2],
    forward: SegmentT.WithPrev[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): R =
    // backward: X---------------?
    // forward:      |-----------?
    // If value of backward segment is invariant (i.e. value of forward segment doesn't affect on operator),
    // we can immediately define operator value up to lower bound of backward segment skipping segments of
    // forward sequence above it.
    // Input forward segment has previous segment => all previous segments of corresponding sequence has next segment =>
    // cast is safe.
    if (isInvariantSegment(backward))
      zipper(forward.moveToFirst.asInstanceOf[SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2]], backward)
    else
      zipper(forward.movePrev, backward)

  /** Same as [[stepForwardZipper]] function but with stricter input types. */
  protected final def stepForwardNextNextZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    zipper: Zipper[E, D, U1, U2, V, S1, S2, R],
    left: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): R = {
    val cmp = domainOps.boundOrd.compare(left.upper, right.upper)
    // left:  ?---------------|
    // right: ?---------------|
    if (cmp == 0) zipper(left.moveNext, right.moveNext)
    else
      // left:  ?---------------|
      // right: ?----------|
      if (cmp > 0) stepForwardNextNextDiffZipper(zipper, right, left)
      // left:  ?----------|
      // right: ?---------------|
      else stepForwardNextNextDiffZipper(zipper, left, right)
    }

  /** Same as [[stepBackwardZipper]] function but with stricter input types. */
  protected final def stepBackwardPrevPrevZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    zipper: NextGenZipper[E, D, U1, U2, V, S1, S2, R],
    left: SegmentT.WithPrev[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT.WithPrev[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): R = {
    val cmp = domainOps.boundOrd.compare(left.lower, right.lower)
    // left:  |---------------?
    // right: |---------------?
    if (cmp == 0) zipper(left.movePrev, right.movePrev)
    else
      // left:      |-----------?
      // right: |---------------?
      if (cmp > 0) stepBackwardPrevPrevDiffZipper(zipper, right, left)
      // left:  |---------------?
      // right:     |-----------?
      else stepBackwardPrevPrevDiffZipper(zipper, left, right)
  }

  /**
   * Same as [[stepForwardNextNextZipper]] function with additional precondition:
   *  1. Upper bounds of `left` and `right` subsegments are not equal.
   */
  protected final def stepForwardNextNextDiffZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    zipper: Zipper[E, D, U1, U2, V, S1, S2, R],
    backward: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2],
    forward: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): R =
    // backward: ?----------|
    // forward:  ?---------------|
    // If value of forward segment is invariant (i.e. value of backward segment doesn't affect on operator),
    // we can immediately define operator value up to upper bound of forward segment skipping segments of
    // backward sequence below it.
    if (isInvariantSegment(forward))
      zipper(backward.moveToBound(forward.upper), forward)
    else
      zipper(backward.moveNext, forward)

  /**
   * Same as [[stepBackwardPrevPrevZipper]] function with additional precondition:
   *  1. Lower bounds of `left` and `right` subsegments are not equal.
   */
  protected final def stepBackwardPrevPrevDiffZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    zipper: NextGenZipper[E, D, U1, U2, V, S1, S2, R],
    backward: SegmentT.WithPrev[E, D, ? <: U1 | U2, ? <: S1 | S2],
    forward: SegmentT.WithPrev[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): R =
    // backward: |---------------?
    // forward:      |-----------?
    // If value of backward segment is invariant (i.e. value of forward segment doesn't affect on operator),
    // we can immediately define operator value up to lower bound of backward segment skipping segments of
    // forward sequence above it.
    // Input forward segment has previous segment => all previous segments of corresponding sequence has next segment
    // => cast is safe.
    if (isInvariantSegment(backward))
      zipper(forward.moveToBound(backward.lower).asInstanceOf[SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2]], backward)
    else
      zipper(forward.movePrev, backward)

  protected final def throwSegmentsMustBelongToOriginalSeqs(
    segments: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]*
  ): Nothing = {
    val segmentsStr = SetBuilderFormat.segmentIterable(
      segments, SetBuilderFormat.toStringFunc[E], SetBuilderFormat.toStringFunc[Any]
    )
    throw new AssertionError(
      s"Expected that segments $segmentsStr belong to one of original sequences."
    )
  }
}

object AbstractZippedSegmentSeq {

  type ZippedSegment[E, D <: Domain[E], U1, U2, V, S1, S2] =
    SegmentT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2]] with ZippedSegmentBase[E, D, U1, U2, V, S1, S2]

  type ZippedFirstSegment[E, D <: Domain[E], U1, U2, V, S1, S2] =
    SegmentT.First[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2]] with ZippedSegmentBase[E, D, U1, U2, V, S1, S2]

  type ZippedLastSegment[E, D <: Domain[E], U1, U2, V, S1, S2] =
    SegmentT.Last[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2]] with ZippedSegmentBase[E, D, U1, U2, V, S1, S2]

  type ZippedTruncation[E, D <: Domain[E], U1, U2, V, S1, S2] =
    SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], ZippedSegment[E, D, U1, U2, V, S1, S2]]

  /**
   * Pair of segments of original sequences.
   *
   * Preconditions:
   *
   * 1. `left` segment belongs to `firstSeq` or `secondSeq` sequence.
   *    `right` segment belongs to `firstSeq` or `secondSeq` sequence other then sequence of `left`.
   */
  sealed trait ZippedTuple[E, D <: Domain[E], U1, U2, V, S1, S2] {

    /**
     * Value of zipped sequence.
     */
    lazy val value: V = sequence.getSegmentValue(left, right)

    /**
     * Zipped sequence.
     */
    def sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2]

    /**
     * Segment of original sequence.
     */
    def left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]

    /**
     * Segment of another original sequence.
     */
    def right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]

    /** @return `true` if subsegments of tuple correspond to input `left` and `right`.
     *           Which is `left` and which is `right` doesn't matter.
     */
    def isSpecifiedBy(
      left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
      right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
    ): Boolean =
      if (validateSequences(left, right)) {
        val ord = sequence.domainOps.segments.upperOrd
        if (ord.eqv(left, this.left)) ord.eqv(right, this.right)
        else if (ord.eqv(right, this.left)) ord.eqv(left, this.right)
        else false
      } else false

    /**
     * @return segment (either [[left]] or [[right]]) that belong to first original sequence.
     */
    def firstSeqSegment: SegmentT[E, D, U1, S1] = {
      var segment = sequence.castSegmentToFirstSeq(left)
      if (segment != null) segment
      else {
        segment = sequence.castSegmentToFirstSeq(right)
        if (segment != null) segment
        else sequence.throwSegmentsMustBelongToOriginalSeqs(left, right)
      }
    }

    /**
     * @return segment (either [[left]] or [[right]]) that belong to second original sequence.
     */
    def secondSeqSegment: SegmentT[E, D, U2, S2] = {
      var segment = sequence.castSegmentToSecondSeq(left)
      if (segment != null) segment
      else {
        segment = sequence.castSegmentToSecondSeq(right)
        if (segment != null) segment
        else sequence.throwSegmentsMustBelongToOriginalSeqs(left, right)
      }
    }

    // Protected section -------------------------------------------------------- //
    /**
     * @return `true` if `left` segment belongs to one of the original sequences of zipped sequence and
     *         `right` belongs to another one.
     *
     * @note sequences are identified by reference.
     */
    protected def validateSequences(
      left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
      right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
    ): Boolean =
      if (sequence.firstSeq.eq(left.sequence)) sequence.secondSeq.eq(right.sequence)
      else if (sequence.firstSeq.eq(right.sequence)) sequence.secondSeq.eq(left.sequence)
      else false
}

  /**
   * Default implementation of [[ZippedTuple]] (see preconditions).
   */
  final case class DefaultZippedTuple[E, D <: Domain[E], U1, U2, V, S1, S2](
    override val sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
    override val left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
    override val right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ) extends ZippedTuple[E, D, U1, U2, V, S1, S2]

  object DefaultZippedTuple {

    /**
     * Creates [[Zipper]] function.
     */
    def zipper[E, D <: Domain[E], U1, U2, V, S1, S2](
      sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2]
    ): Zipper[E, D, U1, U2, V, S1, S2, DefaultZippedTuple[E, D, U1, U2, V, S1, S2]] =
      (left, right) => new DefaultZippedTuple[E, D, U1, U2, V, S1, S2](sequence, left, right)
  }

  /**
   * Extension of [[ZippedTuple]] (see preconditions) which tracks ordering of original segments.
   * For `backward` and `forward` segments the relation must be hold according to some ordering:
   *
   * `backward` `≤` `forward`
   */
  sealed trait OrderedZippedTuple[E, D <: Domain[E], U1, U2, V, S1, S2]
    extends ZippedTuple[E, D, U1, U2, V, S1, S2] {

    def backward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]

    def forward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]

    override def left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = backward

    override def right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = forward
  }

  object OrderedZippedTuple {

    /**
     * @return `true` if `backward` and `forward` segments are ordered according to specified `order`.
     */
    def isValidOrder[E, D <: Domain[E], U1, U2, V, S1, S2](
      order: Order[Segment[E, D, ?]],
      backward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
      forward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
    ): Boolean =
      order.lteqv(backward, forward)

    /**
     * [[OrderedZippedTuple]] with segments ordered according to [[DomainOps.segments.upperOrd]]:
     */
    trait ByUpperBound[E, D <: Domain[E], U1, U2, V, S1, S2] extends OrderedZippedTuple[E, D, U1, U2, V, S1, S2]

    object ByUpperBound {

      /**
       * Creates ordered tuple from two original segments.
       *
       * @see preconditions of [[ZippedTuple]].
       */
      def apply[E, D <: Domain[E], U1, U2, V, S1, S2](
        sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
        left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
        right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
      ): ByUpperBound[E, D, U1, U2, V, S1, S2] =
        if (isValidOrder(sequence.domainOps.segments.upperOrd, left, right))
          DefaultImpl(sequence, left, right)
        else
          DefaultImpl(sequence, right, left)

      /**
       * Creates ordered tuple from two original segments without validation of segments order.
       *
       * Preconditions:
       *
       * 1. `backward` `≤` `forward` according to [[DomainOps.segments.upperOrd]] of `sequence`.
       *
       * @see preconditions of [[ZippedTuple]].
       */
      def unsafe[E, D <: Domain[E], U1, U2, V, S1, S2](
        sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
        backward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
        forward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
      ): ByUpperBound[E, D, U1, U2, V, S1, S2] =
        DefaultImpl(sequence, backward, forward)

      /**
       * Creates [[Zipper]] function.
       */
      def zipper[E, D <: Domain[E], U1, U2, V, S1, S2](
        sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2]
      ): Zipper[E, D, U1, U2, V, S1, S2, ByUpperBound[E, D, U1, U2, V, S1, S2]] =
        (left, right) => apply(sequence, left, right)

      // Private section ---------------------------------------------------------- //
      /**
       * Implementation of [[ByUpperBound]].
       */
      private final case class DefaultImpl[E, D <: Domain[E], U1, U2, V, S1, S2] (
        override val sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
        override val backward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
        override val forward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
      ) extends OrderedZippedTuple.ByUpperBound[E, D, U1, U2, V, S1, S2]
    }

    /**
     * [[OrderedZippedTuple]] with segments ordered according to [[DomainOps.segments.lowerOrd]]:
     */
    trait ByLowerBound[E, D <: Domain[E], U1, U2, V, S1, S2] extends OrderedZippedTuple[E, D, U1, U2, V, S1, S2]

    object ByLowerBound {

      /**
       * Creates ordered tuple from two original segments.
       *
       * @see preconditions of [[ZippedTuple]].
       */
      def apply[E, D <: Domain[E], U1, U2, V, S1, S2](
        sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
        left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
        right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
      ): ByLowerBound[E, D, U1, U2, V, S1, S2] =
        if (isValidOrder(sequence.domainOps.segments.lowerOrd, left, right))
          DefaultImpl(sequence, left, right)
        else
          DefaultImpl(sequence, right, left)

      /**
       * Creates ordered tuple from two original segments without validation of segments order.
       *
       * Preconditions:
       *
       * 1. `backward` `≤` `forward` according to [[DomainOps.segments.lowerOrd]] of `sequence`.
       *
       * @see preconditions of [[ZippedTuple]].
       */
      def unsafe[E, D <: Domain[E], U1, U2, V, S1, S2](
        sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
        backward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
        forward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
      ): ByLowerBound[E, D, U1, U2, V, S1, S2] =
        DefaultImpl(sequence, backward, forward)

      /**
       * Creates [[Zipper]] function.
       */
      def zipper[E, D <: Domain[E], U1, U2, V, S1, S2](
        sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2]
      ): Zipper[E, D, U1, U2, V, S1, S2, ByLowerBound[E, D, U1, U2, V, S1, S2]] =
        (left, right) => apply(sequence, left, right)

      // Private section ---------------------------------------------------------- //
      /**
       * Implementation of [[ByLowerBound]].
       */
      private final case class DefaultImpl[E, D <: Domain[E], U1, U2, V, S1, S2] (
        override val sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
        override val backward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
        override val forward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
      ) extends OrderedZippedTuple.ByLowerBound[E, D, U1, U2, V, S1, S2]
    }
  }

  /**
   * Base trait for zipped segments.
   *
   * Upper and lower bounds of zipped segments are defined by change of the operator value.
   * Zipped segment is specified by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]])
   * that corresponds to the upper bound of zipped segment.
   * {{{
   *
   *                                      left
   *                                        V
   *       -------------|-------------|-------------|-------------
   *              A     |     B              C      |     D         - segment value
   *                    |                           |
   *                    |                right      |
   *                    |                  V        |
   *       -------|------------|---------------------------|------
   *          A         |  B                  C     |          D    - segment value
   *                    |                           |
   *       prev zipped  |          zipped (this)    |     next zipped
   *           V        |              V            |         V
   *       -------------|---------------------------|-------------
   *             A      |              B            |      C        - segment value
   *                lower bound                upper bound
   * }}}
   */
  sealed trait ZippedSegmentBase[E, D <: Domain[E], U1, U2, V, S1, S2]
    extends SegmentLikeT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2]]
      with OrderedZippedTuple.ByUpperBound[E, D, U1, U2, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2]

    override def isIncluded: Boolean = sequence.isValueIncluded(value)
    
    // Navigation --------------------------------------------------------------- //
    /**
     * Returns front tuple of original segments.
     * {{{
     *
     *   front == (S2, S4)
     *
     *      S1               S2
     *   --------------](----------)[-----  first original seq
     *            |      S3        |   S4
     *   --------](-----------)[----------  second original seq
     *            |                |
     *   --------](----------------)[-----  zipped seq
     *              current segment
     * }}}
     */
    def front: OrderedZippedTuple.ByUpperBound[E, D, U1, U2, V, S1, S2] = this

    /**
     * Returns back tuple of original segments.
     * {{{
     *
     *   back == (S1, S3)
     *
     *      S1               S2
     *   --------------](----------)[-----  first original seq
     *            |      S3        |   S4
     *   --------](-----------)[----------  second original seq
     *            |                |
     *   --------](----------------)[-----  zipped seq
     *              current segment
     * }}}
     */
    def back: OrderedZippedTuple.ByLowerBound[E, D, U1, U2, V, S1, S2]

    override def moveToFirst: ZippedFirstSegment[E, D, U1, U2, V, S1, S2] = sequence.firstSegment

    override def moveToLast: ZippedLastSegment[E, D, U1, U2, V, S1, S2] = sequence.lastSegment

    override def moveToBound(bound: Bound[E]): ZippedSegment[E, D, U1, U2, V, S1, S2] =
      sequence.searchFrontZipper(sequence.frontZipperGeneral, left.moveToBound(bound), right.moveToBound(bound))

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: SegmentSeq[E, D, V]

    override def takeBelow: SegmentSeq[E, D, V]

    override def slice: (SegmentSeq[E, D, V], SegmentSeq[E, D, V])

    override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
      // Default implementation for first segment. Must be overridden if segment has previous segment.
      sequence
    }

    override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = {
      // Default implementation for first segment. Must be overridden if segment has previous segment.
      sequence
    }

    /**
     * Returns truncation of first <u>original</u> sequence at lower bound of <u>zipped</u> segment.
     * {{{
     *
     *   firstSeqLowerTruncation - truncation with segment S1 and bound B1;
     *   firstSeqUpperTruncation - truncation with segment S2 and bound B2.
     * 
     *            B1               B2
     *      S1    (          S2    )
     *   --------------](----------)[-----  first original seq
     *   
     *   --------](-----------)[----------  second original seq
     *   
     *   --------](----------------)[-----  zipped seq
     *              current segment
     * }}}
     *
     * Note, in general case:
     * {{{
     *   segment.firstSeqLowerTruncation != segment.firstSeqSegment.lowerTruncation
     * }}}
     * because of `segment.firstSeqSegment` is a segment of first original sequence at upper bound of zipped segment
     * (S2 in example above).
     */
    def firstSeqLowerTruncation: SegmentTruncationT[E, D, U1, S1, SegmentT[E, D, U1, S1] with S1] = {
      // Implementation is only for first zipped segment. Must be overridden if segment has previous segment.
      sequence.firstSeq.firstSegment.lowerTruncation
    }

    /**
     * Returns truncation of first <u>original</u> sequence at upper bound of <u>zipped</u> segment.
     * {{{
     *
     *   firstSeqLowerTruncation - truncation with segment S1 and bound B1;
     *   firstSeqUpperTruncation - truncation with segment S2 and bound B2.
     *
     *            B1               B2
     *      S1    (          S2    )
     *   --------------](----------)[-----  first original seq
     *   
     *   --------](-----------)[----------  second original seq
     *   
     *   --------](----------------)[-----  zipped seq
     *              current segment
     * }}}
     *
     * Note, in general case:
     * {{{
     *   segment.firstSeqUpperTruncation != segment.firstSeqSegment.upperTruncation
     * }}}
     * because of upper bound of `segment.firstSeqSegment` may differ from upper bound of zipped segment.
     */
    def firstSeqUpperTruncation: SegmentTruncationT[E, D, U1, S1, SegmentT[E, D, U1, S1] with S1] = {
      // Implementation is only for last zipped segment. Must be overridden if segment has next segment.
      sequence.firstSeq.lastSegment.upperTruncation
    }

    /**
     * Returns truncation of second <u>original</u> sequence at lower bound of <u>zipped</u> segment.
     * {{{
     *
     *   secondSeqLowerTruncation - truncation with segment S1 and bound B1;
     *   secondSeqUpperTruncation - truncation with segment S2 and bound B2.
     *
     *            B1               B2
     *            (   S1           )   S2
     *   --------](-----------)[----------  second original seq
     *   
     *   --------------](----------)[-----  first original seq
     *   
     *   --------](----------------)[-----  zipped seq
     *              current segment
     * }}}
     *
     * Note, in general case:
     * {{{
     *   segment.secondSeqLowerTruncation != segment.secondSeqSegment.lowerTruncation
     * }}}
     * because of `segment.secondSeqSegment` is a segment of second original sequence at upper bound of zipped segment
     * (S2 in example above).
     */
    def secondSeqLowerTruncation: SegmentTruncationT[E, D, U2, S2, SegmentT[E, D, U2, S2] with S2] = {
      // Implementation is only for first zipped segment. Must be overridden if segment has previous segment.
      sequence.secondSeq.firstSegment.lowerTruncation
    }

    /**
     * Returns truncation of second <u>original</u> sequence at upper bound of <u>zipped</u> segment.
     * {{{
     *
     *   secondSeqLowerTruncation - truncation with segment S1 and bound B1;
     *   secondSeqUpperTruncation - truncation with segment S2 and bound B2.
     * 
     *            B1               B2
     *            (   S1           )   S2
     *   --------](-----------)[----------  second original seq
     *   
     *   --------------](----------)[-----  first original seq
     *   
     *   --------](----------------)[-----  zipped seq
     *              current segment
     * }}}
     *
     * Note, in general case:
     * {{{
     *   segment.secondSeqUpperTruncation != segment.secondSeqSegment.upperTruncation
     * }}}
     * because of upper bound of `segment.secondSeqSegment` may differ from upper bound of zipped segment.
     */
    def secondSeqUpperTruncation: SegmentTruncationT[E, D, U2, S2, SegmentT[E, D, U2, S2] with S2] = {
      // Implementation is only for last zipped segment. Must be overridden if segment has next segment.
      sequence.secondSeq.lastSegment.upperTruncation
    }

    /**
     * Applies patch operation to first original sequence within current zipped segment.
     *
     * Returns sequence containing
     * <tr>
     *   - segments {(l,,i,,, min(u,,i,,, U(lower))) -> v,,i,,}
     *   of first original sequence for which l,,i,, `<` lower
     * </tr>
     * <tr>
     *   - segments {(max(lower, l,,i,,), min(upper, u,,i,,)) -> v,,i,,}
     *   of `other` sequence for which l,,i,, `≤` upper and u,,i,, `≥` lower
     * </tr>
     * <tr>
     *   - segments {(max(l,,i,,, L(upper)), u,,i,,) -> v,,i,,}
     *   of first original sequence for which u,,i,, `>` upper
     * </tr>
     * <tr>where</tr>
     * <tr>lower - lower bound of current zipped segment;</tr>
     * <tr>upper - upper bound of current zipped segment;</tr>
     * <tr>l,,i,, - lower bound of segment S,,i,,;</tr>
     * <tr>u,,i,, - upper bound of segment S,,i,,;</tr>
     * <tr>v,,i,, - value of segment S,,i,,.</tr>
     * <tr>
     *   U - upper bound operator, it acts as identity if bound is upper and flips bound otherwise
     *   (see [[Bound.provideUpper]]);
     * </tr>
     * <tr>
     *   L - lower bound operator, it acts as identity if bound is lower and flips bound otherwise
     *   (see [[Bound.provideLower]]).
     * </tr>
     *
     * {{{
     * first original sequence:
     *
     *   X--------](---------------)[------------X
     *        A              B             C        - values
     *
     * second original sequence:
     *
     *   X---------------)[--------------](-------X
     *            A               B           C     - values
     *
     * zipped sequence:
     *                    segment
     *   X-------](----------------------](-------X
     *       A    ^           B          ^    C     - values
     *          lower                  upper
     *
     * operator:
     *   A + A = A   A + B = B   B + B = B
     *   C + C = C   C + B = B
     *
     * other sequence:
     *
     *   X--)[--------------------------------](--X
     *     D                  E                 F   - values
     *
     * segment.patchFirstSeq(other):
     *
     *   X-------](----------------------](-------X
     *       A                E               C     - values
     * }}}
     */
    def patchFirstSeq(other: SegmentSeq[E, D, U1]): SegmentSeq[E, D, U1]

    /**
     * Applies patch operation to second original sequence within current zipped segment.
     *
     * See [[patchFirstSeq]].
     */
    def patchSecondSeq(other: SegmentSeq[E, D, U2]): SegmentSeq[E, D, U2]
  }

  object ZippedSegmentBase {

    trait TruncationBase[E, D <: Domain[E], U1, U2, V, S1, S2] {
      self: SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], ZippedSegment[E, D, U1, U2, V, S1, S2]] =>

      override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
        segment.sequence.prependBelowExtendedInternal(bound, getSegmentForPrepending, other)

      override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
        segment.sequence.appendAboveExtendedInternal(bound, getSegmentForAppending, other)
    }
  }

  /**
   * Zipped segment with next segment.
   *
   * Segment is specified by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]])
   * that corresponds to the upper bound. 'frontBackward' and 'frontForward' are the same two subsegments
   * which are ordered by their upper bound.
   */
  sealed trait ZippedSegmentWithNext[E, D <: Domain[E], U1, U2, V, S1, S2]
    extends SegmentT.WithNext[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2]]
      with ZippedSegmentBase[E, D, U1, U2, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def upper: Bound.Upper[E] = frontBackward.upper

    override def self: ZippedSegmentWithNext[E, D, U1, U2, V, S1, S2]

    // Navigation --------------------------------------------------------------- //
    def frontBackward: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2]

    def frontForward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]

    override def backward: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2] = frontBackward

    override def forward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = frontForward

    override def moveNext: ZippedSegmentWithPrev[E, D, U1, U2, V, S1, S2] =
      sequence.stepForwardNextGenZipper(
        sequence.supplyZipper[ZippedSegmentWithPrev[E, D, U1, U2, V, S1, S2]](
          sequence.frontZipperWithPrev,
          sequence.searchFrontZipper
        ),
        frontBackward,
        frontForward
      )

    // Transformation ----------------------------------------------------------- //
    override def append(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = 
      sequence.appendAboveBoundInternal(upper, this, other)
    
    override def firstSeqUpperTruncation: SegmentTruncationT[E, D, U1, S1, SegmentT[E, D, U1, S1] with S1] =
      firstSeqSegment.self.truncation(upper)

    override def secondSeqUpperTruncation: SegmentTruncationT[E, D, U2, S2, SegmentT[E, D, U2, S2] with S2] =
      secondSeqSegment.self.truncation(upper)
  }

  /**
   * Zipped segment with previous segment.
   *
   * Segment is specified by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]])
   * that corresponds to the upper bound.
   *
   * Lower bound is defined by 'backBackward' and 'backForward' subsegments and is searched lazily.
   * 'backBackward' and 'backForward' subsegments are ordered by their lower bound.
   * {{{
   *
   *                      backForward        left
   *                           V              V
   *       -------------|-------------|-------------|-------------
   *              A     |     B              C      |     D         - segment value
   *                    |                           |
   *                backBackward                right
   *                     V                       V
   *       -------|------------|---------------------------|------
   *          A          B                    C                D    - segment value
   *                    |                           |
   *                    |          zipped (this)    |
   *                    |              V            |
   *       -------------|---------------------------|-------------
   *             A                     B                  C         - segment value
   * }}}
   *
   * Preconditions:
   *
   * 1. 'backForward' subsegment must have previous segment.
   *    This condition is equivalent to: zipped segment has previous segment.
   */
  sealed trait ZippedSegmentWithPrev[E, D <: Domain[E], U1, U2, V, S1, S2]
    extends SegmentT.WithPrev[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2]]
      with ZippedSegmentBase[E, D, U1, U2, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def lower: Bound.Lower[E] = backForward.lower

    override def self: ZippedSegmentWithPrev[E, D, U1, U2, V, S1, S2]

    // Navigation --------------------------------------------------------------- //
    override lazy val back: OrderedZippedTuple.ByLowerBound[E, D, U1, U2, V, S1, S2] =
      sequence.searchBackZipper(OrderedZippedTuple.ByLowerBound.zipper(sequence), left, right)

    def backBackward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = back.backward

    def backForward: SegmentT.WithPrev[E, D, ? <: U1 | U2, ? <: S1 | S2] =
      // Cast is safe if precondition 1 is provided.
      back.forward.asInstanceOf[SegmentT.WithPrev[E, D, ? <: U1 | U2, ? <: S1 | S2]]

    override def movePrev: ZippedSegmentWithNext[E, D, U1, U2, V, S1, S2] =
      sequence.stepBackwardPrevGenZipper(sequence.frontZipperWithNext, backForward, backBackward)
    
    // Transformation ----------------------------------------------------------- //
    override def prepend(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] =
      sequence.prependBelowBoundInternal(lower, this, other)

    override def firstSeqLowerTruncation: SegmentTruncationT[E, D, U1, S1, SegmentT[E, D, U1, S1] with S1] =
      back.firstSeqSegment.self.truncation(lower)

    override def secondSeqLowerTruncation: SegmentTruncationT[E, D, U2, S2, SegmentT[E, D, U2, S2] with S2] =
      back.secondSeqSegment.self.truncation(lower)
  }

  /**
   * Initial segment of zipped sequence.
   *
   * Segment is specified by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]])
   * that corresponds to the upper bound. 'frontBackward' and 'frontForward' are the same two subsegments
   * which are ordered by their upper bound.
   */
  final case class ZippedInitialSegment[E, D <: Domain[E], U1, U2, V, S1, S2] (
    override val sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
    override val frontBackward: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2],
    override val frontForward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ) extends SegmentT.Initial[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2]]
    with ZippedSegmentWithNext[E, D, U1, U2, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def self: ZippedInitialSegment[E, D, U1, U2, V, S1, S2] = this

    // Navigation --------------------------------------------------------------- //
    override def left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = frontBackward

    override def right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = frontForward

    override lazy val back: OrderedZippedTuple.ByLowerBound[E, D, U1, U2, V, S1, S2] =
      OrderedZippedTuple.ByLowerBound.unsafe(sequence, left.moveToFirst, right.moveToFirst)

    override def moveToFirst: ZippedInitialSegment[E, D, U1, U2, V, S1, S2] = this

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2] = sequence

    override def takeBelow: UniformSegmentSeq[E, D, V] = sequence.consUniform(value)

    override def slice: (UniformSegmentSeq[E, D, V], ZippedSegmentSeq[E, D, U1, U2, V, S1, S2]) = (takeBelow, takeAbove)

    override def patch(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = moveNext.prepend(other)

    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], this.type] =
      new ZippedInitialSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], this.type] =
      SegmentTruncationT.upperTruncation(this)

    override def patchFirstSeq(other: SegmentSeq[E, D, U1]): SegmentSeq[E, D, U1] =
      firstSeqUpperTruncation.prepend(other)

    override def patchSecondSeq(other: SegmentSeq[E, D, U2]): SegmentSeq[E, D, U2] =
      secondSeqUpperTruncation.prepend(other)
  }

  object ZippedInitialSegment {

    final class Truncation[E, D <: Domain[E], U1, U2, V, S1, S2, +Seg <: ZippedInitialSegment[E, D, U1, U2, V, S1, S2]](
      override val segment: Seg,
      inputBound: ExtendedBound[E]
    ) extends SegmentT.Initial.Truncation[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], Seg](
      segment,
      inputBound
    ) with ZippedSegmentBase.TruncationBase[E, D, U1, U2, V, S1, S2]
  }

  /**
   * Terminal segment of zipped sequence.
   *
   * Segment is specified by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]])
   * that must be the last segments of original sequences.
   */
  final case class ZippedTerminalSegment[E, D <: Domain[E], U1, U2, V, S1, S2] (
    override val sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
    override val left: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2],
    override val right: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ) extends SegmentT.Terminal[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2]]
    with ZippedSegmentWithPrev[E, D, U1, U2, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def isSpecifiedBy(
      left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
      right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
    ): Boolean =
      validateSequences(left, right) && left.isLast && right.isLast

    override def self: ZippedTerminalSegment[E, D, U1, U2, V, S1, S2] = this

    // Navigation --------------------------------------------------------------- //
    override def backward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = left

    override def forward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = right
    
    override def moveToLast: ZippedTerminalSegment[E, D, U1, U2, V, S1, S2] = this

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: UniformSegmentSeq[E, D, V] = sequence.consUniform(value)

    override def takeBelow: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2] = sequence

    override def slice: (ZippedSegmentSeq[E, D, U1, U2, V, S1, S2], UniformSegmentSeq[E, D, V]) = (takeBelow, takeAbove)

    override def patch(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = movePrev.append(other)

    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], this.type] =
      new ZippedTerminalSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], this.type] =
      SegmentTruncationT.upperTruncation(this)

    override def patchFirstSeq(other: SegmentSeq[E, D, U1]): SegmentSeq[E, D, U1] =
      firstSeqLowerTruncation.append(other)

    override def patchSecondSeq(other: SegmentSeq[E, D, U2]): SegmentSeq[E, D, U2] =
      secondSeqLowerTruncation.append(other)
  }

  object ZippedTerminalSegment {

    final class Truncation[E, D <: Domain[E], U1, U2, V, S1, S2, +Seg <: ZippedTerminalSegment[E, D, U1, U2, V, S1, S2]](
      override val segment: Seg,
      inputBound: ExtendedBound[E]
    ) extends SegmentT.Terminal.Truncation[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], Seg](
      segment,
      inputBound
    ) with ZippedSegmentBase.TruncationBase[E, D, U1, U2, V, S1, S2]
  }

  /**
   * Inner segment of zipped sequence.
   *
   * Segment is specified by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]])
   * that corresponds to the upper bound. 'frontBackward' and 'frontForward' are the same two subsegments
   * which are ordered by their upper bound.
   */
  final case class ZippedInnerSegment[E, D <: Domain[E], U1, U2, V, S1, S2](
    override val sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
    override val frontBackward: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2],
    override val frontForward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ) extends SegmentT.Inner[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2]]
    with ZippedSegmentWithPrev[E, D, U1, U2, V, S1, S2]
    with ZippedSegmentWithNext[E, D, U1, U2, V, S1, S2] {

    import ordset.core.internal.TransformationUtil

    // Inspection --------------------------------------------------------------- //
    override def self: ZippedInnerSegment[E, D, U1, U2, V, S1, S2] = this

    // Navigation --------------------------------------------------------------- //
    override def left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = frontBackward

    override def right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = frontForward

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: SegmentSeq[E, D, V] =
      sequence.cons(front.firstSeqSegment.takeAbove, front.secondSeqSegment.takeAbove)

    override def takeBelow: SegmentSeq[E, D, V] = 
      sequence.cons(back.firstSeqSegment.takeBelow, back.secondSeqSegment.takeBelow)

    override def slice: (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) = {
      val firstSlice = TransformationUtil.sliceComposedSegment(back.firstSeqSegment, front.firstSeqSegment)
      val secondSlice = TransformationUtil.sliceComposedSegment(back.secondSeqSegment, front.secondSeqSegment)
      (sequence.cons(firstSlice._1, secondSlice._1), sequence.cons(firstSlice._2, secondSlice._2))
    }

    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], this.type] =
      new ZippedInnerSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], this.type] =
      SegmentTruncationT.upperTruncation(this)

    override def patchFirstSeq(other: SegmentSeq[E, D, U1]): SegmentSeq[E, D, U1] =
      firstSeqUpperTruncation.prepend(firstSeqLowerTruncation.append(other))

    override def patchSecondSeq(other: SegmentSeq[E, D, U2]): SegmentSeq[E, D, U2] =
      secondSeqUpperTruncation.prepend(secondSeqLowerTruncation.append(other))
  }

  object ZippedInnerSegment {

    final class Truncation[E, D <: Domain[E], U1, U2, V, S1, S2, +Seg <: ZippedInnerSegment[E, D, U1, U2, V, S1, S2]](
      override val segment: Seg,
      inputBound: ExtendedBound[E]
    ) extends SegmentT.Inner.Truncation[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], Seg](
      segment,
      inputBound
    ) with ZippedSegmentBase.TruncationBase[E, D, U1, U2, V, S1, S2]
  }

  /**
   * Single segment of zipped sequence.
   *
   * Segment is specified by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]])
   * that must be the last segments of original sequences.
   */
  final case class ZippedSingleSegment[E, D <: Domain[E], U1, U2, V, S1, S2](
    override val sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
    override val left: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2],
    override val right: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ) extends SegmentT.Single[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2]]
    with ZippedSegmentBase[E, D, U1, U2, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def backward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = left

    override def forward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = right

    override lazy val back: OrderedZippedTuple.ByLowerBound[E, D, U1, U2, V, S1, S2] =
      OrderedZippedTuple.ByLowerBound.unsafe(sequence, left.moveToFirst, right.moveToFirst)

    override def isSpecifiedBy(
      left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
      right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
    ): Boolean =
      (validateSequences(left, right)) && left.isLast && right.isLast

    override def self: ZippedSingleSegment[E, D, U1, U2, V, S1, S2] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: ZippedSingleSegment[E, D, U1, U2, V, S1, S2] = this

    override def moveToLast: ZippedSingleSegment[E, D, U1, U2, V, S1, S2] = this

    override def moveToBound(bound: Bound[E]): ZippedSingleSegment[E, D, U1, U2, V, S1, S2] = this

    override def moveToExtended(bound: ExtendedBound[E]): ZippedSingleSegment[E, D, U1, U2, V, S1, S2] = this

    override def moveToElement(element: E): ZippedSingleSegment[E, D, U1, U2, V, S1, S2] = this

    // Transformation ----------------------------------------------------------- //
    override def takeAbove: UniformSegmentSeq[E, D, V] = sequence.consUniform(value)

    override def takeBelow: UniformSegmentSeq[E, D, V] = sequence.consUniform(value)

    override def slice: (UniformSegmentSeq[E, D, V], UniformSegmentSeq[E, D, V]) = (takeBelow, takeAbove)

    override def truncation(
      bound: ExtendedBound[E]
    ): SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], this.type] =
      new ZippedSingleSegment.Truncation(this, bound)

    override def lowerTruncation: SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], this.type] =
      SegmentTruncationT.lowerTruncation(this)

    override def upperTruncation: SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], this.type] =
      SegmentTruncationT.upperTruncation(this)

    override def patchFirstSeq(other: SegmentSeq[E, D, U1]): SegmentSeq[E, D, U1] = other

    override def patchSecondSeq(other: SegmentSeq[E, D, U2]): SegmentSeq[E, D, U2] = other
  }

  object ZippedSingleSegment {

    final class Truncation[E, D <: Domain[E], U1, U2, V, S1, S2, +Seg <: ZippedSingleSegment[E, D, U1, U2, V, S1, S2]](
      override val segment: Seg,
      inputBound: ExtendedBound[E]
    ) extends SegmentT.Single.Truncation[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], Seg](
      segment,
      inputBound
    ) with ZippedSegmentBase.TruncationBase[E, D, U1, U2, V, S1, S2]
  }

  // Protected section -------------------------------------------------------- //
  protected type Zipper[E, D <: Domain[E], U1, U2, V, S1, S2, +R <: ZippedTuple[E, D, U1, U2, V, S1, S2]] =
    (SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2], SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]) => R

  protected type NextGenZipper[E, D <: Domain[E], U1, U2, V, S1, S2, +R <: ZippedTuple[E, D, U1, U2, V, S1, S2]] =
    (SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2], SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]) => R

  protected type ComposedZipper[E, D <: Domain[E], U1, U2, V, S1, S2, R <: ZippedTuple[E, D, U1, U2, V, S1, S2]] =
    (Zipper[E, D, U1, U2, V, S1, S2, R], SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2], SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]) => R
}