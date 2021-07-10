package ordset.core

import ordset.core.value.ValueOps
import ordset.core.domain.{AscOrder, Domain, DomainOps}
import AbstractZippedSegmentSeq._

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

  final override def contains(bound: Bound[E]): Boolean =
    isValueIncluded(getSegmentValue(firstSeq.getSegment(bound), secondSeq.getSegment(bound)))

  final override def containsElement(element: E): Boolean = super.containsElement(element)

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = super.upperBounds

  final override def firstSegment: ZippedFirstSegment[E, D, U1, U2, V, S1, S2] =
    firstSegmentInstance

  final override def lastSegment: ZippedLastSegment[E, D, U1, U2, V, S1, S2] =
    lastFrontZipper(firstSeq.lastSegment, secondSeq.lastSegment)

  final override def getSegment(bound: Bound[E]): ZippedSegment[E, D, U1, U2, V, S1, S2] =
    searchFrontZipper(generalFrontZipper, firstSeq.getSegment(bound), secondSeq.getSegment(bound))

  final override def getSegmentForElement(element: E): ZippedSegment[E, D, U1, U2, V, S1, S2] =
    getSegment(Bound.Upper.inclusive(element))

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): SegmentSeq[E, D, V] =
    cons(firstSeq.takenAbove(bound), secondSeq.takenAbove(bound))

  final override def takenBelow(bound: Bound[E]): SegmentSeq[E, D, V] =
    cons(firstSeq.takenBelow(bound), secondSeq.takenBelow(bound))

  final override def sliced(bound: Bound[E]): (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) =
    (takenBelow(bound), takenAbove(bound))

  final override def appended(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  // Protected section -------------------------------------------------------- //
  /**
   * Returns `true` if segment with given value is considered to be included in set.
   *
   * For example, if `V` = `Option[AnyType]`, then we assume `None` is not included and `Some(anyValue)` - is included.
   */
  protected def isValueIncluded(value: V): Boolean

  /**
   * Creates zipped segment sequence.
   */
  protected def cons(first: SegmentSeq[E, D, U1], second: SegmentSeq[E, D, U2]): SegmentSeq[E, D, V]

  /**
   * First segment of sequence. It's either initial ot single.
   */
  protected final lazy val firstSegmentInstance: ZippedFirstSegment[E, D, U1, U2, V, S1, S2] =
    searchFrontZipper(firstFrontZipper, firstSeq.firstSegment, secondSeq.firstSegment)

  /**
   * Preconditions:
   *
   * 1. `left` segment belongs to `firstSeq` or `secondSeq` sequence.
   *    `right` segment belongs to `firstSeq` or `secondSeq` sequence other then sequence of `left`.
   *
   * @return value of zipped segment defined by `left` and `right` subsegments.
   * For sets it returns 'belongs to set' indicator (`Boolean`).
   * For maps `E` -> `W` it returns `Option[W]` where `None` means that segment doesn't belong to set.
   */
  protected final def getSegmentValue(
    left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): V = {
    (castSegmentToFirstSeq(left), castSegmentToSecondSeq(right)) match {
      case (firstSegment: SegmentT[E, D, U1, S1], secondSegment: SegmentT[E, D, U2, S2]) =>
        operator(firstSegment.value, secondSegment.value)
      case (null, null) =>
        (castSegmentToFirstSeq(right), castSegmentToSecondSeq(left)) match {
          case (firstSegment: SegmentT[E, D, U1, S1], secondSegment: SegmentT[E, D, U2, S2]) =>
            operator(firstSegment.value, secondSegment.value)
          case (null, null) =>
            throwSegmentMustBelongToOriginalSeqs(left, right)
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
      else throwSegmentMustBelongToOriginalSeqs(segment)
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
  protected final def generalFrontZipper(
    left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): ZippedSegment[E, D, U1, U2, V, S1, S2] =
    if (firstSegmentInstance.isSpecifiedBy(left, right)) firstSegmentInstance
    else withPrevFrontZipper(left, right)

  /**
   * Preconditions:
   *
   * 1. All preconditions of [[generalFrontZipper]] method.
   *
   * 2. `left` and `right` subsegment must belong to first zipped segment.
   *    I.e. they must define its front bound.
   *
   * @return first zipped segment for `left` and `right` subsegments.
   */
  protected final def firstFrontZipper(
    left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): ZippedFirstSegment[E, D, U1, U2, V, S1, S2] =
    (left, right) match {
      case (ln: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2], rn: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2]) =>
        if (domainOps.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0)
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
        throwSegmentMustBeLastOrWithNext(left, right) // just to remove warning
    }

  /**
   * Preconditions:
   *
   * 1. All preconditions of [[generalFrontZipper]] method.
   *
   * 2. `left` and `right` must be last segments of original sequences.
   *
   * @return last zipped segment for `left` and `right` subsegments.
   */
  protected final def lastFrontZipper(
    left: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT.Last[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): ZippedLastSegment[E, D, U1, U2, V, S1, S2] =
    // First zipped segment is single if it's represented by two last subsegments => cast is safe.
    if (firstSegmentInstance.isSpecifiedBy(left, right))
      firstSegmentInstance.asInstanceOf[ZippedSingleSegment[E, D, U1, U2, V, S1, S2]]
    else
      ZippedTerminalSegment[E, D, U1, U2, V, S1, S2](this, left, right)

  /**
   * Preconditions:
   *
   * 1. All preconditions of [[generalFrontZipper]] method.
   *
   * 2. `left` and `right` must define front bound of zipped segment which is not first
   *    segment of zipped sequence.
   *
   * @return zipped segment which has previous zipped segment for `left` and `right` subsegments.
   */
  protected final def withPrevFrontZipper(
    left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): ZippedSegmentWithPrev[E, D, U1, U2, V, S1, S2] =
    (left, right) match {
      case (ln: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2], rn: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2]) =>
        if (domainOps.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0)
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
        throwSegmentMustBeLastOrWithNext(left, right) // just to remove warning
    }

  /**
   *  Preconditions:
   *
   * 1. All preconditions of [[generalFrontZipper]] method.
   *
   * 2. `left` and `right` must define front bound of zipped segment which is not last
   *     segment of zipped sequence.
   *
   * @return zipped segment which has next zipped segment for `left` and `right` subsegments.
   */
  protected final def withNextFrontZipper(
    left: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2],
    right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
  ): ZippedSegmentWithNext[E, D, U1, U2, V, S1, S2] =
    // First zipped segment is initial if one of its subsegments has next segment => cast is safe.
    if (firstSegmentInstance.isSpecifiedBy(left, right))
      firstSegmentInstance.asInstanceOf[ZippedInitialSegment[E, D, U1, U2, V, S1, S2]]
    else right match {
      case rn: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2] =>
        if (domainOps.boundOrd.compare(left.upperBound, rn.upperBound) >= 0)
          ZippedInnerSegment[E, D, U1, U2, V, S1, S2](this, rn, left)
        else
          ZippedInnerSegment[E, D, U1, U2, V, S1, S2](this, left, rn)
      case _ =>
        ZippedInnerSegment[E, D, U1, U2, V, S1, S2](this, left, right)
    }

  protected final def supplyZipper[R <: ZippedTuple[E, D, U1, U2, V, S1, S2]](
    supplied: Zipper[E, D, U1, U2, V, S1, S2, R],
    composed: ComposedZipped[E, D, U1, U2, V, S1, S2, R]
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
      var nextZipped: ZippedTuple[E, D, U1, U2, V, S1, S2] = null
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
      var prevZipped: ZippedTuple[E, D, U1, U2, V, S1, S2] = null
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
      case _ => throwSegmentMustBeLastOrWithNext(left) // just to remove warning
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
      case _ => throwSegmentMustBeFirstOrWithPrev(left) // just to remove warning
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
      case _ => throwSegmentMustBeLastOrWithNext(right) // just to remove warning
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
    val cmp = domainOps.boundOrd.compare(left.upperBound, right.upperBound)
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
    val cmp = domainOps.boundOrd.compare(left.lowerBound, right.lowerBound)
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
      zipper(backward.moveTo(forward.upperBound), forward)
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
      zipper(forward.moveTo(backward.lowerBound).asInstanceOf[SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2]], backward)
    else
      zipper(forward.movePrev, backward)

  protected final def throwSegmentMustBeLastOrWithNext(
    segments: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]*
  ): Nothing = {
    val segmentsStr = SetBuilderFormat.segmentIterable(segments, (e: E) => e.toString, (v: Any) => v.toString)
    throw new AssertionError(
      s"Expected that segments $segmentsStr are either last or has next segment."
    )
  }

  protected final def throwSegmentMustBeFirstOrWithPrev(
    segments: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]*
  ): Nothing = {
    val segmentsStr = SetBuilderFormat.segmentIterable(segments, (e: E) => e.toString, (v: Any) => v.toString)
    throw new AssertionError(
      s"Expected that segments $segmentsStr are either first or has previous segment."
    )
  }

  protected final def throwSegmentMustBelongToOriginalSeqs(
    segments: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]*
  ): Nothing = {
    val segmentsStr = SetBuilderFormat.segmentIterable(segments, (e: E) => e.toString, (v: Any) => v.toString)
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
        val ord = sequence.domainOps.segmentUpperOrd
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
        else sequence.throwSegmentMustBelongToOriginalSeqs(left, right)
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
        else sequence.throwSegmentMustBelongToOriginalSeqs(left, right)
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
   * It distinguishes backward and forward segments.
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
     * Implementation of [[OrderedZippedTuple]] with segments ordered according to [[DomainOps.segmentUpperOrd]].
     *
     * Additional preconditions:
     *
     * 1. `backward` is less then or equals to `forward` according to [[DomainOps.segmentUpperOrd]] of `sequence`.
     */
    final case class ByUpperBound[E, D <: Domain[E], U1, U2, V, S1, S2] private (
      override val sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
      override val backward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
      override val forward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
    ) extends OrderedZippedTuple[E, D, U1, U2, V, S1, S2]

    object ByUpperBound {

      /**
       * Creates ordered tuple from two original segments.
       */
      def apply[E, D <: Domain[E], U1, U2, V, S1, S2](
        sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
        left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
        right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
      ): ByUpperBound[E, D, U1, U2, V, S1, S2] =
        if (isValidOrder(sequence.domainOps.segmentUpperOrd, left, right))
          new ByUpperBound(sequence, left, right)
        else
          new ByUpperBound(sequence, right, left)

      /**
       * Creates [[Zipper]] function.
       */
      def zipper[E, D <: Domain[E], U1, U2, V, S1, S2](
        sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2]
      ): Zipper[E, D, U1, U2, V, S1, S2, ByUpperBound[E, D, U1, U2, V, S1, S2]] =
        (left, right) => apply(sequence, left, right)
    }

    /**
     * Implementation of [[OrderedZippedTuple]] with segments ordered according to [[DomainOps.segmentLowerOrd]].
     *
     * Additional preconditions:
     *
     * 1. `backward` is less then or equals to `forward` according to [[DomainOps.segmentLowerOrd]] of `sequence`.
     */
    final case class ByLowerBound[E, D <: Domain[E], U1, U2, V, S1, S2] private (
      override val sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
      override val backward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
      override val forward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
    ) extends OrderedZippedTuple[E, D, U1, U2, V, S1, S2]

    object ByLowerBound {

      /**
       * Creates ordered tuple from two original segments.
       */
      def apply[E, D <: Domain[E], U1, U2, V, S1, S2](
        sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2],
        left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
        right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
      ): ByLowerBound[E, D, U1, U2, V, S1, S2] =
        if (isValidOrder(sequence.domainOps.segmentLowerOrd, left, right))
          new ByLowerBound(sequence, left, right)
        else
          new ByLowerBound(sequence, right, left)

      /**
       * Creates [[Zipper]] function.
       */
      def zipper[E, D <: Domain[E], U1, U2, V, S1, S2](
        sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2]
      ): Zipper[E, D, U1, U2, V, S1, S2, ByLowerBound[E, D, U1, U2, V, S1, S2]] =
        (left, right) => apply(sequence, left, right)
    }

    /**
     * @return `true` if `backward` and `forward` segments are ordered according to specified `order`.
     */
    def isValidOrder[E, D <: Domain[E], U1, U2, V, S1, S2](
      order: AscOrder[Segment[E, D, ?]],
      backward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
      forward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
    ): Boolean =
      order.lteqv(backward, forward)
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
      with ZippedTuple[E, D, U1, U2, V, S1, S2] {

    // Inspection --------------------------------------------------------------- //
    override def sequence: ZippedSegmentSeq[E, D, U1, U2, V, S1, S2]

    override def isIncluded: Boolean = sequence.isValueIncluded(value)

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: ZippedFirstSegment[E, D, U1, U2, V, S1, S2] = sequence.firstSegment

    override def moveToLast: ZippedLastSegment[E, D, U1, U2, V, S1, S2] = sequence.lastSegment

    override def moveTo(bound: Bound[E]): ZippedSegment[E, D, U1, U2, V, S1, S2] =
      sequence.searchFrontZipper(sequence.generalFrontZipper, left.moveTo(bound), right.moveTo(bound))

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: SegmentSeq[E, D, V] = ???

    override def takenBelow: SegmentSeq[E, D, V] = ???

    override def sliced: (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) = ???

    override def prepended(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

    override def appended(other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

    override def truncation(bound: Bound[E]): SegmentTruncationT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V, S1, S2], this.type] = ???

    /**
     * Applies patch operation to first original sequence within current zipped segment.
     *
     * Returns sequence containing
     * <tr>
     *   - segments {i ∈ [0, L-1]: (l,,i,,, min(u,,i,,, U(lowerBound))) -> v,,i,,}
     *   of first original sequence for which l,,i,, `<` lowerBound
     * </tr>
     * <tr>
     *   - segments {i ∈ [L, M-1]: (max(lowerBound, l,,i,,), min(upperBound, u,,i,,)) -> v,,i,,}
     *   of `other` sequence for which l,,i,, `≤` upperBound and u,,i,, `≥` lowerBound
     * </tr>
     * <tr>
     *   - segments {i ∈ [M, N-1]: (max(l,,i,,, L(upperBound)), u,,i,,) -> v,,i,,}
     *   of first original sequence for which u,,i,, `>` upperBound
     * </tr>
     * <tr>where</tr>
     * <tr>lowerBound - lower bound of current zipped segment;</tr>
     * <tr>upperBound - upper bound of current zipped segment;</tr>
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
     *       lowerBound             upperBound
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
     * segment.patchedFirstSeq(other):
     *
     *   X-------](----------------------](-------X
     *       A                E               C     - values
     * }}}
     */
    def patchedFirstSeq(other: SegmentSeq[E, D, U1]): SegmentSeq[E, D, U1]

    /**
     * Applies patch operation to second original sequence within current zipped segment.
     *
     * See [[patchedFirstSeq]].
     */
    def patchedSecondSeq(other: SegmentSeq[E, D, U2]): SegmentSeq[E, D, U2]
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
    def frontBackward: SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2]
    def frontForward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]

    override def upperBound: Bound.Upper[E] = frontBackward.upperBound

    override def self: ZippedSegmentWithNext[E, D, U1, U2, V, S1, S2]

    // Navigation --------------------------------------------------------------- //
    override def moveNext: ZippedSegmentWithPrev[E, D, U1, U2, V, S1, S2] =
      sequence.stepForwardNextGenZipper(
        sequence.supplyZipper[ZippedSegmentWithPrev[E, D, U1, U2, V, S1, S2]](
          sequence.withPrevFrontZipper,
          sequence.searchFrontZipper
        ),
        frontBackward,
        frontForward
      )
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
    lazy val back: OrderedZippedTuple.ByLowerBound[E, D, U1, U2, V, S1, S2] =
      sequence.searchBackZipper(OrderedZippedTuple.ByLowerBound.zipper(sequence), left, right)

    def backBackward: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = back.backward
    // Cast is safe if precondition 1 is provided.
    def backForward: SegmentT.WithPrev[E, D, ? <: U1 | U2, ? <: S1 | S2] =
      back.forward.asInstanceOf[SegmentT.WithPrev[E, D, ? <: U1 | U2, ? <: S1 | S2]]

    override def lowerBound: Bound.Lower[E] = backForward.lowerBound

    override def self: ZippedSegmentWithPrev[E, D, U1, U2, V, S1, S2]

    // Navigation --------------------------------------------------------------- //
    override def movePrev: ZippedSegmentWithNext[E, D, U1, U2, V, S1, S2] =
      sequence.stepBackwardPrevGenZipper(sequence.withNextFrontZipper, backForward, backBackward)
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
    override def left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = frontBackward
    override def right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = frontForward

    override def self: ZippedInitialSegment[E, D, U1, U2, V, S1, S2] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: ZippedInitialSegment[E, D, U1, U2, V, S1, S2] = this

    // Transformation ----------------------------------------------------------- //
    override def patchedFirstSeq(other: SegmentSeq[E, D, U1]): SegmentSeq[E, D, U1] =
      firstSeqSegment.truncation(upperBound).prepended(other)

    override def patchedSecondSeq(other: SegmentSeq[E, D, U2]): SegmentSeq[E, D, U2] =
      secondSeqSegment.truncation(upperBound).prepended(other)
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
    override def moveToLast: ZippedTerminalSegment[E, D, U1, U2, V, S1, S2] = this

    // Transformation ----------------------------------------------------------- //
    override def patchedFirstSeq(other: SegmentSeq[E, D, U1]): SegmentSeq[E, D, U1] =
      back.firstSeqSegment.truncation(lowerBound).appended(other)

    override def patchedSecondSeq(other: SegmentSeq[E, D, U2]): SegmentSeq[E, D, U2] =
      back.secondSeqSegment.truncation(lowerBound).appended(other)
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

    // Inspection --------------------------------------------------------------- //
    override def left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = frontBackward
    override def right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2] = frontForward

    override def self: ZippedInnerSegment[E, D, U1, U2, V, S1, S2] = this

    // Transformation ----------------------------------------------------------- //
    override def patchedFirstSeq(other: SegmentSeq[E, D, U1]): SegmentSeq[E, D, U1] =
      firstSeqSegment.truncation(upperBound).prepended(
        back.firstSeqSegment.truncation(lowerBound).appended(other)
      )

    override def patchedSecondSeq(other: SegmentSeq[E, D, U2]): SegmentSeq[E, D, U2] =
      secondSeqSegment.truncation(upperBound).prepended(
        back.secondSeqSegment.truncation(lowerBound).appended(other)
      )
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
    override def isSpecifiedBy(
      left: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2],
      right: SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]
    ): Boolean =
      (validateSequences(left, right)) && left.isLast && right.isLast

    override def self: ZippedSingleSegment[E, D, U1, U2, V, S1, S2] = this

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: ZippedSingleSegment[E, D, U1, U2, V, S1, S2] = this

    override def moveToLast: ZippedSingleSegment[E, D, U1, U2, V, S1, S2] = this

    override def moveTo(bound: Bound[E]): ZippedSingleSegment[E, D, U1, U2, V, S1, S2] = this

    // Transformation ----------------------------------------------------------- //
    override def patchedFirstSeq(other: SegmentSeq[E, D, U1]): SegmentSeq[E, D, U1] = other

    override def patchedSecondSeq(other: SegmentSeq[E, D, U2]): SegmentSeq[E, D, U2] = other
  }

  // Protected section -------------------------------------------------------- //
  protected type Zipper[E, D <: Domain[E], U1, U2, V, S1, S2, +R <: ZippedTuple[E, D, U1, U2, V, S1, S2]] =
    (SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2], SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]) => R

  protected type NextGenZipper[E, D <: Domain[E], U1, U2, V, S1, S2, +R <: ZippedTuple[E, D, U1, U2, V, S1, S2]] =
    (SegmentT.WithNext[E, D, ? <: U1 | U2, ? <: S1 | S2], SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]) => R

  protected type ComposedZipped[E, D <: Domain[E], U1, U2, V, S1, S2, R <: ZippedTuple[E, D, U1, U2, V, S1, S2]] =
    (Zipper[E, D, U1, U2, V, S1, S2, R], SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2], SegmentT[E, D, ? <: U1 | U2, ? <: S1 | S2]) => R
}