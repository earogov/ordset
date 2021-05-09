package ordset.core

import ordset.core.value.ValueOps
import ordset.core.domain.{Domain, DomainOps}

// TODO: class description.
abstract class AbstractZippedSegmentSeq[E, D <: Domain[E], U1, U2, V]
  extends AbstractSegmentSeq[E, D, V, AbstractZippedSegmentSeq.ZippedSegmentBase[E, D, U1, U2, V]] {
  seq =>

  import AbstractZippedSegmentSeq._
  
  // Inspection --------------------------------------------------------------- //
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

  final override def firstSegment: ZippedFirstSegment[E, D, U1, U2, V] = firstSegmentInstance

  final override def lastSegment: ZippedLastSegment[E, D, U1, U2, V] = lastFrontZipper(firstSeq.lastSegment, secondSeq.lastSegment)

  final override def getSegment(bound: Bound[E]): ZippedSegment[E, D, U1, U2, V] =
    searchFrontZipper(generalFrontZipper, firstSeq.getSegment(bound), secondSeq.getSegment(bound))

  final override def getSegmentForElement(element: E): ZippedSegment[E, D, U1, U2, V] =
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
  protected final type Zipper[R <: ZippedTuple[E, D, U1, U2, V]] =
    (Segment[E, D, ? <: U1 | U2], Segment[E, D, ? <: U1 | U2]) => R

  protected final type NextGenZipper[R <: ZippedTuple[E, D, U1, U2, V]] =
    (Segment.WithNext[E, D, ? <: U1 | U2], Segment[E, D, ? <: U1 | U2]) => R

  protected final type ComposedZipped[R <: ZippedTuple[E, D, U1, U2, V]] =
    (Zipper[R], Segment[E, D, ? <: U1 | U2], Segment[E, D, ? <: U1 | U2]) => R

  /** Original sequence to which zipping is applied. */
  protected val firstSeq: SegmentSeq[E, D, U1]

  /** Original sequence to which zipping is applied. */
  protected val secondSeq: SegmentSeq[E, D, U2]

  /**
   * Function combining values of `firstSeq` and `secondSeq` sequences and returning value of zipped sequence.
   */
  protected def operator(first: U1, second: U2): V

  /**
   * Function that returns `true` for value `x` of `firstSeq` sequence iff
   * {{{
   * Ǝ C ∀ y: operator(x, y) = C.
   * }}}
   * I.e. input value `x` is invariant if `operator` output doesn't depend on second argument.
   */
  protected def firstInvariant(x: U1): Boolean

  /**
   * Function that returns `true` for value `y` of `secondSeq` sequence iff
   * {{{
   * Ǝ C ∀ x: operator(x, y) = C.
   * }}}
   * I.e. input value `y` is invariant if `operator` output doesn't depend on first argument.
   */
  protected def secondInvariant(x: U2): Boolean

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
  protected final lazy val firstSegmentInstance: ZippedFirstSegment[E, D, U1, U2, V] =
    searchFrontZipper(firstFrontZipper, firstSeq.firstSegment, secondSeq.firstSegment)

  /**
   * Preconditions:
   *
   * 1. `left` segment belongs to `firstSeq` or `secondSeq` sequence.
   *    `right` segment belongs to `firstSeq` or `secondSeq` sequence other then sequence of `left`.
   *
   * @return value assigned to zipped segment defined by `left` and `right` subsegments.
   * For sets it returns 'belongs to set' indicator (`Boolean`).
   * For maps `E` -> `W` it returns `Option[W]` where `None` means that segment doesn't belong to set.
   */
  protected final def getSegmentValue(left: Segment[E, D, ? <: U1 | U2], right: Segment[E, D, ? <: U1 | U2]): V = {
    (castSegmentToFirstSeq(left), castSegmentToSecondSeq(right)) match {
      case (firstSegment: Segment[E, D, U1], secondSegment: Segment[E, D, U2]) =>
        operator(firstSegment.value, secondSegment.value)
      case (null, null) =>
        (castSegmentToFirstSeq(right), castSegmentToSecondSeq(left)) match {
          case (firstSegment: Segment[E, D, U1], secondSegment: Segment[E, D, U2]) =>
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
  protected final def isInvariantSegment(segment: Segment[E, D, ? <: U1 | U2]): Boolean = {
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
  protected final def castSegmentToFirstSeq(segment: Segment[E, D, ? <: U1 | U2]): Segment[E, D, U1] | Null = {
    // We can use reference equality to check whether segment belongs to `firstSeq`. If `true` cast is safe.
    if (segment.sequence.eq(firstSeq)) segment.asInstanceOf[Segment[E, D, U1]]
    else null
  }

  /**
   * Preconditions:
   *
   * 1. `segment` belongs to `firstSeq` or `secondSeq` sequence.
   *
   * Restores segment value type if segment belongs to [[secondSeq]], otherwise returns `null`.
   */
  protected final def castSegmentToSecondSeq(segment: Segment[E, D, ? <: U1 | U2]): Segment[E, D, U2] | Null = {
    // We can use reference equality to check whether segment belongs to `secondSeq`. If `true` cast is safe.
    if (segment.sequence.eq(secondSeq)) segment.asInstanceOf[Segment[E, D, U2]]
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
    left: Segment[E, D, ? <: U1 | U2],
    right: Segment[E, D, ? <: U1 | U2]
  ): ZippedSegment[E, D, U1, U2, V] =
    if (firstSegmentInstance.isRepresentedBy(left, right)) firstSegmentInstance
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
    left: Segment[E, D, ? <: U1 | U2],
    right: Segment[E, D, ? <: U1 | U2]
  ): ZippedFirstSegment[E, D, U1, U2, V] =
    (left, right) match {
      case (ln: Segment.WithNext[E, D, ? <: U1 | U2], rn: Segment.WithNext[E, D, ? <: U1 | U2]) =>
        if (domainOps.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0)
          ZippedInitialSegment[E, D, U1, U2, V](this, rn, ln)
        else
          ZippedInitialSegment[E, D, U1, U2, V](this, ln, rn)
      case (ln: Segment.WithNext[E, D, ? <: U1 | U2], _) =>
        ZippedInitialSegment[E, D, U1, U2, V](this, ln, right)
      case (_, rn: Segment.WithNext[E, D, ? <: U1 | U2]) =>
        ZippedInitialSegment[E, D, U1, U2, V](this, rn, left)
      case (ll: Segment.Last[E, D, ? <: U1 | U2], rl: Segment.Last[E, D, ? <: U1 | U2]) =>
        ZippedSingleSegment[E, D, U1, U2, V](this, ll, rl)
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
    left: Segment.Last[E, D, ? <: U1 | U2],
    right: Segment.Last[E, D, ? <: U1 | U2]
  ): ZippedLastSegment[E, D, U1, U2, V] =
    // First zipped segment is single if it's represented by two last subsegments => cast is safe.
    if (firstSegmentInstance.isRepresentedBy(left, right))
      firstSegmentInstance.asInstanceOf[ZippedSingleSegment[E, D, U1, U2, V]]
    else
      ZippedTerminalSegment[E, D, U1, U2, V](this, left, right)

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
    left: Segment[E, D, ? <: U1 | U2],
    right: Segment[E, D, ? <: U1 | U2]
  ): ZippedSegmentWithPrev[E, D, U1, U2, V] =
    (left, right) match {
      case (ln: Segment.WithNext[E, D, ? <: U1 | U2], rn: Segment.WithNext[E, D, ? <: U1 | U2]) =>
        if (domainOps.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0)
          ZippedInnerSegment[E, D, U1, U2, V](this, rn, ln)
        else
          ZippedInnerSegment[E, D, U1, U2, V](this, ln, rn)
      case (ln: Segment.WithNext[E, D, ? <: U1 | U2], _) =>
        ZippedInnerSegment[E, D, U1, U2, V](this, ln, right)
      case (_, rn: Segment.WithNext[E, D, ? <: U1 | U2]) =>
        ZippedInnerSegment[E, D, U1, U2, V](this, rn, left)
      case (ll: Segment.Last[E, D, ? <: U1 | U2], rl: Segment.Last[E, D, ? <: U1 | U2]) =>
        ZippedTerminalSegment[E, D, U1, U2, V](this, ll, rl)
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
    left: Segment.WithNext[E, D, ? <: U1 | U2],
    right: Segment[E, D, ? <: U1 | U2]
  ): ZippedSegmentWithNext[E, D, U1, U2, V] =
    // First zipped segment is initial if one of its subsegments has next segment => cast is safe.
    if (firstSegmentInstance.isRepresentedBy(left, right))
      firstSegmentInstance.asInstanceOf[ZippedInitialSegment[E, D, U1, U2, V]]
    else right match {
      case rn: Segment.WithNext[E, D, ? <: U1 | U2] =>
        if (domainOps.boundOrd.compare(left.upperBound, rn.upperBound) >= 0)
          ZippedInnerSegment[E, D, U1, U2, V](this, rn, left)
        else
          ZippedInnerSegment[E, D, U1, U2, V](this, left, rn)
      case _ =>
        ZippedInnerSegment[E, D, U1, U2, V](this, left, right)
    }

  protected final def supplyZipper[R <: ZippedTuple[E, D, U1, U2, V]](
    supplied: Zipper[R],
    composed: ComposedZipped[R]
  ): Zipper[R] =
    (left: Segment[E, D, ? <: U1 | U2], right: Segment[E, D, ? <: U1 | U2]) => composed(supplied, left, right)

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
  protected final def searchFrontZipper[R <: ZippedTuple[E, D, U1, U2, V]](
    zipper: Zipper[R],
    left: Segment[E, D, ? <: U1 | U2],
    right: Segment[E, D, ? <: U1 | U2]
  ): R =
    if (left.isLast && right.isLast) zipper(left, right)
    else {
      var stop = false
      var nextZipped: ZippedTuple[E, D, U1, U2, V] = null
      var currZipped: ZippedTuple[E, D, U1, U2, V] = ZippedTupleImpl[E, D, U1, U2, V](this, left, right)
      while (!stop) {
        nextZipped = stepForwardZipper(ZippedTupleImpl.zipper(this), currZipped.left, currZipped.right)
        if (valueOps.neqv(currZipped.value, nextZipped.value)) {
          // We have found a bound, Vhere operator change its value => return 'currZipped'.
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
  protected final def searchBackZipper[R <: ZippedTuple[E, D, U1, U2, V]](
    zipper: Zipper[R],
    left: Segment[E, D, ? <: U1 | U2],
    right: Segment[E, D, ? <: U1 | U2]
  ): R =
    if (left.isFirst && right.isFirst) zipper(left, right)
    else {
      var stop = false
      var prevZipped: ZippedTuple[E, D, U1, U2, V] = null
      var currZipped: ZippedTuple[E, D, U1, U2, V] = ZippedTupleImpl[E, D, U1, U2, V](this, left, right)
      while (!stop) {
        prevZipped = stepBackwardZipper(ZippedTupleImpl.zipper(this), currZipped.left, currZipped.right)
        if (valueOps.neqv(currZipped.value, prevZipped.value)) {
          // We have found a bound, Vhere operator change its value => return 'currZipped'.
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
  protected final def stepForwardZipper[R <: ZippedTuple[E, D, U1, U2, V]](
    zipper: Zipper[R],
    left: Segment[E, D, ? <: U1 | U2],
    right: Segment[E, D, ? <: U1 | U2]
  ): R =
    left match {
      case ln: Segment.WithNext[E, D, ? <: U1 | U2] => stepForwardNextGenZipper(zipper, ln, right)
      // left:  ?---------------X
      // right: ?----------?
      case ll: Segment.Last[E, D, ? <: U1 | U2] => right match {
        // left:  ?---------------X
        // right: ?----------|
        case rn: Segment.WithNext[E, D, ? <: U1 | U2] => stepForwardNextLastZipper(zipper, rn, ll)
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
  protected final def stepBackwardZipper[R <: ZippedTuple[E, D, U1, U2, V]](
    zipper: Zipper[R],
    left: Segment[E, D, ? <: U1 | U2],
    right: Segment[E, D, ? <: U1 | U2]
  ): R =
    left match {
      case lp: Segment.WithPrev[E, D, ? <: U1 | U2] => stepBackwardPrevGenZipper(zipper, lp, right)
      // left:  X---------------?
      // right:      ?----------?
      case lf: Segment.First[E, D, ? <: U1 | U2] => right match {
        // left:  X---------------?
        // right:      |----------?
        case rp: Segment.WithPrev[E, D, ? <: U1 | U2] => stepBackwardFirstPrevZipper(zipper, lf, rp)
        // left:  X---------------?
        // right: X---------------?
        // Unable to make step backward => return zipped segment for current left and right values.
        case _ => zipper(left, right)
      }
      case _ => throwSegmentMustBeFirstOrWithPrev(left) // just to remove warning
    }

  /** Same as [[stepForwardZipper]] function but with stricter input types. */
  protected final def stepForwardNextGenZipper[R <: ZippedTuple[E, D, U1, U2, V]](
    zipper: Zipper[R],
    left: Segment.WithNext[E, D, ? <: U1 | U2],
    right: Segment[E, D, ? <: U1 | U2]
  ): R =
    right match {
      case rn: Segment.WithNext[E, D, ? <: U1 | U2] => stepForwardNextNextZipper(zipper, left, rn)
      case rl: Segment.Last[E, D, ? <: U1 | U2] => stepForwardNextLastZipper(zipper, left, rl)
      case _ => throwSegmentMustBeLastOrWithNext(right) // just to remove warning
    }

  /** Same as [[stepBackwardZipper]] function but with stricter input types. */
  protected final def stepBackwardPrevGenZipper[R <: ZippedTuple[E, D, U1, U2, V]](
    zipper: NextGenZipper[R],
    left: Segment.WithPrev[E, D, ? <: U1 | U2],
    right: Segment[E, D, ? <: U1 | U2]
  ): R =
    right match {
      case rp: Segment.WithPrev[E, D, ? <: U1 | U2] => stepBackwardPrevPrevZipper(zipper, rp, left)
      case rf: Segment.First[E, D, ? <: U1 | U2] => stepBackwardFirstPrevZipper(zipper, rf, left)
      case _ => throwSegmentMustBeFirstOrWithPrev(right) // just to remove warning
    }

  /** Same as [[stepForwardZipper]] function but with stricter input types. */
  protected final def stepForwardNextLastZipper[R <: ZippedTuple[E, D, U1, U2, V]](
    zipper: Zipper[R],
    backward: Segment.WithNext[E, D, ? <: U1 | U2],
    forward: Segment.Last[E, D, ? <: U1 | U2]
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
  protected final def stepBackwardFirstPrevZipper[R <: ZippedTuple[E, D, U1, U2, V]](
    zipper: NextGenZipper[R],
    backward: Segment.First[E, D, ? <: U1 | U2],
    forward: Segment.WithPrev[E, D, ? <: U1 | U2]
  ): R =
    // backward: X---------------?
    // forward:      |-----------?
    // If value of backward segment is invariant (i.e. value of forward segment doesn't affect on operator),
    // we can immediately define operator value up to lower bound of backward segment skipping segments of
    // forward sequence above it.
    // Input forward segment has previous segment => all previous segments of corresponding sequence has next segment =>
    // cast is safe.
    if (isInvariantSegment(backward))
      zipper(forward.moveToFirst.asInstanceOf[Segment.WithNext[E, D, ? <: U1 | U2]], backward)
    else
      zipper(forward.movePrev, backward)

  /** Same as [[stepForwardZipper]] function but with stricter input types. */
  protected final def stepForwardNextNextZipper[R <: ZippedTuple[E, D, U1, U2, V]](
    zipper: Zipper[R],
    left: Segment.WithNext[E, D, ? <: U1 | U2],
    right: Segment.WithNext[E, D, ? <: U1 | U2]
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
  protected final def stepBackwardPrevPrevZipper[R <: ZippedTuple[E, D, U1, U2, V]](
    zipper: NextGenZipper[R],
    left: Segment.WithPrev[E, D, ? <: U1 | U2],
    right: Segment.WithPrev[E, D, ? <: U1 | U2]
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
  protected final def stepForwardNextNextDiffZipper[R <: ZippedTuple[E, D, U1, U2, V]](
    zipper: Zipper[R],
    backward: Segment.WithNext[E, D, ? <: U1 | U2],
    forward: Segment.WithNext[E, D, ? <: U1 | U2]
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
  protected final def stepBackwardPrevPrevDiffZipper[R <: ZippedTuple[E, D, U1, U2, V]](
    zipper: NextGenZipper[R],
    backward: Segment.WithPrev[E, D, ? <: U1 | U2],
    forward: Segment.WithPrev[E, D, ? <: U1 | U2]
  ): R =
    // backward: |---------------?
    // forward:      |-----------?
    // If value of backward segment is invariant (i.e. value of forward segment doesn't affect on operator),
    // we can immediately define operator value up to lower bound of backward segment skipping segments of
    // forward sequence above it.
    // Input forward segment has previous segment => all previous segments of corresponding sequence has next segment
    // => cast is safe.
    if (isInvariantSegment(backward))
      zipper(forward.moveTo(backward.lowerBound).asInstanceOf[Segment.WithNext[E, D, ? <: U1 | U2]], backward)
    else
      zipper(forward.movePrev, backward)

  protected final def throwSegmentMustBeLastOrWithNext(segments: Segment[E, D, ? <: U1 | U2]*): Nothing = {
    val segmentsStr = SetBuilderFormat.segmentIterable(segments, (e: E) => e.toString, (v: Any) => v.toString);
    throw new AssertionError(
      s"Expected segments $segmentsStr are either last or has next segment."
    )
  }

  protected final def throwSegmentMustBeFirstOrWithPrev(segments: Segment[E, D, ? <: U1 | U2]*): Nothing = {
    val segmentsStr = SetBuilderFormat.segmentIterable(segments, (e: E) => e.toString, (v: Any) => v.toString);
    throw new AssertionError(
      s"Expected segments $segmentsStr are either first or has previous segment."
    )
  }

  protected final def throwSegmentMustBelongToOriginalSeqs(segments: Segment[E, D, ? <: U1 | U2]*): Nothing = {
    val segmentsStr = SetBuilderFormat.segmentIterable(segments, (e: E) => e.toString, (v: Any) => v.toString);
    throw new AssertionError(
      s"Expected segments $segmentsStr belong to one of original sequences."
    )
  }
}

object AbstractZippedSegmentSeq {

  type ZippedSegment[E, D <: Domain[E], U1, U2, V] =
    SegmentT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V]] with ZippedSegmentBase[E, D, U1, U2, V]

  type ZippedFirstSegment[E, D <: Domain[E], U1, U2, V] =
    SegmentT.First[E, D, V, ZippedSegmentBase[E, D, U1, U2, V]] with ZippedSegmentBase[E, D, U1, U2, V]

  type ZippedLastSegment[E, D <: Domain[E], U1, U2, V] =
    SegmentT.Last[E, D, V, ZippedSegmentBase[E, D, U1, U2, V]] with ZippedSegmentBase[E, D, U1, U2, V]

  /**
   * Pair of subsegments of original sequences.
   *
   * Preconditions:
   *
   * 1. `left` segment belongs to `firstSeq` or `secondSeq` sequence.
   *    `right` segment belongs to `firstSeq` or `secondSeq` sequence other then sequence of `left`.
   */
  sealed trait ZippedTuple[E, D <: Domain[E], U1, U2, V] {

    def sequence: AbstractZippedSegmentSeq[E, D, U1, U2, V]

    def left: Segment[E, D, ? <: U1 | U2]

    def right: Segment[E, D, ? <: U1 | U2]

    /** @return `true` if subsegments of tuple correspond to input `left` and `right`.
     *           Which is `left` and which is `right` doesn't matter.
     */
    def isRepresentedBy(left: Segment[E, D, ?], right: Segment[E, D, ?]): Boolean = {
      val ord = sequence.domainOps.segmentUpperOrd
      if      (ord.eqv(left, this.left))  ord.eqv(right, this.right)
      else if (ord.eqv(right, this.left)) ord.eqv(left, this.right)
      else                                false
    }

    def firstSeqSegment: Segment[E, D, U1] = {
      var segment = sequence.castSegmentToFirstSeq(left)
      if (segment != null) segment
      else {
        segment = sequence.castSegmentToFirstSeq(right)
        if (segment != null) segment
        else sequence.throwSegmentMustBelongToOriginalSeqs(left, right)
      }
    }

    def secondSeqSegment: Segment[E, D, U2] = {
      var segment = sequence.castSegmentToSecondSeq(left)
      if (segment != null) segment
      else {
        segment = sequence.castSegmentToSecondSeq(right)
        if (segment != null) segment
        else sequence.throwSegmentMustBelongToOriginalSeqs(left, right)
      }
    }

    lazy val value: V = sequence.getSegmentValue(left, right)
  }

  final case class ZippedTupleImpl[E, D <: Domain[E], U1, U2, V](
    override val sequence: AbstractZippedSegmentSeq[E, D, U1, U2, V],
    override val left: Segment[E, D, ? <: U1 | U2],
    override val right: Segment[E, D, ? <: U1 | U2]
  ) extends ZippedTuple[E, D, U1, U2, V]

  object ZippedTupleImpl {

    def zipper[E, D <: Domain[E], U1, U2, V](
      sequence: AbstractZippedSegmentSeq[E, D, U1, U2, V]
    ): (Segment[E, D, ? <: U1 | U2], Segment[E, D, ? <: U1 | U2]) => ZippedTuple[E, D, U1, U2, V] =
      (left, right) => new ZippedTupleImpl[E, D, U1, U2, V](sequence, left, right)
  }

  /**
   * Base trait for zipped segments.
   *
   * Upper and lower bounds of zipped segments are defined by change of the operator value.
   * Zipped segments are represented by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]]) 
   * which must specify the upper bound.
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
  sealed trait ZippedSegmentBase[E, D <: Domain[E], U1, U2, V]
    extends SegmentLikeT[E, D, V, ZippedSegmentBase[E, D, U1, U2, V]]
      with ZippedTuple[E, D, U1, U2, V] {

    // Inspection --------------------------------------------------------------- //
    override def sequence: AbstractZippedSegmentSeq[E, D, U1, U2, V]

    override def isIncluded: Boolean = sequence.isValueIncluded(value)

    override def firstSeqSegment: Segment[E, D, U1] = super.firstSeqSegment

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: ZippedFirstSegment[E, D, U1, U2, V] = sequence.firstSegment

    override def moveToLast: ZippedLastSegment[E, D, U1, U2, V] = sequence.lastSegment

    override def moveTo(bound: Bound[E]): ZippedSegment[E, D, U1, U2, V] =
      sequence.searchFrontZipper(
        sequence.generalFrontZipper, 
        left.moveTo(bound), 
        right.moveTo(bound)
      )

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: SegmentSeq[E, D, V] = ???

    override def takenBelow: SegmentSeq[E, D, V] = ???

    override def sliced: (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) = ???
  }

  /**
   * Zipped segment with next segment.
   *
   * Segment is represented by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]]) 
   * which must specify the upper bound. 'frontBackward' and 'frontForward' are the same two subsegments 
   * which are ordered by their upper bound.
   */
  sealed trait ZippedSegmentWithNext[E, D <: Domain[E], U1, U2, V]
    extends SegmentT.WithNext[E, D, V, ZippedSegmentBase[E, D, U1, U2, V]]
      with ZippedSegmentBase[E, D, U1, U2, V] {

    // Inspection --------------------------------------------------------------- //
    def frontBackward: Segment.WithNext[E, D, ? <: U1 | U2]
    def frontForward: Segment[E, D, ? <: U1 | U2]

    override def upperBound: Bound.Upper[E] = frontBackward.upperBound

    // Navigation --------------------------------------------------------------- //
    override def moveNext: ZippedSegmentWithPrev[E, D, U1, U2, V] =
      sequence.stepForwardNextGenZipper(
        sequence.supplyZipper[ZippedSegmentWithPrev[E, D, U1, U2, V]](
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
   * Segment is represented by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]]) 
   * which must specify the upper bound.
   * 
   * Lower bound is defined by 'backBackward' and 'backForward' subsegments and is searched lazily.
   * 'backBackward' and 'backForward' subsegments are ordered by their lower bound.
   * {{{
   *
   *        backBackward                 frontLeft
   *            V                            V
   *       -------------|-------------|-------------|-------------
   *              A     |     B              C      |     D         - segment value
   *                    |                           |
   *                backForward               frontRight
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
  sealed trait ZippedSegmentWithPrev[E, D <: Domain[E], U1, U2, V]
    extends SegmentT.WithPrev[E, D, V, ZippedSegmentBase[E, D, U1, U2, V]]
      with ZippedSegmentBase[E, D, U1, U2, V] {

    // Inspection --------------------------------------------------------------- //
    def backBackward: Segment[E, D, ? <: U1 | U2] = back.backward
    // Cast is safe if precondition 1 is provided.
    def backForward: Segment.WithPrev[E, D, ? <: U1 | U2] = back.forward.asInstanceOf[Segment.WithPrev[E, D, ? <: U1 | U2]]

    override def lowerBound: Bound.Lower[E] = backForward.lowerBound

    // Navigation --------------------------------------------------------------- //
    override def movePrev: ZippedSegmentWithNext[E, D, U1, U2, V] =
      sequence.stepBackwardPrevGenZipper(
        sequence.withNextFrontZipper, 
        backForward, 
        backBackward
      )

    // Private section ---------------------------------------------------------- //
    private lazy val back: Back = {
      val backTuple = sequence.searchBackZipper(ZippedTupleImpl.zipper(sequence), left, right)
      if (domainOps.segmentLowerOrd.compare(backTuple.left, backTuple.right) >= 0)
        Back(backTuple.right, backTuple.left)
      else
        Back(backTuple.left, backTuple.right)
    }

    private case class Back(backward: Segment[E, D, ? <: U1 | U2], forward: Segment[E, D, ? <: U1 | U2])
  }

  /**
   * Initial segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]]) 
   * which must specify the upper bound. 'frontBackward' and 'frontForward' are the same two subsegments 
   * which are ordered by their upper bound.
   */
  final case class ZippedInitialSegment[E, D <: Domain[E], U1, U2, V] (
    override val sequence: AbstractZippedSegmentSeq[E, D, U1, U2, V],
    override val frontBackward: Segment.WithNext[E, D, ? <: U1 | U2],
    override val frontForward: Segment[E, D, ? <: U1 | U2]
  ) extends SegmentT.Initial[E, D, V, ZippedSegmentBase[E, D, U1, U2, V]]
    with ZippedSegmentWithNext[E, D, U1, U2, V] {

    // Inspection --------------------------------------------------------------- //
    override def left: Segment[E, D, ? <: U1 | U2] = frontBackward
    override def right: Segment[E, D, ? <: U1 | U2] = frontForward

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: ZippedInitialSegment[E, D, U1, U2, V] = this

    // Protected section -------------------------------------------------------- //
    protected override def self: ZippedInitialSegment[E, D, U1, U2, V] = this
  }

  /**
   * Terminal segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]]) 
   * which must be last segments of original sequences.
   */
  final case class ZippedTerminalSegment[E, D <: Domain[E], U1, U2, V] (
    override val sequence: AbstractZippedSegmentSeq[E, D, U1, U2, V],
    override val left: Segment.Last[E, D, ? <: U1 | U2],
    override val right: Segment.Last[E, D, ? <: U1 | U2]
  ) extends SegmentT.Terminal[E, D, V, ZippedSegmentBase[E, D, U1, U2, V]]
    with ZippedSegmentWithPrev[E, D, U1, U2, V] {

    // Inspection --------------------------------------------------------------- //
    override def isRepresentedBy(left: Segment[E, D, ?], right: Segment[E, D, ?]): Boolean =
      left.isLast && right.isLast

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: ZippedTerminalSegment[E, D, U1, U2, V] = this

    // Protected section -------------------------------------------------------- //
    protected override def self: ZippedTerminalSegment[E, D, U1, U2, V] = this
  }
  
  /**
   * Inner segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]]) 
   * which must specify the upper bound. 'frontBackward' and 'frontForward' are the same two subsegments 
   * which are ordered by their upper bound.
   */
  final case class ZippedInnerSegment[E, D <: Domain[E], U1, U2, V](
    override val sequence: AbstractZippedSegmentSeq[E, D, U1, U2, V],
    override val frontBackward: Segment.WithNext[E, D, ? <: U1 | U2],
    override val frontForward: Segment[E, D, ? <: U1 | U2]
  ) extends SegmentT.Inner[E, D, V, ZippedSegmentBase[E, D, U1, U2, V]]
    with ZippedSegmentWithPrev[E, D, U1, U2, V]
    with ZippedSegmentWithNext[E, D, U1, U2, V] {

    // Inspection --------------------------------------------------------------- //
    override def left: Segment[E, D, ? <: U1 | U2] = frontBackward
    override def right: Segment[E, D, ? <: U1 | U2] = frontForward

    // Protected section -------------------------------------------------------- //
    protected override def self: ZippedInnerSegment[E, D, U1, U2, V] = this
  }

  /**
   * Single segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments (see preconditions of [[ZippedTuple]]) 
   * which must be last segments of original sequences.
   */
  final case class ZippedSingleSegment[E, D <: Domain[E], U1, U2, V](
    override val sequence: AbstractZippedSegmentSeq[E, D, U1, U2, V],
    override val left: Segment.Last[E, D, ? <: U1 | U2],
    override val right: Segment.Last[E, D, ? <: U1 | U2]
  ) extends SegmentT.Single[E, D, V, ZippedSegmentBase[E, D, U1, U2, V]]
    with ZippedSegmentBase[E, D, U1, U2, V] {

    // Inspection --------------------------------------------------------------- //
    override def isRepresentedBy(left: Segment[E, D, ?], right: Segment[E, D, ?]): Boolean =
      left.isLast && right.isLast

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: ZippedSingleSegment[E, D, U1, U2, V] = this

    override def moveToLast: ZippedSingleSegment[E, D, U1, U2, V] = this

    override def moveTo(bound: Bound[E]): ZippedSingleSegment[E, D, U1, U2, V] = this

    // Protected section -------------------------------------------------------- //
    protected override def self: ZippedSingleSegment[E, D, U1, U2, V] = this
  }
}