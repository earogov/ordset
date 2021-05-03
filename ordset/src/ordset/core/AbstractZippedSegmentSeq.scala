package ordset.core

import ordset.core.value.ValueOps
import ordset.core.domain.{Domain, DomainOps}

// TODO: class description.
abstract class AbstractZippedSegmentSeq[E, D <: Domain[E], V]
  extends AbstractSegmentSeq[E, D, V, AbstractZippedSegmentSeq.ZippedSegmentBase[E, D, V]] {
  seq =>

  import AbstractZippedSegmentSeq._
  
  // Inspection --------------------------------------------------------------- //
  final override def isEmpty: Boolean =
    firstSegmentInstance.isSingle && !isIncludedInSet(firstSegmentInstance.value)

  final override def isUniversal: Boolean =
    firstSegmentInstance.isSingle && isIncludedInSet(firstSegmentInstance.value)

  final override def isUniform: Boolean =
    firstSegmentInstance.isSingle

  final override def contains(bound: Bound[E]): Boolean =
    isIncludedInSet(getSegmentValue(left.getSegment(bound), right.getSegment(bound)))

  final override def containsElement(element: E): Boolean = super.containsElement(element)

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = forwardUpperBoundsFromSegment(firstSegment)

  final override def firstSegment: ZippedFirstSegment[E, D, V] = firstSegmentInstance

  final override def lastSegment: ZippedLastSegment[E, D, V] = lastFrontZipper(left.lastSegment, right.lastSegment)

  final override def getSegment(bound: Bound[E]): ZippedSegment[E, D, V] =
    searchFrontZipper(generalFrontZipper, left.getSegment(bound), right.getSegment(bound))

  final override def getSegmentForElement(element: E): ZippedSegment[E, D, V] = getSegment(Bound.Upper.inclusive(element))

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): SegmentSeq[E, D, V] =
    cons(left.takenAbove(bound), right.takenAbove(bound))

  final override def takenBelow(bound: Bound[E]): SegmentSeq[E, D, V] =
    cons(left.takenBelow(bound), right.takenBelow(bound))

  final override def sliced(bound: Bound[E]): (SegmentSeq[E, D, V], SegmentSeq[E, D, V]) =
    (takenBelow(bound), takenAbove(bound))
  
  final override def appended(bound: Bound[E], other: SegmentSeq[E, D, V]): SegmentSeq[E, D, V] = ???

  // Protected section -------------------------------------------------------- //
  protected final type Zipper[R <: ZippedTuple[E, D, V]] = (Segment[E, D, V], Segment[E, D, V]) => R
  protected final type NextGenZipper[R <: ZippedTuple[E, D, V]] = (Segment.WithNext[E, D, V], Segment[E, D, V]) => R
  protected final type ComposedZipped[R <: ZippedTuple[E, D, V]] = (Zipper[R], Segment[E, D, V], Segment[E, D, V]) => R

  /** Original sequence to which zipping is applied. */
  protected val left: SegmentSeq[E, D, V]

  /** Original sequence to which zipping is applied. */
  protected val right: SegmentSeq[E, D, V]

  /**
   * Function combining values of `left` and `right` sequences and returning value of zipped sequence.
   * Operator must be commutative:
   * {{{
   * operator(x, y) == operator(y, x).
   * }}}
   */
  protected def operator(left: V, right: V): V

  /**
   * Function that returns `true` for value `x` if and only if
   * {{{
   * Ǝ C ∀ y: operator(x, y) = C.
   * }}}
   * I.e. input `value` is invariant if operator value doesn't depend on second argument.
   */
  protected def invariant(value: V): Boolean

  /**
   * Creates zipped segment sequence.
   */
  protected def cons(left: SegmentSeq[E, D, V], right: SegmentSeq[E, D, V]): SegmentSeq[E, D, V]

  /**
   * @return value assigned to zipped segment defined by `left` and `right` subsegments.
   * For sets it returns 'belongs to set' indicator (`Boolean`).
   * For maps `E` -> `W` it returns `Option[W]` where `None` means that segment doesn't belong to set.
   */
  protected def getSegmentValue(left: Segment[E, D, V], right: Segment[E, D, V]): V =
    // TODO: check invariant for segment with minimal complexity (number of children).
    if (invariant(left.value)) left.value
    else operator(left.value, right.value)

  /**
   * Returns `true` if segment with given value is considered to be included in set.
   *
   * For example, if `V` = `Option[AnyType]`, then we assume `None` is not included and `Some(anyValue)` - is included.
   */
  protected def isIncludedInSet(value: V): Boolean

  /** First segment of sequence. It's either initial ot single. */
  protected final lazy val firstSegmentInstance: ZippedFirstSegment[E, D, V] =
    searchFrontZipper(firstFrontZipper, left.firstSegment, right.firstSegment)

  /**
   * Preconditions:
   *
   * 1. `left` and `right` are just labels for subsegments, they don't have to correspond to
   *    `left` and `right` original sequences. `left` subsegment may belong to `right` sequence
   *     and vice versa. The only requirement is subsegments must belong to different sequences.
   *
   * 2. `left` and `right` subsegments must define front bound of zipped segment
   *     (operator must change its value for next pair of subsegments).
   *
   * @return zipped segment for `left` and `right` subsegments.
   */
  protected final def generalFrontZipper(
    left: Segment[E, D, V], 
    right: Segment[E, D, V]
  ): ZippedSegment[E, D, V] =
    if (firstSegmentInstance.isRepresentedBy(left, right)) firstSegmentInstance
    else withPrevFrontZipper(left, right)

  /**
   * Preconditions:
   *
   * 1. All preconditions of [[generalFrontZipper]] method.
   *
   * 2. `left` and `right` subsegment must belong to first zipped segment. I.e. they must define
   *    its front bound.
   *
   * @return first zipped segment for `left` and `right` subsegments.
   */
  protected final def firstFrontZipper(
    left: Segment[E, D, V], 
    right: Segment[E, D, V]
  ): ZippedFirstSegment[E, D, V] =
    (left, right) match {
      case (ln: Segment.WithNext[E, D, V], rn: Segment.WithNext[E, D, V]) =>
        if (domainOps.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0) 
          ZippedInitialSegment[E, D, V](this, rn, ln)
        else                                                               
          ZippedInitialSegment[E, D, V](this, ln, rn)
      case (ln: Segment.WithNext[E, D, V], _) => 
        ZippedInitialSegment[E, D, V](this, ln, right)
      case (_, rn: Segment.WithNext[E, D, V]) => 
        ZippedInitialSegment[E, D, V](this, rn, left)
      case (ll: Segment.Last[E, D, V], rl: Segment.Last[E, D, V]) => 
        ZippedSingleSegment[E, D, V](this, ll, rl)
      case _ => 
        throwSegmentIsEitherLastOrHasNext // just to remove warning
    }

  /**
   * Preconditions:
   *
   * 1. All preconditions of [[generalFrontZipper]] method.
   *
   * 2. `left` and `right` must be last segments of original sequences.
   *
   * @return last zipped segment for `left` and `right` subsegments.
   *
   */
  protected final def lastFrontZipper(
    left: Segment.Last[E, D, V], 
    right: Segment.Last[E, D, V]
  ): ZippedLastSegment[E, D, V] =
    // First zipped segment is single if it's represented by two last subsegments => cast is safe.
    if (firstSegmentInstance.isRepresentedBy(left, right)) 
      firstSegmentInstance.asInstanceOf[ZippedSingleSegment[E, D, V]]
    else 
      ZippedTerminalSegment[E, D, V](this, left, right)

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
    left: Segment[E, D, V], 
    right: Segment[E, D, V]
  ): ZippedSegmentWithPrev[E, D, V] =
    (left, right) match {
      case (ln: Segment.WithNext[E, D, V], rn: Segment.WithNext[E, D, V]) =>
        if (domainOps.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0) 
          ZippedInnerSegment[E, D, V](this, rn, ln)
        else                                                               
          ZippedInnerSegment[E, D, V](this, ln, rn)
      case (ln: Segment.WithNext[E, D, V], _) => 
        ZippedInnerSegment[E, D, V](this, ln, right)
      case (_, rn: Segment.WithNext[E, D, V]) => 
        ZippedInnerSegment[E, D, V](this, rn, left)
      case (ll: Segment.Last[E, D, V], rl: Segment.Last[E, D, V]) => 
        ZippedTerminalSegment[E, D, V](this, ll, rl)
      case _ => 
        throwSegmentIsEitherLastOrHasNext // just to remove warning
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
    left: Segment.WithNext[E, D, V], 
    right: Segment[E, D, V]
  ): ZippedSegmentWithNext[E, D, V] =
    // First zipped segment is initial if one of its subsegments has next segment => cast is safe.
    if (firstSegmentInstance.isRepresentedBy(left, right)) 
      firstSegmentInstance.asInstanceOf[ZippedInitialSegment[E, D, V]]
    else right match {
      case rn: Segment.WithNext[E, D, V] =>
        if (domainOps.boundOrd.compare(left.upperBound, rn.upperBound) >= 0) 
          ZippedInnerSegment[E, D, V](this, rn, left)
        else                                                                 
          ZippedInnerSegment[E, D, V](this, left, rn)
      case _ => 
        ZippedInnerSegment[E, D, V](this, left, right)
    }
  
  protected final def supplyZipper[R <: ZippedTuple[E, D, V]](
    supplied: Zipper[R],
    composed: ComposedZipped[R]
  ): Zipper[R] =
    (left: Segment[E, D, V], right: Segment[E, D, V]) => composed(supplied, left, right)

  /**
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
  protected final def searchFrontZipper[R <: ZippedTuple[E, D, V]](
    zipper: Zipper[R],
    left: Segment[E, D, V],
    right: Segment[E, D, V]
  ): R =
    if (left.isLast && right.isLast) zipper(left, right)
    else {
      var stop = false
      var nextZipped: ZippedTuple[E, D, V] = null
      var currZipped: ZippedTuple[E, D, V] = ZippedTupleImpl[E, D, V](this, left, right)
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
  protected final def searchBackZipper[R <: ZippedTuple[E, D, V]](
    zipper: Zipper[R],
    left: Segment[E, D, V],
    right: Segment[E, D, V]
  ): R =
    if (left.isFirst && right.isFirst) zipper(left, right)
    else {
      var stop = false
      var prevZipped: ZippedTuple[E, D, V] = null
      var currZipped: ZippedTuple[E, D, V] = ZippedTupleImpl[E, D, V](this, left, right)
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
   * Starting from `left` and `right` subsegments function get next pair of subsegments and builds zipped segment
   * with `zipper` function.
   *
   * If `left` and `right` are both last segments of original sequences then `zipper` function is applied to them.
   */
  protected final def stepForwardZipper[R <: ZippedTuple[E, D, V]](
    zipper: Zipper[R],
    left: Segment[E, D, V],
    right: Segment[E, D, V]
  ): R =
    left match {
      case ln: Segment.WithNext[E, D, V] => stepForwardNextGenZipper(zipper, ln, right)
      // left:  ?---------------X
      // right: ?----------?
      case ll: Segment.Last[E, D, V] => right match {
        // left:  ?---------------X
        // right: ?----------|
        case rn: Segment.WithNext[E, D, V] => stepForwardNextLastZipper(zipper, rn, ll)
        // left:  ?---------------X
        // right: ?---------------X
        // Unable to make step forward => return zipped segment for current left and right values.
        case _ => zipper(left, right)
      }
      case _ => throwSegmentIsEitherLastOrHasNext // just to remove warning
    }

  /**
   * Starting from `left` and `right` subsegments function get previous pair of subsegments and builds zipped segment
   * with `zipper` function.
   *
   * If `left` and `right` are both first segments of original sequences then `zipper` function is applied to them.
   */
  protected final def stepBackwardZipper[R <: ZippedTuple[E, D, V]](
    zipper: Zipper[R],
    left: Segment[E, D, V],
    right: Segment[E, D, V]
  ): R =
    left match {
      case lp: Segment.WithPrev[E, D, V] => stepBackwardPrevGenZipper(zipper, lp, right)
      // left:  X---------------?
      // right:      ?----------?
      case lf: Segment.First[E, D, V] => right match {
        // left:  X---------------?
        // right:      |----------?
        case rp: Segment.WithPrev[E, D, V] => stepBackwardFirstPrevZipper(zipper, lf, rp)
        // left:  X---------------?
        // right: X---------------?
        // Unable to make step backward => return zipped segment for current left and right values.
        case _ => zipper(left, right)
      }
      case _ => throwSegmentIsEitherFirstOrHasPrev // just to remove warning
    }

  /** Same as [[stepForwardZipper]] function but with stricter input types. */
  protected final def stepForwardNextGenZipper[R <: ZippedTuple[E, D, V]](
    zipper: Zipper[R],
    left: Segment.WithNext[E, D, V],
    right: Segment[E, D, V]
  ): R =
    right match {
      case rn: Segment.WithNext[E, D, V] => stepForwardNextNextZipper(zipper, left, rn)
      case rl: Segment.Last[E, D, V] => stepForwardNextLastZipper(zipper, left, rl)
      case _ => throwSegmentIsEitherLastOrHasNext // just to remove warning
    }

  /** Same as [[stepBackwardZipper]] function but with stricter input types. */
  protected final def stepBackwardPrevGenZipper[R <: ZippedTuple[E, D, V]](
    zipper: NextGenZipper[R],
    left: Segment.WithPrev[E, D, V],
    right: Segment[E, D, V]
  ): R =
    right match {
      case rp: Segment.WithPrev[E, D, V] => stepBackwardPrevPrevZipper(zipper, rp, left)
      case rf: Segment.First[E, D, V] => stepBackwardFirstPrevZipper(zipper, rf, left)
      case _ => throwSegmentIsEitherFirstOrHasPrev // just to remove warning
    }

  /** Same as [[stepForwardZipper]] function but with stricter input types. */
  protected final def stepForwardNextLastZipper[R <: ZippedTuple[E, D, V]](
    zipper: Zipper[R],
    backward: Segment.WithNext[E, D, V],
    forward: Segment.Last[E, D, V]
  ): R =
    // backward: ?----------|
    // forward:  ?---------------X
    // If value of forward segment is invariant (i.e. value of backward segment doesn't affect on operator),
    // we can immediately define operator value up to upper bound of forward segment skipping segments of
    // backward sequence below it.
    if (invariant(forward.value)) 
      zipper(backward.moveToLast, forward)
    else 
      zipper(backward.moveNext, forward)

  /** Same as [[stepBackwardZipper]] function but with stricter input types. */
  protected final def stepBackwardFirstPrevZipper[R <: ZippedTuple[E, D, V]](
    zipper: NextGenZipper[R],
    backward: Segment.First[E, D, V],
    forward: Segment.WithPrev[E, D, V]
  ): R =
    // backward: X---------------?
    // forward:      |-----------?
    // If value of backward segment is invariant (i.e. value of forward segment doesn't affect on operator),
    // we can immediately define operator value up to lower bound of backward segment skipping segments of
    // forward sequence above it.
    // Input forward segment has previous segment => all previous segments of corresponding sequence has next segment =>
    // cast is safe.
    if (invariant(backward.value)) 
      zipper(forward.moveToFirst.asInstanceOf[Segment.WithNext[E, D, V]], backward)
    else 
      zipper(forward.movePrev, backward)

  /** Same as [[stepForwardZipper]] function but with stricter input types. */
  protected final def stepForwardNextNextZipper[R <: ZippedTuple[E, D, V]](
    zipper: Zipper[R],
    left: Segment.WithNext[E, D, V],
    right: Segment.WithNext[E, D, V]
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
  protected final def stepBackwardPrevPrevZipper[R <: ZippedTuple[E, D, V]](
    zipper: NextGenZipper[R],
    left: Segment.WithPrev[E, D, V],
    right: Segment.WithPrev[E, D, V]
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
  protected final def stepForwardNextNextDiffZipper[R <: ZippedTuple[E, D, V]](
    zipper: Zipper[R],
    backward: Segment.WithNext[E, D, V],
    forward: Segment.WithNext[E, D, V]
  ): R =
    // backward: ?----------|
    // forward:  ?---------------|
    // If value of forward segment is invariant (i.e. value of backward segment doesn't affect on operator),
    // we can immediately define operator value up to upper bound of forward segment skipping segments of
    // backward sequence below it.
    if (invariant(forward.value)) 
      zipper(backward.moveTo(forward.upperBound), forward)
    else 
      zipper(backward.moveNext, forward)

  /**
   * Same as [[stepBackwardPrevPrevZipper]] function with additional precondition:
   *  1. Lower bounds of `left` and `right` subsegments are not equal.
   */
  protected final def stepBackwardPrevPrevDiffZipper[R <: ZippedTuple[E, D, V]](
    zipper: NextGenZipper[R],
    backward: Segment.WithPrev[E, D, V],
    forward: Segment.WithPrev[E, D, V]
  ): R =
    // backward: |---------------?
    // forward:      |-----------?
    // If value of backward segment is invariant (i.e. value of forward segment doesn't affect on operator),
    // we can immediately define operator value up to lower bound of backward segment skipping segments of
    // forward sequence above it.
    // Input forward segment has previous segment => all previous segments of corresponding sequence has next segment
    // => cast is safe.
    if (invariant(backward.value)) 
      zipper(forward.moveTo(backward.lowerBound).asInstanceOf[Segment.WithNext[E, D, V]], backward)
    else 
      zipper(forward.movePrev, backward)

  protected final def throwSegmentIsEitherLastOrHasNext: Nothing = {
    throw new AssertionError("Unreachable case: segment is either last or has next segment.")
  }

  protected final def throwSegmentIsEitherFirstOrHasPrev: Nothing =
    throw new AssertionError("Unreachable case: segment is either first or has previous segment.")
}

object AbstractZippedSegmentSeq {

  type ZippedSegment[E, D <: Domain[E], V] = 
    SegmentT[E, D, V, ZippedSegmentBase[E, D, V]] with ZippedSegmentBase[E, D, V]

  type ZippedFirstSegment[E, D <: Domain[E], V] =
    SegmentT.First[E, D, V, ZippedSegmentBase[E, D, V]] with ZippedSegmentBase[E, D, V]

  type ZippedLastSegment[E, D <: Domain[E], V] =
    SegmentT.Last[E, D, V, ZippedSegmentBase[E, D, V]] with ZippedSegmentBase[E, D, V]

  /**
   * Pair of subsegments of original sequences.
   *
   * `left` and `right` are just labels for subsegments,
   * they don't have to correspond to `left` and `right` original sequences. `left` subsegment may belong to
   * `right` sequence and vice versa. The only requirement is subsegments must belong to different sequences.
   */
  sealed trait ZippedTuple[E, D <: Domain[E], V] {

    def sequence: AbstractZippedSegmentSeq[E, D, V]
    
    def left: Segment[E, D, V]
    def right: Segment[E, D, V]

    /** @return `true` if subsegments of tuple correspond to input `left` and `right`.
     *           Which is `left` and which is `right` doesn't matter.
     */
    def isRepresentedBy(left: Segment[E, D, V], right: Segment[E, D, V]): Boolean = {
      val ord = sequence.domainOps.segmentUpperOrd
      if      (ord.eqv(left, this.left))  ord.eqv(right, this.right)
      else if (ord.eqv(right, this.left)) ord.eqv(left, this.right)
      else                                false
    }

    lazy val value: V = sequence.getSegmentValue(left, right)
  }

  final case class ZippedTupleImpl[E, D <: Domain[E], V](
    override val sequence: AbstractZippedSegmentSeq[E, D, V],
    override val left: Segment[E, D, V], 
    override val right: Segment[E, D, V]
  ) extends ZippedTuple[E, D, V]
  
  object ZippedTupleImpl {
    
    def zipper[E, D <: Domain[E], V](
      sequence: AbstractZippedSegmentSeq[E, D, V]
    ): (Segment[E, D, V], Segment[E, D, V]) => ZippedTuple[E, D, V] =
      (left, right) => new ZippedTupleImpl[E, D, V](sequence, left, right)
  }

  /**
   * Base trait for zipped segments.
   *
   * Upper and lower bounds of zipped segments are defined by change of the operator value.
   * Zipped segments are represented by 'left' and 'right' subsegments which must specify the upper bound.
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
  sealed trait ZippedSegmentBase[E, D <: Domain[E], V] 
    extends SegmentLikeT[E, D, V, ZippedSegmentBase[E, D, V]]
      with ZippedTuple[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def sequence: AbstractZippedSegmentSeq[E, D, V]
    
    override def isIncluded: Boolean = sequence.isIncludedInSet(value)

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: ZippedFirstSegment[E, D, V] = sequence.firstSegment

    override def moveToLast: ZippedLastSegment[E, D, V] = sequence.lastSegment

    override def moveTo(bound: Bound[E]): ZippedSegment[E, D, V] =
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
   * Segment is represented by 'left' and 'right' subsegments which must specify the upper bound.
   * 'frontBackward' and 'frontForward' are the same two subsegments which are ordered by their upper bound.
   */
  sealed trait ZippedSegmentWithNext[E, D <: Domain[E], V]  
    extends SegmentT.WithNext[E, D, V, ZippedSegmentBase[E, D, V]]
      with ZippedSegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    def frontBackward: Segment.WithNext[E, D, V]
    def frontForward: Segment[E, D, V]

    override def upperBound: Bound.Upper[E] = frontBackward.upperBound

    // Navigation --------------------------------------------------------------- //
    override def moveNext: ZippedSegmentWithPrev[E, D, V] =
      sequence.stepForwardNextGenZipper(
        sequence.supplyZipper[ZippedSegmentWithPrev[E, D, V]](
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
   * Segment is represented by 'left' and 'right' subsegments which must specify the upper bound.
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
  sealed trait ZippedSegmentWithPrev[E, D <: Domain[E], V] 
    extends SegmentT.WithPrev[E, D, V, ZippedSegmentBase[E, D, V]]
      with ZippedSegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    def backBackward: Segment[E, D, V] = back.backward
    // Cast is safe if precondition 1 is provided.
    def backForward: Segment.WithPrev[E, D, V] = back.forward.asInstanceOf[Segment.WithPrev[E, D, V]]

    override def lowerBound: Bound.Lower[E] = backForward.lowerBound

    // Navigation --------------------------------------------------------------- //
    override def movePrev: ZippedSegmentWithNext[E, D, V] =
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

    private case class Back(backward: Segment[E, D, V], forward: Segment[E, D, V])
  }

  /**
   * Initial segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments which must specify the upper bound.
   * 'frontBackward' and 'frontForward' are the same two subsegments which are ordered by their upper bound.
   */
  final case class ZippedInitialSegment[E, D <: Domain[E], V] (
    override val sequence: AbstractZippedSegmentSeq[E, D, V],
    override val frontBackward: Segment.WithNext[E, D, V], 
    override val frontForward: Segment[E, D, V]
  ) extends SegmentT.Initial[E, D, V, ZippedSegmentBase[E, D, V]]
    with ZippedSegmentWithNext[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def left: Segment[E, D, V] = frontBackward
    override def right: Segment[E, D, V] = frontForward

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: ZippedInitialSegment[E, D, V] = this

    // Protected section -------------------------------------------------------- //
    protected override def self: ZippedInitialSegment[E, D, V] = this
  }

  /**
   * Terminal segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments which must be last segments of original sequences.
   */
  final case class ZippedTerminalSegment[E, D <: Domain[E], V] (
    override val sequence: AbstractZippedSegmentSeq[E, D, V],
    override val left: Segment.Last[E, D, V], 
    override val right: Segment.Last[E, D, V]
  ) extends SegmentT.Terminal[E, D, V, ZippedSegmentBase[E, D, V]]
    with ZippedSegmentWithPrev[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def isRepresentedBy(left: Segment[E, D, V], right: Segment[E, D, V]): Boolean = 
      left.isLast && right.isLast

    // Navigation --------------------------------------------------------------- //
    override def moveToLast: ZippedTerminalSegment[E, D, V] = this

    // Protected section -------------------------------------------------------- //
    protected override def self: ZippedTerminalSegment[E, D, V] = this
  }
  
  /**
   * Inner segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments which must specify the upper bound.
   * 'frontBackward' and 'frontForward' are the same two subsegments which are ordered by their upper bound.
   */
  final case class ZippedInnerSegment[E, D <: Domain[E], V](
    override val sequence: AbstractZippedSegmentSeq[E, D, V],
    override val frontBackward: Segment.WithNext[E, D, V], 
    override val frontForward: Segment[E, D, V]
  ) extends SegmentT.Inner[E, D, V, ZippedSegmentBase[E, D, V]]
    with ZippedSegmentWithPrev[E, D, V]
    with ZippedSegmentWithNext[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def left: Segment[E, D, V] = frontBackward
    override def right: Segment[E, D, V] = frontForward

    // Protected section -------------------------------------------------------- //
    protected override def self: ZippedInnerSegment[E, D, V] = this
  }

  /**
   * Single segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments which must be last segments of original sequences.
   */
  final case class ZippedSingleSegment[E, D <: Domain[E], V](
    override val sequence: AbstractZippedSegmentSeq[E, D, V],
    override val left: Segment.Last[E, D, V], 
    override val right: Segment.Last[E, D, V]
  ) extends SegmentT.Single[E, D, V, ZippedSegmentBase[E, D, V]]
    with ZippedSegmentBase[E, D, V] {

    // Inspection --------------------------------------------------------------- //
    override def isRepresentedBy(left: Segment[E, D, V], right: Segment[E, D, V]): Boolean = 
      left.isLast && right.isLast

    // Navigation --------------------------------------------------------------- //
    override def moveToFirst: ZippedSingleSegment[E, D, V] = this

    override def moveToLast: ZippedSingleSegment[E, D, V] = this

    override def moveTo(bound: Bound[E]): ZippedSingleSegment[E, D, V] = this

    // Protected section -------------------------------------------------------- //
    protected override def self: ZippedSingleSegment[E, D, V] = this
  }
}