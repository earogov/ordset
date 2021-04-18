package ordset.core

import ordset.core.value.ValueOps
import ordset.core.domain.{Domain, DomainOps}

// TODO: class description.
abstract class AbstractZippedSegmentSeq[E, D <: Domain[E], W] extends AbstractSegmentSeq[E, D, W] { seq =>

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

  final override def contains(element: E): Boolean = super.contains(element)

  // Navigation --------------------------------------------------------------- //
  final override def upperBounds: Iterable[Bound.Upper[E]] = forwardUpperBoundsFromSegment(firstSegment)

  final override def firstSegment: ZippedSegmentBase[E, D, W] with FirstSegment = firstSegmentInstance

  final override def lastSegment: ZippedSegmentBase[E, D, W] with LastSegment =
    lastFrontZipper(left.lastSegment, right.lastSegment)

  final override def getSegment(bound: Bound[E]): ZippedSegmentBase[E, D, W] with GenSegment =
    searchFrontZipper(generalFrontZipper, left.getSegment(bound), right.getSegment(bound))

  final override def getSegment(element: E): ZippedSegmentBase[E, D, W] with GenSegment =
    getSegment(Bound.Upper.inclusive(element))

  // Transformation ----------------------------------------------------------- //
  final override def takenAbove(bound: Bound[E]): SegmentSeq[E, D, W] =
    cons(left.takenAbove(bound), right.takenAbove(bound))

  final override def takenBelow(bound: Bound[E]): SegmentSeq[E, D, W] =
    cons(left.takenBelow(bound), right.takenBelow(bound))

  final override def sliced(bound: Bound[E]): (SegmentSeq[E, D, W], SegmentSeq[E, D, W]) =
    (takenBelow(bound), takenAbove(bound))
  
  // TODO implement `appended` method.
  //  That's wrong: cons(left.appended(other), right.appended(other))
  final override def appended(other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W] = ???

  // TODO implement `appended` method.
  final override def appended(bound: Bound[E], other: SegmentSeq[E, D, W]): SegmentSeq[E, D, W] = ???

  // Protected section -------------------------------------------------------- //
  protected final type Zipper[S <: ZippedTuple[E, D, W]] = (GenSegment, GenSegment) => S
  protected final type NextGenZipper[S <: ZippedTuple[E, D, W]] = (SegmentWithNext, GenSegment) => S
  protected final type ComposedZipped[S <: ZippedTuple[E, D, W]] = (Zipper[S], GenSegment, GenSegment) => S

  /** Original sequence to which zipping is applied. */
  protected val left: SegmentSeq[E, D, W]

  /** Original sequence to which zipping is applied. */
  protected val right: SegmentSeq[E, D, W]

  /**
   * Function combining values of `left` and `right` sequences and returning value of zipped sequence.
   * Operator must be commutative:
   * {{{
   * operator(x, y) == operator(y, x).
   * }}}
   */
  protected def operator(left: W, right: W): W

  /**
   * Function that returns `true` for value `x` if and only if
   * {{{
   * Ǝ C ∀ y: operator(x, y) = C.
   * }}}
   * I.e. input `value` is invariant if operator value doesn't depend on second argument.
   */
  protected def invariant(value: W): Boolean

  /**
   * Creates zipped segment sequence.
   */
  protected def cons(left: SegmentSeq[E, D, W], right: SegmentSeq[E, D, W]): SegmentSeq[E, D, W]

  /**
   * @return value assigned to zipped segment defined by `left` and `right` subsegments.
   * For sets it returns 'belongs to set' indicator (`Boolean`).
   * For maps `E` -> `V` it returns `Option[V]` where `None` means that segment doesn't belong to set.
   */
  protected def getSegmentValue(left: Segment[E, D, W], right: Segment[E, D, W]): W =
    // TODO: check invariant for segment with minimal complexity (number of children).
    if (invariant(left.value)) left.value
    else operator(left.value, right.value)

  /**
   * Returns `true` if segment with given value is considered to be included in set.
   *
   * For example, if `W` = `Option[AnyType]`, then we assume `None` is not included and `Some(anyValue)` - is included.
   */
  protected def isIncludedInSet(value: W): Boolean

  /** First segment of sequence. It's either initial ot single. */
  protected final lazy val firstSegmentInstance: ZippedSegmentBase[E, D, W] with FirstSegment =
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
    left: GenSegment, 
    right: GenSegment
  ): ZippedSegmentBase[E, D, W] with GenSegment =
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
    left: GenSegment, 
    right: GenSegment
  ): ZippedSegmentBase[E, D, W] with FirstSegment =
    (left, right) match {
      case (ln: SegmentWithNext, rn: SegmentWithNext) =>
        if (domainOps.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0) 
          ZippedInitialSegment[E, D, W](this, rn, ln)
        else                                                               
          ZippedInitialSegment[E, D, W](this, ln, rn)
      case (ln: SegmentWithNext, _) => ZippedInitialSegment[E, D, W](this, ln, right)
      case (_, rn: SegmentWithNext) => ZippedInitialSegment[E, D, W](this, rn, left)
      case (ll: LastSegment, rl: LastSegment) => ZippedSingleSegment[E, D, W](this, ll, rl)
      case _ => throwSegmentIsEitherLastOrHasNext // just to remove warning
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
    left: LastSegment, 
    right: LastSegment
  ): ZippedSegmentBase[E, D, W] with LastSegment =
    // First zipped segment is single if it's represented by two last subsegments => cast is safe.
    if (firstSegmentInstance.isRepresentedBy(left, right)) 
      firstSegmentInstance.asInstanceOf[ZippedSingleSegment[E, D, W]]
    else 
      ZippedTerminalSegment[E, D, W](this, left, right)

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
    left: GenSegment, 
    right: GenSegment
  ): ZippedSegmentWithPrev[E, D, W] =
    (left, right) match {
      case (ln: SegmentWithNext, rn: SegmentWithNext) =>
        if (domainOps.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0) 
          ZippedInnerSegment[E, D, W](this, rn, ln)
        else                                                               
          ZippedInnerSegment[E, D, W](this, ln, rn)
      case (ln: SegmentWithNext, _) => ZippedInnerSegment[E, D, W](this, ln, right)
      case (_, rn: SegmentWithNext) => ZippedInnerSegment[E, D, W](this, rn, left)
      case (ll: LastSegment, rl: LastSegment) => ZippedTerminalSegment[E, D, W](this, ll, rl)
      case _ => throwSegmentIsEitherLastOrHasNext // just to remove warning
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
    left: SegmentWithNext, 
    right: GenSegment
  ): ZippedSegmentWithNext[E, D, W] =
    // First zipped segment is initial if one of its subsegments has next segment => cast is safe.
    if (firstSegmentInstance.isRepresentedBy(left, right)) 
      firstSegmentInstance.asInstanceOf[ZippedInitialSegment[E, D, W]]
    else right match {
      case rn: SegmentWithNext =>
        if (domainOps.boundOrd.compare(left.upperBound, rn.upperBound) >= 0) 
          ZippedInnerSegment[E, D, W](this, rn, left)
        else                                                                 
          ZippedInnerSegment[E, D, W](this, left, rn)
      case _ => ZippedInnerSegment[E, D, W](this, left, right)
    }
  
  protected final def supplyZipper[S <: ZippedTuple[E, D, W]](
    supplied: Zipper[S], 
    composed: ComposedZipped[S]
  ): Zipper[S] =
    (left: GenSegment, right: GenSegment) => composed(supplied, left, right)

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
  protected final def searchFrontZipper[S <: ZippedTuple[E, D, W]](
    zipper: Zipper[S], 
    left: GenSegment, 
    right: GenSegment
  ): S =
    if (left.isLast && right.isLast) zipper(left, right)
    else {
      var stop = false
      var nextZipped: ZippedTuple[E, D, W] = null
      var currZipped: ZippedTuple[E, D, W] = ZippedTupleImpl[E, D, W](this, left, right)
      while (!stop) {
        nextZipped = stepForwardZipper(ZippedTupleImpl.zipper(this), currZipped.left, currZipped.right)
        if (valueOps.neqv(currZipped.value, nextZipped.value)) {
          // We have found a bound, where operator change its value => return 'currZipped'.
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
  protected final def searchBackZipper[S <: ZippedTuple[E, D, W]](
    zipper: Zipper[S], 
    left: GenSegment, 
    right: GenSegment
  ): S =
    if (left.isFirst && right.isFirst) zipper(left, right)
    else {
      var stop = false
      var prevZipped: ZippedTuple[E, D, W] = null
      var currZipped: ZippedTuple[E, D, W] = ZippedTupleImpl[E, D, W](this, left, right)
      while (!stop) {
        prevZipped = stepBackwardZipper(ZippedTupleImpl.zipper(this), currZipped.left, currZipped.right)
        if (valueOps.neqv(currZipped.value, prevZipped.value)) {
          // We have found a bound, where operator change its value => return 'currZipped'.
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
  protected final def stepForwardZipper[S <: ZippedTuple[E, D, W]](
    zipper: Zipper[S], 
    left: GenSegment, 
    right: GenSegment
  ): S = 
    left match {
      case ln: SegmentWithNext => stepForwardNextGenZipper(zipper, ln, right)
      // left:  ?---------------X
      // right: ?----------?
      case ll: LastSegment => right match {
        // left:  ?---------------X
        // right: ?----------|
        case rn: SegmentWithNext => stepForwardNextLastZipper(zipper, rn, ll)
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
  protected final def stepBackwardZipper[S <: ZippedTuple[E, D, W]](
    zipper: Zipper[S], 
    left: GenSegment, 
    right: GenSegment
  ): S = 
    left match {
      case lp: SegmentWithPrev => stepBackwardPrevGenZipper(zipper, lp, right)
      // left:  X---------------?
      // right:      ?----------?
      case lf: FirstSegment => right match {
        // left:  X---------------?
        // right:      |----------?
        case rp: SegmentWithPrev => stepBackwardFirstPrevZipper(zipper, lf, rp)
        // left:  X---------------?
        // right: X---------------?
        // Unable to make step backward => return zipped segment for current left and right values.
        case _ => zipper(left, right)
      }
      case _ => throwSegmentIsEitherFirstOrHasPrev // just to remove warning
    }

  /** Same as [[stepForwardZipper]] function but with stricter input types. */
  protected final def stepForwardNextGenZipper[S <: ZippedTuple[E, D, W]](
    zipper: Zipper[S], 
    left: SegmentWithNext, 
    right: GenSegment
  ): S = 
    right match {
      case rn: SegmentWithNext => stepForwardNextNextZipper(zipper, left, rn)
      case rl: LastSegment => stepForwardNextLastZipper(zipper, left, rl)
      case _ => throwSegmentIsEitherLastOrHasNext // just to remove warning
    }

  /** Same as [[stepBackwardZipper]] function but with stricter input types. */
  protected final def stepBackwardPrevGenZipper[S <: ZippedTuple[E, D, W]](
    zipper: NextGenZipper[S], 
    left: SegmentWithPrev, 
    right: GenSegment
  ): S = 
    right match {
      case rp: SegmentWithPrev => stepBackwardPrevPrevZipper(zipper, rp, left)
      case rf: FirstSegment => stepBackwardFirstPrevZipper(zipper, rf, left)
      case _ => throwSegmentIsEitherFirstOrHasPrev // just to remove warning
    }

  /** Same as [[stepForwardZipper]] function but with stricter input types. */
  protected final def stepForwardNextLastZipper[S <: ZippedTuple[E, D, W]](
    zipper: Zipper[S], 
    backward: SegmentWithNext, 
    forward: LastSegment
  ): S =
    // backward: ?----------|
    // forward:  ?---------------X
    // If value of forward segment is invariant (i.e. value of backward segment doesn't affect on operator),
    // we can immediately define operator value up to upper bound of forward segment skipping segments of
    // backward sequence below it.
    if (invariant(forward.value)) zipper(backward.moveToLast, forward)
    else zipper(backward.moveNext, forward)

  /** Same as [[stepBackwardZipper]] function but with stricter input types. */
  protected final def stepBackwardFirstPrevZipper[S <: ZippedTuple[E, D, W]](
    zipper: NextGenZipper[S], 
    backward: FirstSegment, 
    forward: SegmentWithPrev
  ): S =
    // backward: X---------------?
    // forward:      |-----------?
    // If value of backward segment is invariant (i.e. value of forward segment doesn't affect on operator),
    // we can immediately define operator value up to lower bound of backward segment skipping segments of
    // forward sequence above it.
    // Input forward segment has previous segment => all previous segments of corresponding sequence has next segment =>
    // cast is safe.
    if (invariant(backward.value)) zipper(forward.moveToFirst.asInstanceOf[SegmentWithNext], backward)
    else zipper(forward.movePrev, backward)

  /** Same as [[stepForwardZipper]] function but with stricter input types. */
  protected final def stepForwardNextNextZipper[S <: ZippedTuple[E, D, W]](
    zipper: Zipper[S], 
    left: SegmentWithNext, 
    right: SegmentWithNext
  ): S = {
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
  protected final def stepBackwardPrevPrevZipper[S <: ZippedTuple[E, D, W]](
    zipper: NextGenZipper[S], 
    left: SegmentWithPrev, 
    right: SegmentWithPrev
  ): S = {
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
  protected final def stepForwardNextNextDiffZipper[S <: ZippedTuple[E, D, W]](
    zipper: Zipper[S], 
    backward: SegmentWithNext, 
    forward: SegmentWithNext
  ): S =
    // backward: ?----------|
    // forward:  ?---------------|
    // If value of forward segment is invariant (i.e. value of backward segment doesn't affect on operator),
    // we can immediately define operator value up to upper bound of forward segment skipping segments of
    // backward sequence below it.
    if (invariant(forward.value)) zipper(backward.moveTo(forward.upperBound), forward)
    else zipper(backward.moveNext, forward)

  /**
   * Same as [[stepBackwardPrevPrevZipper]] function with additional precondition:
   *  1. Lower bounds of `left` and `right` subsegments are not equal.
   */
  protected final def stepBackwardPrevPrevDiffZipper[S <: ZippedTuple[E, D, W]](
    zipper: NextGenZipper[S], 
    backward: SegmentWithPrev, 
    forward: SegmentWithPrev
  ): S =
    // backward: |---------------?
    // forward:      |-----------?
    // If value of backward segment is invariant (i.e. value of forward segment doesn't affect on operator),
    // we can immediately define operator value up to lower bound of backward segment skipping segments of
    // forward sequence above it.
    // Input forward segment has previous segment => all previous segments of corresponding sequence has next segment
    // => cast is safe.
    if (invariant(backward.value)) zipper(forward.moveTo(backward.lowerBound).asInstanceOf[SegmentWithNext], backward)
    else zipper(forward.movePrev, backward)

  protected final def throwSegmentIsEitherLastOrHasNext: Nothing = {
    throw new AssertionError("Unreachable case: segment is either last or has next segment.")
  }

  protected final def throwSegmentIsEitherFirstOrHasPrev: Nothing =
    throw new AssertionError("Unreachable case: segment is either first or has previous segment.")
}

object AbstractZippedSegmentSeq {
  
  /**
   * Pair of subsegments of original sequences.
   *
   * `left` and `right` are just labels for subsegments,
   * they don't have to correspond to `left` and `right` original sequences. `left` subsegment may belong to
   * `right` sequence and vice versa. The only requirement is subsegments must belong to different sequences.
   */
  sealed trait ZippedTuple[E, D <: Domain[E], W] {

    def sequence: AbstractZippedSegmentSeq[E, D, W]
    
    def left: Segment[E, D, W]
    def right: Segment[E, D, W]

    /** @return `true` if subsegments of tuple correspond to input `left` and `right`.
     *           Which is `left` and which is `right` doesn't matter.
     */
    def isRepresentedBy(left: Segment[E, D, W], right: Segment[E, D, W]): Boolean = {
      val ord = sequence.domainOps.segmentUpperOrd
      if      (ord.eqv(left, this.left))  ord.eqv(right, this.right)
      else if (ord.eqv(right, this.left)) ord.eqv(left, this.right)
      else                                false
    }

    lazy val value: W = sequence.getSegmentValue(left, right)
  }

  final case class ZippedTupleImpl[E, D <: Domain[E], W](
    override val sequence: AbstractZippedSegmentSeq[E, D, W],
    override val left: Segment[E, D, W], 
    override val right: Segment[E, D, W]
  ) extends ZippedTuple[E, D, W]
  
  object ZippedTupleImpl {
    
    def zipper[E, D <: Domain[E], W](
      sequence: AbstractZippedSegmentSeq[E, D, W]
    ): (Segment[E, D, W], Segment[E, D, W]) => ZippedTuple[E, D, W] =
      (left, right) => new ZippedTupleImpl[E, D, W](sequence, left, right)
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
  sealed trait ZippedSegmentBase[E, D <: Domain[E], W] 
    extends SegmentLike[E, D, W] 
      with ZippedTuple[E, D, W] {
    
    override def sequence: AbstractZippedSegmentSeq[E, D, W]
    
    override def isIncluded: Boolean = sequence.isIncludedInSet(value)

    override def moveToFirst: ZippedSegmentBase[E, D, W] with Segment.First[E, D, W] = sequence.firstSegment

    override def moveToLast: ZippedSegmentBase[E, D, W] with Segment.Last[E, D, W] = sequence.lastSegment

    override def moveTo(bound: Bound[E]): ZippedSegmentBase[E, D, W] with Segment[E, D, W] =
      sequence.searchFrontZipper(
        sequence.generalFrontZipper, 
        left.moveTo(bound), 
        right.moveTo(bound)
      )

    // Transformation ----------------------------------------------------------- //
    override def takenAbove: SegmentSeq[E, D, W] = ???

    override def takenBelow: SegmentSeq[E, D, W] = ???

    override def sliced: (SegmentSeq[E, D, W], SegmentSeq[E, D, W]) = ???
  }

  /**
   * Zipped segment with next segment.
   *
   * Segment is represented by 'left' and 'right' subsegments which must specify the upper bound.
   * 'frontBackward' and 'frontForward' are the same two subsegments which are ordered by their upper bound.
   */
  sealed trait ZippedSegmentWithNext[E, D <: Domain[E], W]  
    extends ZippedSegmentBase[E, D, W]
      with Segment.WithNext[E, D, W] {

    def frontBackward: Segment.WithNext[E, D, W]
    def frontForward: Segment[E, D, W]

    override def upperBound: Bound.Upper[E] = frontBackward.upperBound

    override def moveNext: ZippedSegmentWithPrev[E, D, W] =
      sequence.stepForwardNextGenZipper(
        sequence.supplyZipper[ZippedSegmentWithPrev[E, D, W]](
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
  sealed trait ZippedSegmentWithPrev[E, D <: Domain[E], W] 
    extends ZippedSegmentBase[E, D, W]
      with Segment.WithPrev[E, D, W] {

    def backBackward: Segment[E, D, W] = back.backward
    // Cast is safe if precondition 1 is provided.
    def backForward: Segment.WithPrev[E, D, W] = back.forward.asInstanceOf[Segment.WithPrev[E, D, W]]

    override def lowerBound: Bound.Lower[E] = backForward.lowerBound

    override def movePrev: ZippedSegmentWithNext[E, D, W] =
      sequence.stepBackwardPrevGenZipper(
        sequence.withNextFrontZipper, 
        backForward, 
        backBackward
      )

    private lazy val back: Back = {
      val backTuple = sequence.searchBackZipper(ZippedTupleImpl.zipper(sequence), left, right)
      if (domainOps.segmentLowerOrd.compare(backTuple.left, backTuple.right) >= 0)
        Back(backTuple.right, backTuple.left)
      else
        Back(backTuple.left, backTuple.right)
    }

    private case class Back(backward: Segment[E, D, W], forward: Segment[E, D, W])
  }

  /**
   * Initial segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments which must specify the upper bound.
   * 'frontBackward' and 'frontForward' are the same two subsegments which are ordered by their upper bound.
   */
  final case class ZippedInitialSegment[E, D <: Domain[E], W] (
    override val sequence: AbstractZippedSegmentSeq[E, D, W],
    override val frontBackward: Segment.WithNext[E, D, W], 
    override val frontForward: Segment[E, D, W]
  ) extends ZippedSegmentWithNext[E, D, W] 
    with Segment.Initial[E, D, W] {

    override def left: Segment[E, D, W] = frontBackward
    override def right: Segment[E, D, W] = frontForward

    override def moveToFirst: ZippedInitialSegment[E, D, W] = this
  }

  /**
   * Terminal segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments which must be last segments of original sequences.
   */
  final case class ZippedTerminalSegment[E, D <: Domain[E], W] (
    override val sequence: AbstractZippedSegmentSeq[E, D, W],
    override val left: Segment.Last[E, D, W], 
    override val right: Segment.Last[E, D, W]
  ) extends ZippedSegmentWithPrev[E, D, W] 
    with Segment.Terminal[E, D, W] {

    override def isRepresentedBy(left: Segment[E, D, W], right: Segment[E, D, W]): Boolean = 
      left.isLast && right.isLast

    override def moveToLast: ZippedTerminalSegment[E, D, W] = this
  }
  
  /**
   * Inner segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments which must specify the upper bound.
   * 'frontBackward' and 'frontForward' are the same two subsegments which are ordered by their upper bound.
   */
  final case class ZippedInnerSegment[E, D <: Domain[E], W](
    override val sequence: AbstractZippedSegmentSeq[E, D, W],
    override val frontBackward: Segment.WithNext[E, D, W], 
    override val frontForward: Segment[E, D, W]
  ) extends ZippedSegmentWithPrev[E, D, W] 
    with ZippedSegmentWithNext[E, D, W]
    with Segment.Inner[E, D, W] {

    override def left: Segment[E, D, W] = frontBackward
    override def right: Segment[E, D, W] = frontForward
  }

  /**
   * Single segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments which must be last segments of original sequences.
   */
  final case class ZippedSingleSegment[E, D <: Domain[E], W](
    override val sequence: AbstractZippedSegmentSeq[E, D, W],
    override val left: Segment.Last[E, D, W], 
    override val right: Segment.Last[E, D, W]
  ) extends ZippedSegmentBase[E, D, W]
    with Segment.Single[E, D, W] {

    override def isRepresentedBy(left: Segment[E, D, W], right: Segment[E, D, W]): Boolean = 
      left.isLast && right.isLast

    override def moveToFirst: ZippedSingleSegment[E, D, W] = this

    override def moveToLast: ZippedSingleSegment[E, D, W] = this

    override def moveTo(bound: Bound[E]): ZippedSingleSegment[E, D, W] = this
  }
}