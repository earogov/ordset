package ordset

import ordset.domain.{Domain, DomainOps}

// TODO: class description.
abstract class AbstractZippedSegmentSeq[E, D <: Domain[E], W] extends AbstractSegmentSeq[E, D, W] { seq =>
  import domainOps._

  protected final type Zipper[S <: ZippedTuple] = (GenSegment, GenSegment) => S
  protected final type NextGenZipper[S <: ZippedTuple] = (SegmentWithNext, GenSegment) => S
  protected final type ComposedZipped[S <: ZippedTuple] = (Zipper[S], GenSegment, GenSegment) => S

  /** Equality typeclass for segments values. */
  def valueEq: Eq[W]

  /** @return true if sequence is empty i.e. contains no elements. */
  override def isEmpty: Boolean =
    firstSegmentInstance.isSingle && !belongsToSet(firstSegmentInstance.value)

  /** @return true if sequence is universal i.e. contains all elements of domain. */
  override def isUniversal: Boolean =
    firstSegmentInstance.isSingle && belongsToSet(firstSegmentInstance.value)

  /** @return true if sequence contains `bound`. */
  override def contains(bound: Bound[E]): Boolean =
    belongsToSet(getSegmentValue(left.getSegment(bound), right.getSegment(bound)))

  /** @return true if sequence contains `element`. */
  override def contains(element: E): Boolean =
    super.contains(element)

  /** @return first segment of sequence. */
  final override def firstSegment: Segment.First[E, D, W] =
    firstSegmentInstance

  /** @return last segment of sequence. */
  final override def lastSegment: Segment.Last[E, D, W] =
    lastFrontZipper(left.lastSegment, right.lastSegment)

  /** @return segment which contains specified `bound`. */
  final override def getSegment(bound: Bound[E]): Segment[E, D, W] =
    searchFrontZipper(generalFrontZipper, left.getSegment(bound), right.getSegment(bound))

  /** @return segment which contains specified `element`. */
  final override def getSegment(element: E): Segment[E, D, W] =
    super.getSegment(element)

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
   * @return value assigned to zipped segment defined by `left` and `right` subsegments.
   * For sets it returns 'belongs to set' indicator (`Boolean`).
   * For maps `E` -> `V` it returns `Option[V]` where `None` means that segment doesn't belong to set.
   */
  protected def getSegmentValue(left: Segment[E, D, W], right: Segment[E, D, W]): W =
    // TODO: check invariant for segment with minimal complexity (number of children).
    if (invariant(left.value)) left.value
    else operator(left.value, right.value)

  /** @return true if segment `value` belongs to set. */
  @inline
  protected def belongsToSet(value: W): Boolean

  /** First segment of sequence. It's either initial ot single. */
  protected final lazy val firstSegmentInstance: ZippedSegmentBase with FirstSegment =
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
  protected final def generalFrontZipper(left: GenSegment, right: GenSegment): ZippedSegmentBase with GenSegment =
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
  protected final def firstFrontZipper(left: GenSegment, right: GenSegment): ZippedSegmentBase with FirstSegment =
    (left, right) match {
      case (ln: SegmentWithNext, rn: SegmentWithNext) =>
        if (boundOrd.compare(ln.upperBound, rn.upperBound) >= 0) ZippedInitialSegment(rn, ln)
        else                                                     ZippedInitialSegment(ln, rn)
      case (ln: SegmentWithNext, _) => ZippedInitialSegment(ln, right)
      case (_, rn: SegmentWithNext) => ZippedInitialSegment(rn, left)
      case (ll: LastSegment, rl: LastSegment) => ZippedSingleSegment(ll, rl)
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
  protected final def lastFrontZipper(left: LastSegment, right: LastSegment): ZippedSegmentBase with LastSegment =
    // First zipped segment is single if it's represented by two last subsegments => cast is safe.
    if (firstSegmentInstance.isRepresentedBy(left, right)) firstSegmentInstance.asInstanceOf[ZippedSingleSegment]
    else ZippedTerminalSegment(left, right)

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
  protected final def withPrevFrontZipper(left: GenSegment, right: GenSegment): ZippedSegmentWithPrev =
    (left, right) match {
      case (ln: SegmentWithNext, rn: SegmentWithNext) =>
        if (boundOrd.compare(ln.upperBound, rn.upperBound) >= 0) ZippedInnerSegment(rn, ln)
        else                                                     ZippedInnerSegment(ln, rn)
      case (ln: SegmentWithNext, _) => ZippedInnerSegment(ln, right)
      case (_, rn: SegmentWithNext) => ZippedInnerSegment(rn, left)
      case (ll: LastSegment, rl: LastSegment) => ZippedTerminalSegment(ll, rl)
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
  protected final def withNextFrontZipper(left: SegmentWithNext, right: GenSegment): ZippedSegmentWithNext =
    // First zipped segment is initial if one of its subsegments has next segment => cast is safe.
    if (firstSegmentInstance.isRepresentedBy(left, right)) firstSegmentInstance.asInstanceOf[ZippedInitialSegment]
    else right match {
      case rn: SegmentWithNext =>
        if (boundOrd.compare(left.upperBound, rn.upperBound) >= 0) ZippedInnerSegment(rn, left)
        else                                                       ZippedInnerSegment(left, rn)
      case _ => ZippedInnerSegment(left, right)
    }
  
  protected final def supplyZipper[S <: ZippedTuple](supplied: Zipper[S], composed: ComposedZipped[S]): Zipper[S] =
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
  protected final def searchFrontZipper[S <: ZippedTuple](
    zipper: Zipper[S], left: GenSegment, right: GenSegment
  ): S =
    if (left.isLast && right.isLast) zipper(left, right)
    else {
      var stop = false
      var nextZipped: ZippedTuple = null
      var currZipped: ZippedTuple = ZippedTupleImpl(left, right)
      while (!stop) {
        nextZipped = stepForwardZipper(ZippedTupleImpl, currZipped.left, currZipped.right)
        if (valueEq.neqv(currZipped.value, nextZipped.value)) {
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
  protected final def searchBackZipper[S <: ZippedTuple](
    zipper: Zipper[S], left: GenSegment, right: GenSegment
  ): S =
    if (left.isFirst && right.isFirst) zipper(left, right)
    else {
      var stop = false
      var prevZipped: ZippedTuple = null
      var currZipped: ZippedTuple = ZippedTupleImpl(left, right)
      while (!stop) {
        prevZipped = stepBackwardZipper(ZippedTupleImpl, currZipped.left, currZipped.right)
        if (valueEq.neqv(currZipped.value, prevZipped.value)) {
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
  protected final def stepForwardZipper[S <: ZippedTuple](
    zipper: Zipper[S], left: GenSegment, right: GenSegment
  ): S = left match {
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
  protected final def stepBackwardZipper[S <: ZippedTuple](
    zipper: Zipper[S], left: GenSegment, right: GenSegment
  ): S = left match {
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
  protected final def stepForwardNextGenZipper[S <: ZippedTuple](
    zipper: Zipper[S], left: SegmentWithNext, right: GenSegment
  ): S = right match {
    case rn: SegmentWithNext => stepForwardNextNextZipper(zipper, left, rn)
    case rl: LastSegment => stepForwardNextLastZipper(zipper, left, rl)
    case _ => throwSegmentIsEitherLastOrHasNext // just to remove warning
  }

  /** Same as [[stepBackwardZipper]] function but with stricter input types. */
  protected final def stepBackwardPrevGenZipper[S <: ZippedTuple](
    zipper: NextGenZipper[S], left: SegmentWithPrev, right: GenSegment
  ): S = right match {
    case rp: SegmentWithPrev => stepBackwardPrevPrevZipper(zipper, rp, left)
    case rf: FirstSegment => stepBackwardFirstPrevZipper(zipper, rf, left)
    case _ => throwSegmentIsEitherFirstOrHasPrev // just to remove warning
  }

  /** Same as [[stepForwardZipper]] function but with stricter input types. */
  protected final def stepForwardNextLastZipper[S <: ZippedTuple](
    zipper: Zipper[S], backward: SegmentWithNext, forward: LastSegment
  ): S =
    // backward: ?----------|
    // forward:  ?---------------X
    // If value of forward segment is invariant (i.e. value of backward segment doesn't affect on operator),
    // we can immediately define operator value up to upper bound of forward segment skipping segments of
    // backward sequence below it.
    if (invariant(forward.value)) zipper(backward.moveToLast, forward)
    else zipper(backward.moveNext, forward)

  /** Same as [[stepBackwardZipper]] function but with stricter input types. */
  protected final def stepBackwardFirstPrevZipper[S <: ZippedTuple](
    zipper: NextGenZipper[S], backward: FirstSegment, forward: SegmentWithPrev
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
  protected final def stepForwardNextNextZipper[S <: ZippedTuple](
    zipper: Zipper[S], left: SegmentWithNext, right: SegmentWithNext
  ): S = {
    val cmp = boundOrd.compare(left.upperBound, right.upperBound)
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
  protected final def stepBackwardPrevPrevZipper[S <: ZippedTuple](
    zipper: NextGenZipper[S], left: SegmentWithPrev, right: SegmentWithPrev
  ): S = {
    val cmp = boundOrd.compare(left.lowerBound, right.lowerBound)
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
  protected final def stepForwardNextNextDiffZipper[S <: ZippedTuple](
    zipper: Zipper[S], backward: SegmentWithNext, forward: SegmentWithNext
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
  protected final def stepBackwardPrevPrevDiffZipper[S <: ZippedTuple](
    zipper: NextGenZipper[S], backward: SegmentWithPrev, forward: SegmentWithPrev
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

  protected final def throwSegmentIsEitherLastOrHasNext: Nothing =
    throw new IllegalArgumentException("Unreachable case: segment is either last or has next segment.")

  protected final def throwSegmentIsEitherFirstOrHasPrev: Nothing =
    throw new IllegalArgumentException("Unreachable case: segment is either first or has previous segment.")

  /**
   * Pair of subsegments of original sequences.
   *
   * `left` and `right` are just labels for subsegments,
   * they don't have to correspond to `left` and `right` original sequences. `left` subsegment may belong to
   * `right` sequence and vice versa. The only requirement is subsegments must belong to different sequences.
   */
  protected trait ZippedTuple {

    def left: GenSegment
    def right: GenSegment

    /** @return `true` if subsegments of tuple correspond to input `left` and `right`.
     *           Which is `left` and which is `right` doesn't matter.
     */
    def isRepresentedBy(left: GenSegment, right: GenSegment): Boolean =
      if      (segmentUpperOrd.eqv(left, this.left))  segmentUpperOrd.eqv(right, this.right)
      else if (segmentUpperOrd.eqv(right, this.left)) segmentUpperOrd.eqv(left, this.right)
      else                                                   false

    lazy val value: W = getSegmentValue(left, right)
  }

  protected case class ZippedTupleImpl(
    override val left: GenSegment, override val right: GenSegment
  ) extends ZippedTuple

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
   *                    |                 right     |
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
  protected sealed trait ZippedSegmentBase extends SegmentLike[E, D, W] with ZippedTuple {

    override def domainOps: DomainOps[E, D] = seq.domainOps

    override def moveToFirst: FirstSegment = firstSegment

    override def moveToLast: LastSegment = lastSegment

    override def moveTo(bound: Bound[E]): GenSegment =
      searchFrontZipper(generalFrontZipper, left.moveTo(bound), right.moveTo(bound))
  }

  /**
   * Zipped segment with next segment.
   *
   * Segment is represented by 'left' and 'right' subsegments which must specify the upper bound.
   * 'frontBackward' and 'frontForward' are the same two subsegments which are ordered by their upper bound.
   */
  protected sealed trait ZippedSegmentWithNext extends ZippedSegmentBase with SegmentWithNext {

    def frontBackward: SegmentWithNext
    def frontForward: GenSegment

    override def upperBound: Bound.Upper[E] = frontBackward.upperBound

    override def moveNext: SegmentWithPrev =
      stepForwardNextGenZipper(
        supplyZipper[ZippedSegmentWithPrev](withPrevFrontZipper, searchFrontZipper), frontBackward, frontForward)
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
  protected sealed trait ZippedSegmentWithPrev extends ZippedSegmentBase with SegmentWithPrev {

    def backBackward: GenSegment = back.backward
    // Cast is safe if precondition 1 is provided.
    def backForward: SegmentWithPrev = back.forward.asInstanceOf[SegmentWithPrev]

    override def lowerBound: Bound.Lower[E] = backForward.lowerBound

    override def movePrev: SegmentWithNext =
      stepBackwardPrevGenZipper(withNextFrontZipper, backForward, backBackward)

    private lazy val back: Back = {
      val backTuple = searchBackZipper(ZippedTupleImpl, left, right)
      if (segmentLowerOrd.compare(backTuple.left, backTuple.right) >= 0)
        Back(backTuple.right, backTuple.left)
      else
        Back(backTuple.left, backTuple.right)
    }

    private case class Back(backward: GenSegment, forward: GenSegment)
  }

  /**
   * Initial segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments which must specify the upper bound.
   * 'frontBackward' and 'frontForward' are the same two subsegments which are ordered by their upper bound.
   */
  protected sealed case class ZippedInitialSegment (
    override val frontBackward: SegmentWithNext, override val frontForward: GenSegment
  ) extends ZippedSegmentWithNext with InitialSegment {

    override def left: GenSegment = frontBackward
    override def right: GenSegment = frontForward
  }

  /**
   * Inner segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments which must specify the upper bound.
   * 'frontBackward' and 'frontForward' are the same two subsegments which are ordered by their upper bound.
   */
  protected sealed case class ZippedInnerSegment(
    override val frontBackward: SegmentWithNext, override val frontForward: GenSegment
  ) extends ZippedSegmentWithPrev with ZippedSegmentWithNext with InnerSegment {

    override def left: GenSegment = frontBackward
    override def right: GenSegment = frontForward
  }

  /**
   * Terminal segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments which must be last segments of original sequences.
   */
  protected sealed case class ZippedTerminalSegment (
    override val left: LastSegment, override val right: LastSegment
  ) extends ZippedSegmentWithPrev with TerminalSegment {

    override def isRepresentedBy(left: GenSegment, right: GenSegment): Boolean = left.isLast && right.isLast
  }

  /**
   * Single segment of zipped sequence.
   *
   * Segment is represented by 'left' and 'right' subsegments which must be last segments of original sequences.
   */
  protected sealed case class ZippedSingleSegment(
    override val left: LastSegment, override val right: LastSegment
  ) extends ZippedSegmentBase with SingleSegment {

    override def isRepresentedBy(left: GenSegment, right: GenSegment): Boolean = left.isLast && right.isLast
  }
}