package ordset

abstract class AbstractZippedSegmentSeq[E, D <: Domain[E], W] extends SegmentSeq[E, D, W] { seq =>

  protected final type GenSegment = Segment[E, D, W]
  protected final type FirstSegment = Segment.First[E, D, W]
  protected final type LastSegment = Segment.Last[E, D, W]
  protected final type InitialSegment = Segment.Initial[E, D, W]
  protected final type TerminalSegment = Segment.Terminal[E, D, W]
  protected final type InnerSegment = Segment.Inner[E, D, W]
  protected final type SingleSegment = Segment.Single[E, D, W]
  protected final type SegmentWithNext = Segment.WithNext[E, D, W]
  protected final type SegmentWithPrev = Segment.WithPrev[E, D, W]

  protected final type Zipper[S <: ZippedTuple] = (GenSegment, GenSegment) => S
  protected final type NextGenZipper[S <: ZippedTuple] = (SegmentWithNext, GenSegment) => S
  protected final type ComposedZipped[S <: ZippedTuple] = (Zipper[S], GenSegment, GenSegment) => S

  val valueEq: Eq[W]

  final override def firstSegment: Segment.First[E, D, W] = _firstSegment

  final override def lastSegment: Segment.Last[E, D, W] = lastFrontZipper(left.lastSegment, right.lastSegment)

  /** @return segment containing `bound`. */
  final override def getSegment(bound: Bound[E]): Segment[E, D, W] =
    searchFrontZipper(generalFrontZipper, left.getSegment(bound), right.getSegment(bound))

  /** @return segment containing `element`. */
  final override def getSegment(element: E): Segment[E, D, W] =
    searchFrontZipper(generalFrontZipper, left.getSegment(element), right.getSegment(element))

  protected val left: SegmentSeq[E, D, W]

  protected val right: SegmentSeq[E, D, W]

  protected def operator(left: W, right: W): W

  protected def invariant(value: W): Boolean

  protected def getSegmentValue(left: Segment[E, D, W], right: Segment[E, D, W]): W =
    // TODO: check invariant for segment with minimal complexity (number of children).
    if (invariant(left.value)) left.value
    else operator(left.value, right.value)

  protected final lazy val _firstSegment: ZippedSegmentBase with FirstSegment =
    searchFrontZipper(firstFrontZipper, left.firstSegment, right.firstSegment)

  protected final def generalFrontZipper(left: GenSegment, right: GenSegment): ZippedSegmentBase with GenSegment =
    if (_firstSegment.isRepresentedBy(left, right)) _firstSegment
    else withPrevFrontZipper(left, right)

  protected final def firstFrontZipper(left: GenSegment, right: GenSegment): ZippedSegmentBase with FirstSegment =
    (left, right) match {
      case (ln: SegmentWithNext, rn: SegmentWithNext) =>
        if (domain.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0) ZippedInitialSegment(rn, ln)
        else                                                            ZippedInitialSegment(ln, rn)
      case (ln: SegmentWithNext, _) => ZippedInitialSegment(ln, right)
      case (_, rn: SegmentWithNext) => ZippedInitialSegment(rn, left)
      case (ll: LastSegment, rl: LastSegment) => ZippedSingleSegment(ll, rl)
      case _ => throwSegmentIsEitherLastOrHasNext // just to remove warning
    }

  protected final def lastFrontZipper(left: LastSegment, right: LastSegment): ZippedSegmentBase with LastSegment =
    // First zipped segment is single if it's represented by two last subsegments => cast is safe.
    if (_firstSegment.isRepresentedBy(left, right)) _firstSegment.asInstanceOf[ZippedSingleSegment]
    else ZippedTerminalSegment(left, right)

  protected final def withPrevFrontZipper(left: GenSegment, right: GenSegment): ZippedSegmentWithPrev =
    (left, right) match {
      case (ln: SegmentWithNext, rn: SegmentWithNext) =>
        if (domain.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0) ZippedInnerSegment(rn, ln)
        else                                                            ZippedInnerSegment(ln, rn)
      case (ln: SegmentWithNext, _) => ZippedInnerSegment(ln, right)
      case (_, rn: SegmentWithNext) => ZippedInnerSegment(rn, left)
      case (ll: LastSegment, rl: LastSegment) => ZippedTerminalSegment(ll, rl)
      case _ => throwSegmentIsEitherLastOrHasNext // just to remove warning
    }

  protected final def withNextFrontZipper(left: SegmentWithNext, right: GenSegment): ZippedSegmentWithNext =
    // First zipped segment is initial if one of its subsegments has next segment => cast is safe.
    if (_firstSegment.isRepresentedBy(left, right)) _firstSegment.asInstanceOf[ZippedInitialSegment]
    else right match {
      case rn: SegmentWithNext =>
        if (domain.boundOrd.compare(left.upperBound, rn.upperBound) >= 0) ZippedInnerSegment(rn, left)
        else                                                              ZippedInnerSegment(left, rn)
      case _ => ZippedInnerSegment(left, right)
    }

  protected final def supplyZipper[S <: ZippedTuple](supplied: Zipper[S], composed: ComposedZipped[S]): Zipper[S] =
    (left: GenSegment, right: GenSegment) => composed(supplied, left, right)

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
          // We'v found bound, where operator change its value => return 'currZipped'.
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
          // We'v found bound, where operator change its value => return 'currZipped'.
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

  protected final def stepForwardNextGenZipper[S <: ZippedTuple](
    zipper: Zipper[S], left: SegmentWithNext, right: GenSegment
  ): S = right match {
    case rn: SegmentWithNext => stepForwardNextNextZipper(zipper, left, rn)
    case rl: LastSegment => stepForwardNextLastZipper(zipper, left, rl)
    case _ => throwSegmentIsEitherLastOrHasNext // just to remove warning
  }

  protected final def stepBackwardPrevGenZipper[S <: ZippedTuple](
    zipper: NextGenZipper[S], left: SegmentWithPrev, right: GenSegment
  ): S = right match {
    case rp: SegmentWithPrev => stepBackwardPrevPrevZipper(zipper, rp, left)
    case rf: FirstSegment => stepBackwardFirstPrevZipper(zipper, rf, left)
    case _ => throwSegmentIsEitherFirstOrHasPrev // just to remove warning
  }

  protected final def stepForwardNextLastZipper[S <: ZippedTuple](
    zipper: Zipper[S], backward: SegmentWithNext, forward: LastSegment
  ): S =
    // backward: ?----------|
    // forward:  ?---------------X
    // If value of forward segment is invariant (i.e. value of backward segment doesn't affect on operator),
    // we can immediately define operator's value up to forward segment upper bound skipping segments of
    // backward sequence below it.
    if (invariant(forward.value)) zipper(backward.moveToLast, forward)
    else zipper(backward.moveNext, forward)

  protected final def stepBackwardFirstPrevZipper[S <: ZippedTuple](
    zipper: NextGenZipper[S], backward: FirstSegment, forward: SegmentWithPrev
  ): S =
    // backward: X---------------?
    // forward:      |-----------?
    // If value of backward segment is invariant (i.e. value of forward segment doesn't affect on operator),
    // we can immediately define operator's value up to backward segment lower bound skipping segments of
    // forward sequence above it.
    // Input forward segment has previous segment => all previous segments of corresponding sequence has next segment =>
    // cast is safe.
    if (invariant(backward.value)) zipper(forward.moveToFirst.asInstanceOf[SegmentWithNext], backward)
    else zipper(forward.movePrev, backward)

  protected final def stepForwardNextNextZipper[S <: ZippedTuple](
    zipper: Zipper[S], left: SegmentWithNext, right: SegmentWithNext
  ): S = {
    val cmp = domain.boundOrd.compare(left.upperBound, right.upperBound)
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

  protected final def stepBackwardPrevPrevZipper[S <: ZippedTuple](
    zipper: NextGenZipper[S], left: SegmentWithPrev, right: SegmentWithPrev
  ): S = {
    val cmp = domain.boundOrd.compare(left.lowerBound, right.lowerBound)
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

  protected final def stepForwardNextNextDiffZipper[S <: ZippedTuple](
    zipper: Zipper[S], backward: SegmentWithNext, forward: SegmentWithNext
  ): S =
    // backward: ?----------|
    // forward:  ?---------------|
    // If value of forward segment is invariant (i.e. value of backward segment doesn't affect on operator),
    // we can immediately define operator's value up to forward segment upper bound skipping segments of
    // backward sequence below it.
    if (invariant(forward.value)) zipper(backward.moveTo(forward.upperBound), forward)
    else zipper(backward.moveNext, forward)

  protected final def stepBackwardPrevPrevDiffZipper[S <: ZippedTuple](
    zipper: NextGenZipper[S], backward: SegmentWithPrev, forward: SegmentWithPrev
  ): S =
    // backward: |---------------?
    // forward:      |-----------?
    // If value of backward segment is invariant (i.e. value of forward segment doesn't affect on operator),
    // we can immediately define operator's value up to backward segment lower bound skipping segments of
    // forward sequence above it.
    // Input forward segment has previous segment => all previous segments of corresponding sequence has next segment =>
    // cast is safe.
    if (invariant(backward.value)) zipper(forward.moveTo(backward.lowerBound).asInstanceOf[SegmentWithNext], backward)
    else zipper(forward.movePrev, backward)

  protected final def throwSegmentIsEitherLastOrHasNext: Nothing =
    throw new IllegalArgumentException("Unreachable case: segment is either last or has next segment.")

  protected final def throwSegmentIsEitherFirstOrHasPrev: Nothing =
    throw new IllegalArgumentException("Unreachable case: segment is either first or has previous segment.")

  protected trait ZippedTuple {

    def left: GenSegment
    def right: GenSegment

    def isRepresentedBy(left: GenSegment, right: GenSegment): Boolean =
      if      (domain.segmentUpperOrd.eqv(left, this.left))  domain.segmentUpperOrd.eqv(right, this.right)
      else if (domain.segmentUpperOrd.eqv(right, this.left)) domain.segmentUpperOrd.eqv(left, this.right)
      else                                                   false

    lazy val value: W = getSegmentValue(left, right)
  }

  protected case class ZippedTupleImpl(
    override val left: GenSegment, override val right: GenSegment
  ) extends ZippedTuple

  /**
   * Base trait for zipped segments.
   * Upper and lower bounds of zipped segments are defined by change of the operator's value.
   * Zipped segments are represented by 'left' and 'right' subsegments which must specify the upper bound.
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
   */
  protected sealed trait ZippedSegmentBase extends SegmentLike[E, D, W] with ZippedTuple {

    override def domain: D = seq.domain

    override def moveToFirst: FirstSegment = firstSegment

    override def moveToLast: LastSegment = lastSegment

    override def moveTo(bound: Bound[E]): GenSegment =
      searchFrontZipper(generalFrontZipper, left.moveTo(bound), right.moveTo(bound))
  }

  /**
   * Zipped segment with next segment.
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
   * Segment is represented by 'left' and 'right' subsegments which must specify the upper bound.
   * Lower bound is defined by 'backBackward' and 'backForward' subsegments and is searched lazily.
   * 'backBackward' and 'backForward' subsegments are ordered by their lower bound.
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
   *
   * @note preconditions: 1. 'backForward' subsegment must have previous segment.
   *                          This condition is equivalent to: zipped segment has previous segment.
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
      if (domain.segmentLowerOrd.compare(backTuple.left, backTuple.right) >= 0)
        Back(backTuple.right, backTuple.left)
      else
        Back(backTuple.left, backTuple.right)
    }

    private case class Back(backward: GenSegment, forward: GenSegment)
  }

  protected sealed case class ZippedInitialSegment (
    override val frontBackward: SegmentWithNext, override val frontForward: GenSegment
  ) extends ZippedSegmentWithNext with InitialSegment {

    override def left: GenSegment = frontBackward
    override def right: GenSegment = frontForward
  }

  protected sealed case class ZippedInnerSegment(
    override val frontBackward: SegmentWithNext, override val frontForward: GenSegment
  ) extends ZippedSegmentWithPrev with ZippedSegmentWithNext with InnerSegment {

    override def left: GenSegment = frontBackward
    override def right: GenSegment = frontForward
  }

  protected sealed case class ZippedTerminalSegment (
    override val left: LastSegment, override val right: LastSegment
  ) extends ZippedSegmentWithPrev with TerminalSegment {

    override def isRepresentedBy(left: GenSegment, right: GenSegment): Boolean = left.isLast && right.isLast
  }

  protected sealed case class ZippedSingleSegment(
    override val left: LastSegment, override val right: LastSegment
  ) extends ZippedSegmentBase with SingleSegment {

    override def isRepresentedBy(left: GenSegment, right: GenSegment): Boolean = left.isLast && right.isLast
  }
}