package ordset

abstract class AbstractZippedSegmentSeq[E, D <: Domain[E], W] extends SegmentSeq[E, D, W] { seq =>

  val valueEq: Eq[W]

  final override def firstSegment: Segment.First[E, D, W] = _firstSegment

  final override def lastSegment: Segment.Last[E, D, W] = lastZipper(left.lastSegment, right.lastSegment)

  /** @return segment containing `bound`. */
  final override def getSegment(bound: Bound[E]): Segment[E, D, W] =
    searchFrontZipper(generalZipper, left.getSegment(bound), right.getSegment(bound))

  /** @return segment containing `element`. */
  final override def getSegment(element: E): Segment[E, D, W] =
    searchFrontZipper(generalZipper, left.getSegment(element), right.getSegment(element))

  protected final type GenSegment = Segment[E, D, W]
  protected final type FirstSegment = Segment.First[E, D, W]
  protected final type LastSegment = Segment.Last[E, D, W]
  protected final type InitialSegment = Segment.Initial[E, D, W]
  protected final type TerminalSegment = Segment.Terminal[E, D, W]
  protected final type InnerSegment = Segment.Inner[E, D, W]
  protected final type SingleSegment = Segment.Single[E, D, W]
  protected final type SegmentWithNext = Segment.WithNext[E, D, W]
  protected final type SegmentWithPrev = Segment.WithPrev[E, D, W]

  protected final type Zipper[S <: ZippedSegment] = (GenSegment, GenSegment) => S

  protected val left: SegmentSeq[E, D, W]

  protected val right: SegmentSeq[E, D, W]

  protected def operator(left: W, right: W): W

  protected def invariant(value: W): Boolean

  protected def getSegmentValue(left: Segment[E, D, W], right: Segment[E, D, W]): W =
    // TODO: check invariant for segment with minimal complexity (number of children).
    if (invariant(left.value)) left.value
    else operator(left.value, right.value)

  protected final lazy val _firstSegment: ZippedSegment with FirstSegment =
    searchFrontZipper(firstZipper, left.firstSegment, right.firstSegment)

  protected final def generalZipper(left: GenSegment, right: GenSegment): ZippedSegment with GenSegment =
    if (_firstSegment.hasFront(left, right)) _firstSegment
    else withPrevZipper(left, right)

  protected final def firstZipper(left: GenSegment, right: GenSegment): ZippedSegment with FirstSegment =
    (left, right) match {
      case (ll: LastSegment, rl: LastSegment) => ZippedSingleSegment(ll, rl)
      case (ln: SegmentWithNext, rn: SegmentWithNext) =>
        if (domain.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0) ZippedInitialSegment(rn, ln)
        else                                                            ZippedInitialSegment(ln, rn)
      case (ln: SegmentWithNext, _) => ZippedInitialSegment(ln, right)
      case (_, rn: SegmentWithNext) => ZippedInitialSegment(rn, left)
      case _ => throw new IllegalStateException("Unreachable case: segment is either last or has next segment.")
    }

  protected final def lastZipper(left: LastSegment, right: LastSegment): ZippedSegment with LastSegment =
    if (_firstSegment.hasFront(left, right)) _firstSegment.asInstanceOf[ZippedSingleSegment]
    else ZippedTerminalSegment(left, right)

  protected final def withPrevZipper(left: GenSegment, right: GenSegment): ZippedSegmentWithPrev =
    (left, right) match {
      case (ll: LastSegment, rl: LastSegment) => ZippedTerminalSegment(ll, rl)
      case (ln: SegmentWithNext, rn: SegmentWithNext) =>
        if (domain.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0) ZippedInnerSegment(rn, ln)
        else                                                            ZippedInnerSegment(ln, rn)
      case (ln: SegmentWithNext, _) => ZippedInnerSegment(ln, right)
      case (_, rn: SegmentWithNext) => ZippedInnerSegment(rn, left)
      case _ => throw new IllegalStateException("Unreachable case: segment is either last or has next segment.")
    }

  protected final def searchFrontZipper[S <: ZippedSegment](
    zipper: Zipper[S], left: GenSegment, right: GenSegment
  ): S = {
    var l = left
    var r = right
    var zipped = zipper(l, r)
    var stop = searchFrontStopCondition(zipped, l, r)
    while (!stop) {
      l = zipped.frontLeft
      r = zipped.frontRight
      zipped = zipper(l, r)
      stop = searchFrontStopCondition(zipped, l, r)
    }
    zipped
  }

  protected final def searchFrontStopCondition(
    zipped: ZippedSegment, prevLeft: GenSegment, prevRight: GenSegment
  ): Boolean =
    (prevLeft.isLast && prevRight.isLast) ||
     valueEq.eqv(
       operator(prevLeft.value, prevRight.value),
       operator(zipped.frontLeft.value, zipped.frontRight.value))

  protected final def stepForwardZipper[S <: ZippedSegment](
    zipper: Zipper[S], left: GenSegment, right: GenSegment
  ): S = left match {
      // X - domain bounds;
      // | - segment bounds;
      // ? - unknown bounds (domain or segment).
      case ln: SegmentWithNext => stepForwardNextGenZipper(zipper, ln, right)
      // left:  ?---------------X
      // right: ?----------?
      case _ => right match {
        // left:  ?---------------X
        // right: ?----------|
        case rn: SegmentWithNext =>
          // If value of forward segment is invariant (value of backward segment doesn't affect on operator's value),
          // we can immediately define operator's value up to `forward.upperBound` skipping segments of backward
          // sequence.
          if (invariant(left.value)) zipper(rn.moveToLast, left)
          else zipper(rn.moveNext, left)
        // left:  ?---------------X
        // right: ?---------------X
        case _ => zipper(left, right)
      }
    }

  protected final def stepForwardNextGenZipper[S <: ZippedSegment](
    zipper: Zipper[S], left: SegmentWithNext, right: GenSegment
  ): S = right match {
      // X - domain bounds;
      // | - segment bounds;
      // ? - unknown bounds (domain or segment).
      case rn: SegmentWithNext => stepForwardNextNextZipper(zipper, left, rn)
      // left  : ?----------|
      // right:  ?---------------X
      case _ =>
        // If value of forward segment is invariant (value of backward segment doesn't affect on operator's value),
        // we can immediately define operator's value up to `forward.upperBound` skipping segments of backward
        // sequence.
        if (invariant(right.value)) zipper(left.moveToLast, right)
        else zipper(left.moveNext, right)
    }

  protected final def stepForwardNextNextZipper[S <: ZippedSegment](
    zipper: Zipper[S], left: SegmentWithNext, right: SegmentWithNext
  ): S = {
      // X - domain bounds;
      // | - segment bounds;
      // ? - unknown bounds (domain or segment).
      val cmp = domain.boundOrd.compare(left.upperBound, right.upperBound)
      // left:  ?---------------|
      // right: ?---------------|
      if (cmp == 0) zipper(left.moveNext, right.moveNext)
      else
        // left:  ?---------------|
        // right: ?----------|
        if (cmp > 0)
          // If value of forward segment is invariant (value of backward segment doesn't affect on operator's value),
          // we can immediately define operator's value up to `forward.upperBound` skipping segments of backward
          // sequence.
          if (invariant(left.value)) zipper(right.moveTo(left.upperBound), left)
          else zipper(right.moveNext, left)
        // left:  ?----------|
        // right: ?---------------|
        else
          // If value of forward segment is invariant (value of backward segment doesn't affect on operator's value),
          // we can immediately define operator's value up to `forward.upperBound` skipping segments of backward
          // sequence.
          if (invariant(right.value)) zipper(left.moveTo(right.upperBound), right)
          else zipper(left.moveNext, right)
    }

  protected sealed trait ZippedSegment extends SegmentLike[E, D, W] {

    def frontLeft: GenSegment
    def frontRight: GenSegment

    def hasFront(left: GenSegment, right: GenSegment): Boolean =
      if      (domain.segmentUpperOrd.eqv(left, frontLeft))  domain.segmentUpperOrd.eqv(right, frontRight)
      else if (domain.segmentUpperOrd.eqv(right, frontLeft)) domain.segmentUpperOrd.eqv(left, frontRight)
      else                                                   false

    override lazy val value: W = getSegmentValue(frontLeft, frontRight)

    override def domain: D = seq.domain

    override def moveToFirst: FirstSegment = firstSegment

    override def moveToLast: LastSegment = lastSegment

    override def moveTo(bound: Bound[E]): GenSegment =
      searchFrontZipper(generalZipper, frontLeft.moveTo(bound), frontRight.moveTo(bound))
  }

  protected sealed trait ZippedSegmentWithNext extends ZippedSegment with SegmentWithNext {

    def frontBackward: SegmentWithNext
    def frontForward: GenSegment

    override def upperBound: Bound.Upper[E] = frontBackward.upperBound

    override def moveNext: SegmentWithPrev =
      searchFrontZipper(withPrevZipper, frontBackward.moveNext, frontForward)
  }

  protected sealed trait ZippedSegmentWithPrev extends ZippedSegment with SegmentWithPrev {

    def backBackward: GenSegment = back.backward
    def backForward: SegmentWithPrev = back.forward

    override def lowerBound: Bound.Lower[E] = backForward.lowerBound

    override def movePrev: SegmentWithNext = ???

    private lazy val back: Back = ???

    private case class Back(backward: GenSegment, forward: SegmentWithPrev)
  }

  protected sealed case class ZippedInitialSegment (
    override val frontBackward: SegmentWithNext, override val frontForward: GenSegment
  ) extends ZippedSegmentWithNext with InitialSegment {

    override def frontLeft: GenSegment = frontBackward
    override def frontRight: GenSegment = frontForward
  }

  protected sealed case class ZippedInnerSegment(
    override val frontBackward: SegmentWithNext, override val frontForward: GenSegment
  ) extends ZippedSegmentWithPrev with ZippedSegmentWithNext with InnerSegment {

    override def frontLeft: GenSegment = frontBackward
    override def frontRight: GenSegment = frontForward
  }

  protected sealed case class ZippedTerminalSegment (
    override val frontLeft: LastSegment, override val frontRight: LastSegment
  ) extends ZippedSegmentWithPrev with TerminalSegment {

    override def hasFront(left: GenSegment, right: GenSegment): Boolean = left.isLast && right.isLast
  }

  protected sealed case class ZippedSingleSegment(
    override val frontLeft: LastSegment, override val frontRight: LastSegment
  ) extends ZippedSegment with SingleSegment {

    override def hasFront(left: GenSegment, right: GenSegment): Boolean = left.isLast && right.isLast
  }
}