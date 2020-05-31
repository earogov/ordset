package ordset

import scala.annotation.tailrec

abstract class AbstractZippedSegmentSeq[E, W] extends SegmentSeq[E, W] { seq =>

  val valueEq: Eq[W]

  final override def firstSegment: Segment.First[E, W] = _firstSegment

  final override def lastSegment: Segment.Last[E, W] = makeLastSegment(left.lastSegment, right.lastSegment)

  /** @return segment containing `bound`. */
  final override def getSegment(bound: Bound[E]): Segment[E, W] =
    makeSegment(left.getSegment(bound), right.getSegment(bound))

  /** @return segment containing `element`. */
  final override def getSegment(element: E): Segment[E, W] =
    makeSegment(left.getSegment(element), right.getSegment(element))

  protected type GenSegment =  SegmentBase with Segment[E, W]

  protected type FirstSegment = SegmentBase with Segment.First[E, W]

  protected type InnerSegment = SegmentWithPrev with SegmentWithNext with Segment.Inner[E, W]

  protected type LastSegment = SegmentBase with Segment.Last[E, W]

  protected val left: SegmentSeq[E, W]

  protected val right: SegmentSeq[E, W]

  protected def operator(left: W, right: W): W

  protected def invariant(value: W): Boolean

  protected def getSegmentValue(left: Segment[E, W], right: Segment[E, W]): W =
    // TODO: check invariant for segment with minimal complexity (number of children).
    if (invariant(left.value)) left.value
    else operator(left.value, right.value)

  protected final lazy val _firstSegment: FirstSegment =
    mergeFirst(makeFirstSegment(left.firstSegment, right.firstSegment))

  protected def makeSegment(left: Segment[E, W], right: Segment[E, W]): GenSegment = (left, right) match {
    case (ln: Segment.WithNext[E, W], rn: Segment.WithNext[E, W]) =>
      if (domain.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0) makeSegmentWithNext(rn, ln)
      else makeSegmentWithNext(ln, rn)
    case (ln: Segment.WithNext[E, W], _) => makeSegmentWithNext(ln, right)
    case (_, rn: Segment.WithNext[E, W]) => makeSegmentWithNext(rn, left)
    case _ => makeLastSegment(left, right)
  }

  protected def makeSegmentWithPrev(left: Segment.WithPrev[E, W], right: Segment[E, W]): SegmentWithPrev =
    (left, right) match {
      case (ln: Segment.WithNext[E, W], rn: Segment.WithNext[E, W]) =>
        if (domain.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0) makeInnerSegment(rn, ln)
        else makeInnerSegment(ln, rn)
      case (ln: Segment.WithNext[E, W], _) => makeInnerSegment(ln, right)
      case (_, rn: Segment.WithNext[E, W]) => makeInnerSegment(rn, left)
      case _ => makeTerminalSegment(left, right)
    }

  protected def makeSegmentWithNext(left: Segment.WithNext[E, W], right: Segment[E, W]): SegmentWithNext =
    if (_firstSegment.upperBoundGreaterOrEqv(left.upperBound)) _firstSegment.asInstanceOf[InitialSegment]
    else makeInnerSegment(left, right)

  @throws[IllegalArgumentException]("Either `left` or `right` segment should have previous segment.")
  protected def makeInnerSegment(left: Segment.WithNext[E, W], right: Segment[E, W]): InnerSegment =
    (left, right) match {
      case (lp: Segment.WithPrev[E, W], rp: Segment.WithPrev[E, W]) =>
        if (domain.boundOrd.compare(lp.lowerBound, rp.lowerBound) >= 0) InnerSegment2(lp, rp)
        else InnerSegment1(lp, rp)
      case (lp: Segment.WithPrev[E, W], _) => InnerSegment2(lp, right)
      case (_, rp: Segment.WithPrev[E, W]) => InnerSegment1(left, rp)
      case _ => throw new IllegalArgumentException("Either `left` or `right` segment should have previous segment.")
    }

  protected def makeFirstSegment(left: Segment[E, W], right: Segment[E, W]): FirstSegment =
    (left, right) match {
      case (ln: Segment.WithNext[E, W], rn: Segment.WithNext[E, W]) =>
        if (domain.boundOrd.compare(ln.upperBound, rn.upperBound) >= 0) InitialSegment(rn, ln)
        else InitialSegment(ln, rn)
      case (ln: Segment.WithNext[E, W], _) => InitialSegment(ln, right)
      case (_, rn: Segment.WithNext[E, W]) => InitialSegment(rn, left)
      case _ => SingleSegment(left, right)
    }

  protected def makeLastSegment(left: Segment[E, W], right: Segment[E, W]): LastSegment =
    if (_firstSegment.isSingle) _firstSegment.asInstanceOf[SingleSegment]
    else makeTerminalSegment(left, right)

  @throws[IllegalArgumentException]("Either `left` or `right` segment should have previous segment.")
  protected def makeTerminalSegment(left: Segment[E, W], right: Segment[E, W]): TerminalSegment =
    (left, right) match {
      case (lp: Segment.WithPrev[E, W], rp: Segment.WithPrev[E, W]) =>
        if (domain.boundOrd.compare(lp.lowerBound, rp.lowerBound) >= 0) TerminalSegment(rp, lp)
        else TerminalSegment(lp, rp)
      case (lp: Segment.WithPrev[E, W], _) => TerminalSegment(right, lp)
      case (_, rp: Segment.WithPrev[E, W]) => TerminalSegment(left, rp)
      case _ => throw new IllegalArgumentException("Either `left` or `right` segment should have previous segment.")
    }

  protected final def stepNext(current: SegmentWithNext): SegmentWithPrev = current.upperForward match {
    // X - domain bounds;
    // | - segment bounds;
    // ? - unknown bounds (domain or segment).
    // upperForward:  ?---------------|
    // upperBackward: ?---------------|
    case fn: Segment.WithNext[E, W] if domain.boundOrd.eqv(current.upperBackward.upperBound, fn.upperBound) =>
      makeSegmentWithPrev(current.upperBackward.moveNext, fn.moveNext)
    // upperForward:  ?---------------|
    // upperBackward: ?----------|
    // Value of forward segment is invariant =>
    // we can right away define operator's value up to `upperForward.upperBound`
    // skipping segments of backward sequence below it.
    case fn: Segment.WithNext[E, W] if invariant(fn.value) =>
      // Backward segment has next segment and its upper bound is less then `fn.upperBound` =>
      // new segment of backward sequence has previous segment.
      makeSegmentWithPrev(current.upperBackward.moveTo(fn.upperBound).asInstanceOf[Segment.WithPrev[E, W]], fn)
    // upperForward:  ?---------------X
    // upperBackward: ?----------|
    // Value of forward segment is invariant and it is the last segment =>
    // we can move to the last segment of backward sequence.
    case f if invariant(f.value) =>
      // Backward segment has next segment => last segment of backward sequence has previous segment.
      makeSegmentWithPrev(current.upperBackward.moveToLast.asInstanceOf[Segment.WithPrev[E, W]], f)
    // upperForward:  ?---------------?
    // upperBackward: ?----------|
    // Value of forward segment is not invariant.
    case f =>
      makeSegmentWithPrev(current.upperBackward.moveNext, f)
  }

  protected final def stepPrev(current: SegmentWithPrev): SegmentWithNext = current.lowerBackward match {
    // X - domain bounds;
    // | - segment bounds;
    // ? - unknown bounds (domain or segment).
    // lowerForward:  |---------------?
    // lowerBackward: |---------------?
    case bp: Segment.WithPrev[E, W] if domain.boundOrd.eqv(current.lowerForward.lowerBound, bp.lowerBound) =>
      makeSegmentWithNext(current.lowerForward.movePrev, bp.movePrev)
    // lowerForward:       |----------?
    // lowerBackward: |---------------?
    // Value of backward segment is invariant =>
    // we can right away define operator's value up to `lowerBackward.lowerBound`
    // skipping segments of forward sequence above it.
    case bp: Segment.WithPrev[E, W] if invariant(bp.value) =>
      // Forward segment has previous segment and its lower bound is greater then `bp.lowerBound` =>
      // new segment of forward sequence has next segment.
      makeSegmentWithNext(current.lowerForward.moveTo(bp.lowerBound).asInstanceOf[Segment.WithNext[E, W]], bp)
    // lowerForward:       |----------?
    // lowerBackward: X---------------?
    // Value of backward segment is invariant and it is the first segment =>
    // we can move to the first segment of forward sequence.
    case b if invariant(b.value) =>
      // Forward segment has previous segment => first segment of forward sequence has next segment.
      makeSegmentWithNext(current.lowerForward.moveToFirst.asInstanceOf[Segment.WithNext[E, W]], b)
    // lowerForward:       |----------?
    // lowerBackward: ?---------------?
    // Value of backward segment is not invariant.
    case b =>
      makeSegmentWithNext(current.lowerForward.movePrev, b)
  }

  @tailrec
  protected final def mergeNext[S <: SegmentWithNext](current: S): S = {
    val next = stepNext(current)
    // Operator has the same value => move to the next segment.
    if (valueEq.eqv(current.value, next.value)) next match {
      // Function moves within one segment of zipped sequence => class of segment is preserved.
      case n: SegmentWithNext => mergeNext(n.asInstanceOf[S])
      case _ => current
    }
    // We'v found bound where operator changed its value => return current segment.
    else current
  }

  @tailrec
  protected final def mergePrev[S <: SegmentWithPrev](current: S): S = {
    val prev = stepPrev(current)
    // Operator has the same value => move to the previous segment.
    if (valueEq.eqv(current.value, prev.value)) prev match {
      // Function moves within one segment of zipped sequence => class of segment is preserved.
      case p: SegmentWithPrev => mergePrev(p.asInstanceOf[S])
      case _ => current
    }
    // We'v found bound where operator changed its value => return current segment.
    else current
  }

  protected final def mergeFirst(current: FirstSegment): FirstSegment = current match {
    case n: InitialSegment => mergeNext[InitialSegment](n)
    case s: SingleSegment => s
    // There are only `InitialSegment` and `SingleSegment` classes implementing `FirstSegment`.
    case _ => sys.error("Unexpected case")
  }

  protected sealed trait SegmentBase extends SegmentLike[E, W] {

    def left: Segment[E, W]

    def right: Segment[E, W]

    def upperBoundGreaterOrEqv(bound: Bound.Upper[E]): Boolean

    override lazy val value: W = getSegmentValue(left, right)

    override def domain: Domain[E] = seq.domain

    override def moveToFirst: Segment[E, W] = firstSegment

    override def moveToLast: Segment[E, W] = lastSegment

    override def moveTo(bound: Bound[E]): Segment[E, W] = makeSegment(left.moveTo(bound), right.moveTo(bound)) match {
      case n: SegmentWithNext => mergeNext(n)
      case s => s
    }
  }

  /**
    * Segment of zipped sequence with next segment. It's represented by pair of segments:
    * `upperForward` - segment of first original sequence;
    * `upperBackward` - segment of second original sequence.
    * The following conditions must be provided for them:
    * 1. segments are intersecting;
    * 2. segments are ordered by upper bound: `upperForward.upperBound` >= `upperBackward.upperBound`.
    */
  protected sealed trait SegmentWithNext extends SegmentBase with Segment.WithNext[E, W] {

    def upperBackward: Segment.WithNext[E, W]
    def upperForward: Segment[E, W]

    override def upperBoundGreaterOrEqv(bound: Bound.Upper[E]): Boolean =
      domain.boundOrd.gteqv(upperBackward.upperBound, bound)

    override def upperBound: Bound.Upper[E] = upperBackward.upperBound

    override def moveNext: Segment.WithPrev[E, W] = {
      stepNext(this) match {
        case n: SegmentWithNext => mergeNext(n)
        case t: TerminalSegment => t
        // `this` has next segment => zipped sequence is not empty or universal =>
        // `SingleSegment` is impossible here.
        case _ => sys.error("Unexpected case")
      }
    }
  }

  protected sealed trait SegmentWithPrev extends SegmentBase with Segment.WithPrev[E, W] {

    def lowerBackward: Segment[E, W]
    def lowerForward: Segment.WithPrev[E, W]

    override lazy val lowerBound: Bound.Lower[E] = mergePrev(this).lowerForward.lowerBound

    override def movePrev: Segment.WithNext[E, W] = stepPrev(mergePrev(this))
  }

  protected sealed case class InitialSegment (
    override val upperBackward: Segment.WithNext[E, W], override val upperForward: Segment[E, W]
  ) extends SegmentWithNext with Segment.Initial[E, W] {

    override def left: Segment[E, W] = upperBackward
    override def right: Segment[E, W] = upperForward
  }

  protected sealed case class TerminalSegment (
    override val lowerBackward: Segment[E, W], override val lowerForward: Segment.WithPrev[E, W]
  ) extends SegmentWithPrev with Segment.Terminal[E, W] {

    override def left: Segment[E, W] = lowerBackward
    override def right: Segment[E, W] = lowerForward

    override def upperBoundGreaterOrEqv(bound: Bound.Upper[E]): Boolean = true
  }

  protected sealed case class InnerSegment1(
    override val upperBackward: Segment.WithNext[E, W], override val lowerForward: Segment.WithPrev[E, W]
  ) extends SegmentWithPrev with SegmentWithNext with Segment.Inner[E, W] {

    override def left: Segment[E, W] = upperBackward
    override def right: Segment[E, W] = lowerForward

    override def lowerBackward: Segment[E, W] = upperBackward
    override def upperForward: Segment[E, W] = lowerForward
  }

  protected sealed case class InnerSegment2(
    inner: Segment.WithNext[E, W] with Segment.WithPrev[E, W], other: Segment[E, W]
  ) extends SegmentWithPrev with SegmentWithNext with Segment.Inner[E, W] {

    override def left: Segment[E, W] = inner
    override def right: Segment[E, W] = other

    override def lowerBackward: Segment[E, W] = other
    override def lowerForward: Segment.WithPrev[E, W] = inner

    override def upperBackward: Segment.WithNext[E, W] = inner
    override def upperForward: Segment[E, W] = other
  }

  protected sealed case class SingleSegment(left: Segment[E, W], right: Segment[E, W]
  ) extends SegmentBase with Segment.Single[E, W] {

    override def upperBoundGreaterOrEqv(bound: Bound.Upper[E]): Boolean = true
  }
}