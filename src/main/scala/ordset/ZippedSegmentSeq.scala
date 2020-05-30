//package ordset
//
//import scala.annotation.tailrec
//
//abstract class ZippedSegmentSeq[E, W] extends SegmentSeq[E, W] {
//  import OrderedSegmentTuple2.{upperBoundAsc => makeOrdSegTuple}
//
//  protected type SegTuple = SegmentTuple2[E, W, Segment[E, W], Segment[E, W]]
//  protected type OrdSegTuple = OrderedSegmentTuple2.UpperBoundAscGen[E, W]
//  protected type OrdSegTupleWithNext = OrderedSegmentTuple2.UpperBoundAscWithNext[E, W]
//  protected type OrdSegTupleWithPrev = OrderedSegmentTuple2.UpperBoundAscWithPrev[E, W]
//
//  protected type SegMakerWithNext[+S <: Segment[E, W]] = (OrdSegTupleWithNext, W) => S
//  protected type SegMakerLast[+S <: Segment[E, W]] = (OrdSegTuple, W) => S
//
//  val valueEq: Eq[W]
//
//  final override def firstSegment: Segment[E, W] = _firstSegment.unwrap
//
//  final override def lastSegment: Segment[E, W] =
//    makeSegmentMergingNext(makeOrdSegTuple(left.lastSegment, right.lastSegment))
//
//  protected val left: SegmentSeq[E, W]
//
//  protected val right: SegmentSeq[E, W]
//
//  protected def operator(left: W, right: W): W
//
//  protected def invariant(value: W): Boolean
//
//  protected val upperBoundOrder: Order[Segment[E, Nothing]] = Segment.upperBoundAscOrder[E](domain.boundOrd)
//
//  protected val lowerBoundOrder: Order[Segment[E, Nothing]] = Segment.lowerBoundAscOrder[E](domain.boundOrd)
//
//  protected def getSegmentValue(tuple: SegTuple): W = {
//    val firstVal = tuple._1.value
//    if (invariant(firstVal)) firstVal
//    else operator(firstVal, tuple._2.value)
//  }
//
//  /** Single segment if sequence is empty or universal, first segment otherwise. */
//  protected final lazy val _firstSegment: FirstSegment = {
//    val tuple = makeOrdSegTuple(left.firstSegment, right.firstSegment)
//    mergeNextOrLast(tuple, getSegmentValue(tuple), FirstSegment.apply, FirstSegment.apply)
//  }
//
//  protected final def mergeNextOrLast[S <: Segment[E, W]](
//      tuple: OrdSegTuple, value: W, makeCurrent: SegMakerWithNext[S], makeLast: SegMakerLast[S]): S = tuple match {
//
//    case n: OrdSegTupleWithNext => mergeNext(n, value, makeCurrent, makeLast)
//    case _ => makeLast(tuple, value)
//  }
//
//  @tailrec
//  protected final def mergeNext[S <: Segment[E, W]](
//      tuple: OrdSegTupleWithNext, value: W, makeCurrent: SegMakerWithNext[S], makeLast: SegMakerLast[S]): S = {
//
//    val next = stepNext(tuple)
//    // Operator has the same value => move to the next segment.
//    if (valueEq.eqv(value, getSegmentValue(next))) next match {
//      case n: OrdSegTupleWithNext => mergeNext(n, value, makeCurrent, makeLast)
//      case _ => makeLast(next, value)
//    }
//    // We'v found bound where operator changed its value => return current segment.
//    else makeCurrent(tuple, value)
//  }
//
//  protected final def stepNext(tuple: OrdSegTupleWithNext): OrdSegTupleWithPrev = tuple.forward match {
//    // X - domain bounds;
//    // | - segment bounds;
//    // ? - unknown bounds (domain or segment).
//    // forward:  ?---------------|
//    // backward: ?---------------|
//    case fn: Segment.WithNext[E, W] if domain.boundOrd.eqv(fn.upperBound, tuple.backward.upperBound) =>
//      makeOrdSegTuple(fn.moveNext, tuple.backward.moveNext)
//    // forward:  ?---------------|
//    // backward: ?----------|
//    // Value of forward segment is invariant =>
//    // we can right away define operator value up to `forward.upperBound`
//    // skipping segments of backward sequence below it.
//    case fn: Segment.WithNext[E, W] if invariant(fn.value) =>
//      makeOrdSegTuple(fn, tuple.backward.moveTo(fn.upperBound))
//    // forward:  ?---------------X
//    // backward: ?----------|
//    // Value of forward segment is invariant
//    case f if invariant(f.value) =>
//      makeOrdSegTuple(f, tuple.backward.moveToLast)
//    // forward:  ?---------------?
//    // backward: ?----------|
//    // Value of forward segment is not invariant
//    case f =>
//      makeOrdSegTuple(f, tuple.backward.moveNext)
//  }
//
//  /**
//    * @param tuple - ordered pair of segments such that:
//    *              1. `segmentOrd(tuple.forward, tuple.backward)` >= 0 - i.e. ordered by upper bound
//    *              2. `tuple.forward` intersects `tuple.backward`
//    *
//    * @return Zipped segment defined by input `tuple`. Function moves to the next segment of sequence until detects
//    *         change of operator value or end of domain.
//    *
//    * @example Input tuple corresponds to the segment of zipped sequence with index 1 (intersection of input segments).
//    *          But until index 5 zipped value doesn't change. So function returns segment 4 of zipped sequence.
//    *          To provide this it moves to index 2 of `left` sequence and to index 2 of `right` sequence.
//    *
//    *         input backward            output forward
//    *               V                         V
//    *          X-----------|-----------|-------------|-----------X - `left` sequence
//    *                0           1            2           3        - segment index
//    *                A           B            C           D        - `left` values (type `W`)
//    *              input forward     output backward
//    *                      V            V
//    *          X-----|-----------|------------|------------------X - `right` sequence
//    *             0        1            2             3            - segment index
//    *             A        B            C             D            - `right` values (type `W`)
//    *                                  output
//    *                                     V
//    *          X-----|-----|-----|-----|------|------|-----------X - zipped sequence
//    *             0     1     2     3      4     5        6        - segment index
//    *             A     A     A     A      A     B        B        - zipped value (`operator` output)
//    */
//  protected final def makeSegmentMergingNext(tuple: OrdSegTuple): Segment[E, W] = {
//
//    def makeCurrent: SegMakerWithNext[Segment[E, W]] = tuple => {
//      if (_firstSegment.hasUpperBound(tuple.backward.upperBound)) _firstSegment.asInitial
//      else InnerSegment(tuple, value)
//    }
//    if (_firstSegment.isSingle) _firstSegment.asSingle
//    else mergeNextOrLast(tuple, getSegmentValue(tuple), makeCurrent, TerminalSegment.apply)
//  }
//
//  /**
//    * @return segment (which has previous segment) for current state. It's similar with `makeSegmentMergingNext`
//    *         but with more strict input and output constraints.
//    * @note preconditions:
//    *       1. `forward` segment intersects `backward`;
//    *       2. `segmentOrd(forward, backward)` >= 0.
//    */
//  protected final def makeSegmentWithPrevMergingNext(
//      forward: Segment.WithPrev[E, W], backward: Segment[E, W]): Segment.WithPrev[E, W] = backward match {
//
//    case bn: Segment.WithNext[E, W] =>
//      mergeNext(forward, bn, getSegmentValue(forward, bn), InnerSegment.apply, TerminalSegment.apply)
//    case _ =>
//      TerminalSegment(forward, backward, getSegmentValue(forward, backward))
//  }
//
//  /**
//    * Base class for non single segments. It has either previous segment or next.
//    * @note preconditions:
//    *       1. `forward` segment intersects `backward`;
//    *       2. `segmentOrd(forward, backward)` >= 0.
//    *       3. zipped sequence is not empty or universal.
//    */
//  protected sealed abstract class SegmentBase protected (
//      tuple: OrdSegTuple,
//      override val value: W
//  ) extends SegmentLike[E, W] {
//
//    override def moveToFirst: Segment[E, W] = firstSegment
//
//    override def moveToLast: Segment[E, W] = lastSegment
//
//    override def moveTo(bound: Bound[E]): Segment[E, W] =
//      makeSegmentMergingNext(makeOrdSegTuple(tuple.forward.moveTo(bound), tuple.backward.moveTo(bound)))
//  }
//
//  protected sealed trait SegmentWithNext extends Segment.WithNext[E, W] {
//
//    val tuple: OrdSegTupleWithNext
//
//    override def upperBound: Bound.Upper[E] = tuple.backward.upperBound
//
//    override def moveNext: Segment.WithPrev[E, W] = makeSegmentWithPrevMergingNext(stepNext(tuple))
//  }
//
//  protected sealed trait SegmentWithPrev extends SegmentBase with Segment.WithPrev[E, W] {
//
//    override def lowerBound: Bound.Lower[E] = ??? //getLowerBound(ind)
//
//    override def movePrev: Segment.WithNext[E, W] = ??? //mkSegmentWithNext(forward, backward)
//  }
//
//  /**
//    * Initial segment of zipped sequence. It must have next segment, so single segment is not initial.
//    * @note preconditions:
//    *       1. all preconditions of `SegmentBase`;
//    *       2. `backward` has next segment.
//    */
//  protected sealed case class InitialSegment (
//      override val forward: Segment[E, W],
//      override val backward: Segment[E, W],
//      override val value: W
//  ) extends SegmentWithNext
//
//  /**
//    * Terminal segment of zipped sequence. It must have previous segment, so single segment is not terminal.
//    * @note preconditions:
//    *       1. all preconditions of `SegmentBase`;
//    *       2. `forward` has previous segment.
//    */
//  protected sealed case class TerminalSegment (
//      override val forward: Segment[E, W],
//      override val backward: Segment[E, W],
//      override val value: W
//  ) extends SegmentWithPrev
//
//  /**
//    * Inner segment of sequence. It must have both previous and next segments.
//    * @note preconditions:
//    *       1. all preconditions of `SegmentBase`;
//    *       2. `backward` has next segment;
//    *       3. `forward` has previous segment.
//    */
//  protected sealed case class InnerSegment(
//      override val forward: Segment[E, W],
//      override val backward: Segment[E, W],
//      override val value: W
//  ) extends SegmentWithPrev with SegmentWithNext with Segment.Inner[E, W]
//
//  /**
//    * First segment of sequence. It is single segment (doesn't have previous and next) if sequence empty or universal.
//    * Otherwise it is initial segment (has next).
//    * @note preconditions:
//    *       1. `forward` segment intersects `backward`;
//    *       2. `segmentOrd(forward, backward)` >= 0.
//    */
//  protected sealed case class FirstSegment(
//      tuple: OrdSegTuple,
//      override val value: W
//  ) extends Segment.Single[E, W] {
//
//    if (tuple.backward.hasNext) {
//      _isSingle = false
//      _initial = InitialSegment(tuple, value)
//    } else {
//      _isSingle = true
//    }
//
//    private var _initial: InitialSegment = _
//    private var _isSingle: Boolean = _
//
//    def isSingle: Boolean = _isSingle
//
//    def isInitial: Boolean = !_isSingle
//
//    def hasUpperBound(bound: Bound.Upper[E]): Boolean =
//      if (_isSingle) false else domain.boundOrd.eqv(_initial.upperBound, bound)
//
//    @throws[UnsupportedOperationException]
//    def asInitial: InitialSegment =
//      if (_isSingle) throw new UnsupportedOperationException("Zipped sequence has single segment")
//      else _initial
//
//    @throws[UnsupportedOperationException]
//    def asSingle: Segment.Single[E, W] =
//      if (_isSingle) this
//      else throw new UnsupportedOperationException("Zipped sequence doesn't have single segment")
//
//    def unwrap: Segment[E, W] = if (_isSingle) this else _initial
//  }
//}