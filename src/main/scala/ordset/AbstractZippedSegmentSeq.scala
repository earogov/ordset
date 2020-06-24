//package ordset
//
//import scala.annotation.tailrec
//
//abstract class AbstractZippedSegmentSeq[E, D <: Domain[E], W] extends SegmentSeq[E, D, W] { seq =>
//
//  val valueEq: Eq[W]
//
//  final override def firstSegment: Segment.First[E, D, W] = ???
//
//  final override def lastSegment: Segment.Last[E, D, W] = ???
//  /** @return segment containing `bound`. */
//  final override def getSegment(bound: Bound[E]): Segment[E, D, W] = ???
//  /** @return segment containing `element`. */
//  final override def getSegment(element: E): Segment[E, D, W] = ???
//
//  protected val left: SegmentSeq[E, D, W]
//
//  protected val right: SegmentSeq[E, D, W]
//
//  protected def operator(left: W, right: W): W
//
//  protected def invariant(value: W): Boolean
//
//  protected def getSegmentValue(left: Segment[E, D, W], right: Segment[E, D, W]): W =
//    // TODO: check invariant for segment with minimal complexity (number of children).
//    if (invariant(left.value)) left.value
//    else operator(left.value, right.value)
//
//  protected class ZippedSegment extends SegmentLike[E, D, W] {
//
//    protected var state: State
//
//    override lazy val value: W = getSegmentValue(state.forward, state.backward)
//
//    override def domain: D = seq.domain
//
//    override def moveToFirst: Segment[E, D, W] = firstSegment
//
//    override def moveToLast: Segment[E, D, W] = lastSegment
//
//    override def moveTo(bound: Bound[E]): Segment[E, D, W] = ???
//
//    protected def initialize(): Unit = if (!state.initialized) {
//      var stepNext = true
//      var first = state.forward
//      var second = state.backward
//      var prevForward: Segment[E, D, W] = null
//      var prevBackward: Segment[E, D, W] = null
//      while (stepNext) {
//        (first, second) match {
//          case (fn: Segment.WithNext[E, D, W], sn: Segment.WithNext[E, D, W]) =>
//            val choice = SegmentBinaryChoice.forwardChoice(fn, sn, domain.segmentOrd)
//
//          case (fn: Segment.WithNext[E, D, W], _) => ???
//          case (_, sn: Segment.WithNext[E, D, W]) => ???
//          case (_, _) =>
//            state = new State(first, second, true)
//        }
//      }
//    }
//
//    protected case class State (forward: Segment[E, D, W], backward: Segment[E, D, W], initialized: Boolean)
//  }
//}