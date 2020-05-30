package test.ordset

import org.scalatest.funspec.AnyFunSpec

trait SegmentSeqBehaviors[E, V] { this: AnyFunSpec =>

  import ordset._
  import scala.annotation.tailrec

  def segmentsSupportMovePrevAndNext(
      descr: String, segment: Segment[E, V], expected: Seq[IntervalMapping[E, V]]): Unit = {

    it(s"should move to the next segment if there is one for $descr") {
      @tailrec
      def loop(seg: Segment[E, V], exp: Seq[IntervalMapping[E, V]]): Unit = exp match {
        case e :: es =>
          assert(seg.intervalMapping == e)
          seg match {
            case s: Segment.WithNext[E, V] => loop(s.moveNext, es)
            case _ => // end
          }
        case _ => fail("Invalid test case: expected sequence must be non empty.")
      }
      loop(segment, expected)
    }

    it(s"should move to the previous segment if there is one for $descr") {
      @tailrec
      def loop(seg: Segment[E, V], exp: Seq[IntervalMapping[E, V]]): Unit = exp match {
        case e :: es =>
          assert(seg.intervalMapping == e)
          seg match {
            case s: Segment.WithPrev[E, V] => loop(s.movePrev, es)
            case _ => // end
          }
        case _ => fail("Invalid test case: expected sequence must be non empty.")
      }
      @tailrec
      def last(seg: Segment[E, V]): Segment[E, V] = seg match {
        case s: Segment.WithNext[E, V] => last(s.moveNext)
        case l @ _ => l
      }
      loop(last(segment), expected.reverse)
    }
  }

  def segmentsSupportMoveToBound(
      descr: String, seg: Segment[E, V], moveSeq: Seq[(Bound[E], IntervalMapping[E, V])]): Unit = {

    it(s"should move to the specified bound for $descr") {
      @tailrec
      def loop(seg: Segment[E, V], moveSeq: Seq[(Bound[E], IntervalMapping[E, V])]): Unit = moveSeq match {
        case (bnd, exp) :: xs =>
          val next = seg.moveTo(bnd)
          assert(next.intervalMapping == exp)
          loop(next, xs)
        case _ => // end
      }
      loop(seg, moveSeq)
    }
  }

  def segmentsSupportMoveToFirstAndLast(
    descr: String, seq: SegmentSeq[E, V], firstExp: IntervalMapping[E, V], lastExp: IntervalMapping[E, V]): Unit = {

    it(s"should move to the first and last segments for $descr") {
      @tailrec
      def loop(seg: Segment[E, V]): Unit = {
        assert(seg.moveToFirst.intervalMapping == firstExp)
        assert(seg.moveToLast.intervalMapping == lastExp)
        seg match {
          case n: Segment.WithNext[E, V] => loop(n.moveNext)
          case _ => // end
        }
      }
      loop(seq.firstSegment)
    }
  }

  def segmentsHaveNextAndPrevIndicators(descr: String, seq: SegmentSeq[E, V]): Unit = {

    it(s"should have valid navigation indicators (`hasNext`, `hasPrev`, `isFirst`, ...) for $descr") {
      @tailrec
      def loop(seg: Segment[E, V]): Unit = seg match {
        case n: Segment.Inner[E, V] =>
          assert(seg.isInner)
          assert(seg.hasNext)
          assert(seg.hasPrev)
          assert(!seg.isInitial)
          assert(!seg.isTerminal)
          assert(!seg.isFirst)
          assert(!seg.isLast)
          assert(!seg.isSingle)
          loop(n.moveNext)
        case n: Segment.Initial[E, V] =>
          assert(seg.isInitial)
          assert(seg.hasNext)
          assert(seg.isFirst)
          assert(!seg.isSingle)
          assert(!seg.isInner)
          assert(!seg.isLast)
          assert(!seg.hasPrev)
          assert(!seg.isTerminal)
          loop(n.moveNext)
        case _: Segment.Terminal[E, V] =>
          assert(seg.isTerminal)
          assert(seg.isLast)
          assert(seg.hasPrev)
          assert(!seg.isSingle)
          assert(!seg.isInner)
          assert(!seg.isFirst)
          assert(!seg.hasNext)
          assert(!seg.isInitial)
        case _: Segment.Single[E, V] =>
          assert(seg.isSingle)
          assert(seg.isFirst)
          assert(seg.isLast)
          assert(!seg.isInitial)
          assert(!seg.isTerminal)
          assert(!seg.hasNext)
          assert(!seg.hasPrev)
          assert(!seg.isInner)
        case _ => sys.error("Unexpected case")
      }
      loop(seq.firstSegment)
    }
  }
}