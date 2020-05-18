package test.ordset

import org.scalatest.funspec.AnyFunSpec

trait SegmentSeqBehaviors[E, V] { this: AnyFunSpec =>

  import ordset._
  import scala.annotation.tailrec

  def supportMovePrevAndNextForSegments(
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
      def last(seg: Segment[E, V]): Segment[E, V] = seg match {
        case s: Segment.WithNext[E, V] => last(s.moveNext)
        case l @ _ => l
      }
      loop(last(segment), expected.reverse)
    }
  }

  def supportMoveToForSegments(
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
}