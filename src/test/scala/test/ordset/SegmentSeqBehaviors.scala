package test.ordset

import ordset.domain.Domain
import org.scalatest.funspec.AnyFunSpec

trait SegmentSeqBehaviors[E, D <: Domain[E], V] { this: AnyFunSpec =>

  import ordset._
  import scala.annotation.tailrec

  def segmentsSupportMovePrevAndNext(
    descr: String,
    segmentSeq: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  )(
    implicit valueHash: Hash[V]
  ): Unit = {

    val intervalRelationHash = segmentSeq.domainOps.intervalRelationHash

    it(s"should move to the next segment if there is one for $descr") {
      @tailrec
      def loop(seg: Segment[E, D, V], exp: Seq[IntervalRelation[E, D, V]]): Unit = exp match {
        case e :: es =>
          val relation = seg.intervalRelation
          assert(intervalRelationHash.eqv(relation, e), s"expected: $e, actual: $relation")
          assertSameBounds(seg, e.interval)
          seg match {
            case s: Segment.WithNext[E, D, V] => loop(s.moveNext, es)
            case _ => // end
          }
        case _ => fail("Invalid test case: expected sequence must be non empty.")
      }
      loop(segmentSeq.firstSegment, expected)
    }

    it(s"should move to the previous segment if there is one for $descr") {
      @tailrec
      def loop(seg: Segment[E, D, V], exp: Seq[IntervalRelation[E, D, V]]): Unit = exp match {
        case e :: es =>
          val relation = seg.intervalRelation
          assert(intervalRelationHash.eqv(relation, e), s"expected: $e, actual: $relation")
          assertSameBounds(seg, e.interval)
          seg match {
            case s: Segment.WithPrev[E, D, V] => loop(s.movePrev, es)
            case _ => // end
          }
        case _ => fail("Invalid test case: expected sequence must be non empty.")
      }
      loop(segmentSeq.lastSegment, expected.reverse)
    }
  }

  def segmentsSupportMoveToBound(
    descr: String,
    segmentSeq: SegmentSeq[E, D, V],
    moveSeq: Seq[(Bound[E], IntervalRelation[E, D, V])]
  )(
    implicit valueHash: Hash[V]
  ): Unit = {

    val intervalRelationHash = segmentSeq.domainOps.intervalRelationHash

    it(s"should move to the specified bound for $descr") {
      @tailrec
      def loop(seg: Segment[E, D, V], moveSeq: Seq[(Bound[E], IntervalRelation[E, D, V])]): Unit = moveSeq match {
        case (bnd, exp) :: xs =>
          val next = seg.moveTo(bnd)
          val relation = next.intervalRelation
          assert(intervalRelationHash.eqv(relation, exp), s"expected: $exp, actual: $relation")
          loop(next, xs)
        case _ => // end
      }
      loop(segmentSeq.firstSegment, moveSeq)
    }
  }

  def segmentsSupportMoveToFirstAndLast(
    descr: String,
    seq: SegmentSeq[E, D, V],
    firstExp: IntervalRelation[E, D, V],
    lastExp: IntervalRelation[E, D, V]
  )(
    implicit valueHash: Hash[V]
  ): Unit = {

    val intervalRelationHash = seq.domainOps.intervalRelationHash

    it(s"should move to the first and last segments for $descr") {
      @tailrec
      def loop(seg: Segment[E, D, V]): Unit = {
        val firstRelation = seg.moveToFirst.intervalRelation
        assert(intervalRelationHash.eqv(firstRelation, firstExp), s"expected: $firstExp, actual: $firstRelation")
        val lastRelation = seg.moveToLast.intervalRelation
        assert(intervalRelationHash.eqv(lastRelation, lastExp), s"expected: $lastExp, actual: $lastRelation")
        seg match {
          case n: Segment.WithNext[E, D, V] => loop(n.moveNext)
          case _ => // end
        }
      }
      loop(seq.firstSegment)
    }
  }

  def segmentsHaveNextAndPrevIndicators(
    descr: String,
    seq: SegmentSeq[E, D, V]
  ): Unit = {

    it(s"should have valid navigation indicators (`hasNext`, `hasPrev`, `isFirst`, ...) for $descr") {
      @tailrec
      def loop(seg: Segment[E, D, V]): Unit = seg match {
        case n: Segment.Inner[E, D, V] =>
          assert(seg.isInner, s"expected $seg is inner segment")
          assert(seg.hasNext, s"expected $seg has next segment")
          assert(seg.hasPrev, s"expected $seg has previous segment")
          assert(!seg.isInitial, s"expected $seg is not initial segment")
          assert(!seg.isTerminal, s"expected $seg is not terminal segment")
          assert(!seg.isFirst, s"expected $seg is not first segment")
          assert(!seg.isLast, s"expected $seg is not last segment")
          assert(!seg.isSingle, s"expected $seg is not single segment")
          loop(n.moveNext)
        case n: Segment.Initial[E, D, V] =>
          assert(seg.isInitial, s"expected $seg is initial segment")
          assert(seg.hasNext, s"expected $seg has next segment")
          assert(seg.isFirst, s"expected $seg is first segment")
          assert(!seg.isSingle, s"expected $seg is not single segment")
          assert(!seg.isInner, s"expected $seg is not inner segment")
          assert(!seg.isLast, s"expected $seg is not last segment")
          assert(!seg.hasPrev, s"expected $seg does not have previous segment")
          assert(!seg.isTerminal, s"expected $seg is not terminal segment")
          loop(n.moveNext)
        case _: Segment.Terminal[E, D, V] =>
          assert(seg.isTerminal, s"expected $seg is terminal segment")
          assert(seg.isLast, s"expected $seg is last segment")
          assert(seg.hasPrev, s"expected $seg has previous segment")
          assert(!seg.isSingle, s"expected $seg is not single segment")
          assert(!seg.isInner, s"expected $seg is not inner segment")
          assert(!seg.isFirst, s"expected $seg is not first segment")
          assert(!seg.hasNext, s"expected $seg does not have next segment")
          assert(!seg.isInitial, s"expected $seg is not initial segment")
        case _: Segment.Single[E, D, V] =>
          assert(seg.isSingle, s"expected $seg is single segment")
          assert(seg.isFirst, s"expected $seg is first segment")
          assert(seg.isLast, s"expected $seg is last segment")
          assert(!seg.isInitial, s"expected $seg is not initial segment")
          assert(!seg.isTerminal, s"expected $seg is not terminal segment")
          assert(!seg.hasNext, s"expected $seg does not have next segment")
          assert(!seg.hasPrev, s"expected $seg does not have previous segment")
          assert(!seg.isInner, s"expected $seg is not inner segment")
        case _ => sys.error("Unexpected case")
      }
      loop(seq.firstSegment)
    }
  }

  def assertSameBounds(segment: Segment[E, D, V], interval: Interval[E, D]): Unit = {
    interval match {
      case i: Interval.WithLowerBound[E, D] =>
        assert(segment.hasLowerBound, s"expected $segment has lower bound")
        assert(segment.hasLowerBound(i.lowerBound), s"expected $segment has lower bound ${i.lowerBound}")
      case _ =>
        assert(!segment.hasLowerBound, s"expected $segment does not have lower bound")
    }
    interval match {
      case i: Interval.WithUpperBound[E, D] =>
        assert(segment.hasUpperBound, s"expected $segment has upper bound")
        assert(segment.hasUpperBound(i.upperBound), s"expected $segment has upper bound ${i.upperBound}")
      case _ =>
        assert(!segment.hasUpperBound, s"expected $segment does not have upper bound")
    }
  }
}