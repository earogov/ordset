package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

trait NavigationBehaviors[E, D <: Domain[E], V]
  extends SegmentSeqAssert[E, D, V] { this: AnyFunSpec =>

  import ordset._
  import ordset.core._

  import scala.annotation.tailrec

  def segmentsSupportMovePrevAndNext(
    samples: Iterable[SegmentSeqSample[E, D, V]]
  )(
    implicit valueHash: Hash[V]
  ): Unit = samples.foreach { sample =>

    val intervalRelHash = sample.domainOps.intervalRelationHash

    it(s"should move to the next segment if there is one for $sample") {
      @tailrec
      def loop(seg: Segment[E, D, V], expected: Seq[IntervalRelation[E, D, V]]): Unit = expected match {
        case expectedRel :: tail =>
          val actualRel = seg.intervalRelation
          assert(intervalRelHash.eqv(actualRel, expectedRel), s"expected: $expectedRel, actual: $actualRel")
          assertSameBounds(seg, expectedRel.interval)
          seg match {
            case seg: Segment.WithNext[E, D, V] => loop(seg.moveNext, tail)
            case _ => // end
          }
        case _ => fail("Invalid test case: expected sequence must be non empty.")
      }
      loop(sample.sequence.firstSegment, sample.reference)
    }

    it(s"should move to the previous segment if there is one for $sample") {
      @tailrec
      def loop(seg: Segment[E, D, V], expected: Seq[IntervalRelation[E, D, V]]): Unit = expected match {
        case expectedRel :: tail =>
          val actualRel = seg.intervalRelation
          assert(intervalRelHash.eqv(actualRel, expectedRel), s"expected: $expectedRel, actual: $actualRel")
          assertSameBounds(seg, expectedRel.interval)
          seg match {
            case seg: Segment.WithPrev[E, D, V] => loop(seg.movePrev, tail)
            case _ => // end
          }
        case _ => fail("Invalid test case: expected sequence must be non empty.")
      }
      loop(sample.sequence.lastSegment, sample.reference.reverse)
    }
  }

  def segmentsSupportMoveToBound(
    samples: Iterable[SegmentSeqSample[E, D, V] with SegmentMoveToBoundTest[E, D, V]]
  )(
    implicit valueHash: Hash[V]
  ): Unit = samples.foreach { sample =>

    val intervalRelHash = sample.domainOps.intervalRelationHash

    it(s"should move to the specified bound for $sample") {
      @tailrec
      def loop(seg: Segment[E, D, V], moveSeq: Seq[(Bound[E], IntervalRelation[E, D, V])]): Unit = moveSeq match {
        case (bound, expectedRel) :: tail =>
          val actualSeg = seg.moveTo(bound)
          val actualRel = actualSeg.intervalRelation
          assert(intervalRelHash.eqv(actualRel, expectedRel), s"expected: $expectedRel, actual: $actualRel")
          loop(actualSeg, tail)
        case _ => // end
      }
      loop(sample.sequence.firstSegment, sample.moveToBoundSeq)
    }
  }

  def segmentsSupportMoveToFirstAndLast(
    samples: Iterable[SegmentSeqSample[E, D, V]]
  )(
    implicit valueHash: Hash[V]
  ): Unit = samples.foreach { sample =>

    val intervalRelHash = sample.domainOps.intervalRelationHash

    it(s"should move to the first and last segments for $sample") {
      if (sample.reference.isEmpty) fail("Invalid test case: expected sequence must be non empty.")
      val firstExp = sample.reference.head
      val lastExp = sample.reference.last
      @tailrec
      def loop(seg: Segment[E, D, V]): Unit = {
        val firstRel = seg.moveToFirst.intervalRelation
        assert(intervalRelHash.eqv(firstRel, firstExp), s"expected: $firstExp, actual: $firstRel")
        val lastRel = seg.moveToLast.intervalRelation
        assert(intervalRelHash.eqv(lastRel, lastExp), s"expected: $lastExp, actual: $lastRel")
        seg match {
          case seg: Segment.WithNext[E, D, V] => loop(seg.moveNext)
          case _ => // end
        }
      }
      loop(sample.sequence.firstSegment)
    }
  }

  def segmentsHaveNextAndPrevIndicators(
    samples: Iterable[SegmentSeqSample[E, D, V]]
  ): Unit = samples.foreach { sample =>

    it(s"should have valid navigation indicators (`hasNext`, `hasPrev`, `isFirst`, ...) for $sample") {
      @tailrec
      def loop(seg: Segment[E, D, V]): Unit = seg match {
        case seg: Segment.Inner[E, D, V] =>
          assert(seg.isInner, s"expected $seg is inner segment")
          assert(seg.hasNext, s"expected $seg has next segment")
          assert(seg.hasPrev, s"expected $seg has previous segment")
          assert(!seg.isInitial, s"expected $seg is not initial segment")
          assert(!seg.isTerminal, s"expected $seg is not terminal segment")
          assert(!seg.isFirst, s"expected $seg is not first segment")
          assert(!seg.isLast, s"expected $seg is not last segment")
          assert(!seg.isSingle, s"expected $seg is not single segment")
          loop(seg.moveNext)
        case seg: Segment.Initial[E, D, V] =>
          assert(seg.isInitial, s"expected $seg is initial segment")
          assert(seg.hasNext, s"expected $seg has next segment")
          assert(seg.isFirst, s"expected $seg is first segment")
          assert(!seg.isSingle, s"expected $seg is not single segment")
          assert(!seg.isInner, s"expected $seg is not inner segment")
          assert(!seg.isLast, s"expected $seg is not last segment")
          assert(!seg.hasPrev, s"expected $seg does not have previous segment")
          assert(!seg.isTerminal, s"expected $seg is not terminal segment")
          loop(seg.moveNext)
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

      loop(sample.sequence.firstSegment)
    }
  }
}
