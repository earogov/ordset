package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.SegmentSeqAssertions._
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

trait NavigationBehaviors[E, D <: Domain[E], V] { 
  this: AnyFunSpec =>

  import ordset._
  import ordset.core._

  import scala.annotation.tailrec

  def segmentsSupportMovePrevAndNext(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]]]
  ): Unit = 
    samples.foreach { sample =>

      val intervalRelHash = sample.intervalRelationHash
  
      it(s"should move to the next segment if there is one for $sample") {
        @tailrec
        def loop(seg: Segment[E, D, V], expected: Seq[IntervalRelation[E, D, V]]): Unit = expected match {
          case expectedRel :: tail =>
            val actualRel = seg.intervalRelation
            assert(intervalRelHash.eqv(actualRel, expectedRel), s"expected: $expectedRel, actual: $actualRel")
            seg match {
              case seg: Segment.WithNext[E, D, V] => loop(seg.moveNext, tail)
              case _ => // end
            }
          case _ => fail("Invalid test case: expected sequence must be non-empty.")
        }
        loop(sample.sequence.firstSegment, sample.reference)
      }

      it(s"should move to the previous segment if there is one for $sample") {
        @tailrec
        def loop(seg: Segment[E, D, V], expected: Seq[IntervalRelation[E, D, V]]): Unit = expected match {
          case expectedRel :: tail =>
            val actualRel = seg.intervalRelation
            assert(intervalRelHash.eqv(actualRel, expectedRel), s"expected: $expectedRel, actual: $actualRel")
            seg match {
              case seg: Segment.WithPrev[E, D, V] => loop(seg.movePrev, tail)
              case _ => // end
            }
          case _ => fail("Invalid test case: expected sequence must be non-empty.")
        }
        loop(sample.sequence.lastSegment, sample.reference.reverse)
      }
    }

  def segmentsSupportMoveToBound(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentMoveToBoundTest[E, D, V]]
  ): Unit = 
    samples.foreach { sample =>
    
      val intervalRelHash = sample.intervalRelationHash
  
      it(s"should move to the specified bound for $sample") {
        @tailrec
        def loop(seg: Segment[E, D, V], moveSeq: Seq[(Bound[E], IntervalRelation[E, D, V])]): Unit = moveSeq match {
          case (bound, expectedRel) :: tail =>
            // `segment.moveTo(bound)` is valid
            val actualSeg = seg.moveTo(bound)
            val actualRel = actualSeg.intervalRelation
            assert(intervalRelHash.eqv(actualRel, expectedRel), s"expected: $expectedRel, actual: $actualRel")
  
            // `segment.contains(bound)` is valid
            assert(actualSeg.containsBound(bound), s"expected $actualSeg contains bound $bound")
            val containsElement =
              if (bound.isInclusive) true
              else bound match {
                //    )5
                // (-----)
                // => segment contains 5
                case bound: Bound.Upper[E] if !actualSeg.hasUpperBound(bound) => true
                //    5(
                // (-----)
                // => segment contains 5
                case bound: Bound.Lower[E] if !actualSeg.hasLowerBound(bound) => true
                //       )5   5(
                // (-----)     (-----)
                // => segment doesn't contain 5
                case _ => false
              }
            if (containsElement) {
              assert(actualSeg.containsElement(bound.element), s"expected $actualSeg contains element ${bound.element}")
            }
  
            loop(actualSeg, tail)
          case _ => // end
        }
        loop(sample.sequence.firstSegment, sample.moveToBoundSeq)
      }
    }

  def segmentsSupportMoveToFirstAndLast(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]]]
  ): Unit = 
    samples.foreach { sample =>

      val intervalRelHash = sample.intervalRelationHash
  
      it(s"should move to the first and last segments for $sample") {
        if (sample.reference.isEmpty) fail("Invalid test case: expected sequence must be non-empty.")
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
}
