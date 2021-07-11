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
        def loop(seg: Segment[E, D, V], moveSeq: Seq[(ExtendedBound[E], IntervalRelation[E, D, V])]): Unit =
          moveSeq match {
            case (bound, expectedRel) :: tail =>

              val actualSeg = seg.moveToExtended(bound)
              val actualRel = actualSeg.intervalRelation
              assert(
                intervalRelHash.eqv(actualRel, expectedRel),
                s"expected: $expectedRel, actual: $actualRel, extended bound: $bound"
              )

              bound match {
                case bound: Bound[E] =>
                  val actualSeg = seg.moveToBound(bound)
                  val actualRel = actualSeg.intervalRelation
                  assert(
                    intervalRelHash.eqv(actualRel, expectedRel),
                    s"expected: $expectedRel, actual: $actualRel, bound: $bound"
                  )

                  if (bound.isInclusive) {
                    val actualSeg = seg.moveToElement(bound.element)
                    val actualRel = actualSeg.intervalRelation
                    assert(
                      intervalRelHash.eqv(actualRel, expectedRel),
                      s"expected: $expectedRel, actual: $actualRel, element: ${bound.element}"
                    )
                  }

                case _ => // no additional checks
              }

              loop(actualSeg, tail)
            case _ => // end
          }

        loop(sample.sequence.firstSegment, sample.moveToBoundCases)
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
