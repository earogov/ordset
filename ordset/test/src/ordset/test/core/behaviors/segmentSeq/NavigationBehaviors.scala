package ordset.test.core.behaviors.segmentSeq

import ordset.core.domain.{Domain, DomainOps}
import org.scalatest.funspec.AnyFunSpec
import ordset.test.core.SegmentSeqAssertions
import ordset.test.core.SegmentSeqAssertions._
import ordset.test.core.samples.segmentSeq.SegmentSeqSample

trait NavigationBehaviors[E, D <: Domain[E], V] { 
  this: AnyFunSpec =>

  import ordset._
  import ordset.core._
  import ordset.core.interval._

  import scala.annotation.tailrec

  def segmentsSupportMovePrevAndNext(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]]]
  ): Unit = 
    samples.foreach { sample =>

      implicit val domainOps: DomainOps[E, D] = sample.domainOps
      implicit val valueHash: Hash[V] = sample.valueOps.valueHash
  
      it(s"should move to the next segment if there is one for $sample") {
        @tailrec
        def loop(seg: Segment[E, D, V], expected: Seq[IntervalRelation[E, D, V]]): Unit = expected match {
          case expectedRel :: tail =>
            assertSameRelationAndSegment(expectedRel, seg)
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
            assertSameRelationAndSegment(expectedRel, seg)
            seg match {
              case seg: Segment.WithPrev[E, D, V] => loop(seg.movePrev, tail)
              case _ => // end
            }
          case _ => fail("Invalid test case: expected sequence must be non-empty.")
        }
        loop(sample.sequence.lastSegment, sample.reference.reverse)
      }
    }

  def supportMoveToBound(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentMoveToBoundTest[E, D, V]]
  ): Unit = 
    samples.foreach { sample =>

      implicit val domainOps: DomainOps[E, D] = sample.domainOps
      implicit val valueHash: Hash[V] = sample.valueOps.valueHash

      it(s"segments should move to the specified bound for $sample") {
        @tailrec
        def loop(seg: Segment[E, D, V], moveSeq: Seq[(ExtendedBound[E], IntervalRelation[E, D, V])]): Unit =
          moveSeq match {
            case (bound, expectedRel) :: tail =>
              val actualSeg = seg.moveToExtended(bound)
              assertSameRelationAndSegment(expectedRel, actualSeg, s"extended bound: $bound")
              bound match {
                case bound: Bound[E] =>
                  val actualSeg = seg.moveToBound(bound)
                  assertSameRelationAndSegment(expectedRel, actualSeg, s"bound: $bound")
                  if (bound.isInclusive) {
                    val actualSeg = seg.moveToElement(bound.element)
                    assertSameRelationAndSegment(expectedRel, actualSeg, s"element: ${bound.element}")
                  }
                case _ => // no additional checks
              }
              loop(actualSeg, tail)
            case _ => // end
          }

        loop(sample.sequence.firstSegment, sample.moveToBoundCases)
      }

      it(s"sequence should return segment for the specified bound for $sample") {
        sample.moveToBoundCases.foreach { testCase =>

          val bound = testCase._1
          val expectedRel = testCase._2

          val actualSeg = sample.sequence.getSegmentForExtended(bound)
          assertSameRelationAndSegment(expectedRel, actualSeg, s"extended bound: $bound")
          bound match {
            case bound: Bound[E] =>
              val actualSeg = sample.sequence.getSegmentForBound(bound)
              assertSameRelationAndSegment(expectedRel, actualSeg, s"bound: $bound")
              if (bound.isInclusive) {
                val actualSeg = sample.sequence.getSegmentForElement(bound.element)
                assertSameRelationAndSegment(expectedRel, actualSeg, s"element: ${bound.element}")
              }
            case _ => // no additional checks
          }
        }
      }
    }

  def supportMoveToFirstAndLast(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]]]
  ): Unit = 
    samples.foreach { sample =>

      implicit val domainOps: DomainOps[E, D] = sample.domainOps
      implicit val valueHash: Hash[V] = sample.valueOps.valueHash
  
      it(s"should move to the first and last segments for $sample") {
        if (sample.reference.isEmpty) fail("Invalid test case: expected sequence must be non-empty.")
        val firstExp = sample.reference.head
        val lastExp = sample.reference.last
        @tailrec
        def loop(seg: Segment[E, D, V]): Unit = {
          assertSameRelationAndSegment(firstExp, seg.moveToFirst)
          assertSameRelationAndSegment(lastExp, seg.moveToLast)
          seg match {
            case seg: Segment.WithNext[E, D, V] => loop(seg.moveNext)
            case _ => // end
          }
        }
        loop(sample.sequence.firstSegment)
      }

      it(s"sequence should return first and last segments for $sample") {
        if (sample.reference.isEmpty) fail("Invalid test case: expected sequence must be non-empty.")
        val firstExp = sample.reference.head
        val lastExp = sample.reference.last
        assertSameRelationAndSegment(firstExp, sample.sequence.firstSegment)
        assertSameRelationAndSegment(lastExp, sample.sequence.lastSegment)
      }
    }
}
