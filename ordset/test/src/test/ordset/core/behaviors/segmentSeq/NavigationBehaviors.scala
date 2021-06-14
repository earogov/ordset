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
            assert(actualSeg.contains(bound), s"expected $actualSeg contains bound $bound")
            val containsElement =
              if (bound.isInclusive) true
              else bound match {
                case bound: Bound.Upper[E] if !actualSeg.hasUpperBound(bound) => true
                case bound: Bound.Lower[E] if !actualSeg.hasLowerBound(bound) => true
                case _ => false
              }
            if (containsElement) {
              assert(actualSeg.containsElement(bound.value), s"expected $actualSeg contains element ${bound.value}")
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

  def segmentsHaveNavigationIndicators(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentMoveToBoundTest[E, D, V]]
  ): Unit = 
    samples.foreach { sample =>

      val boundOrd = sample.domainOps.boundOrd
  
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
  
            assert(
              seg.hasLowerBound(seg.lowerBound),
              s"expected $seg has lower bound ${seg.lowerBound}"
            )
            assert(
              !seg.hasLowerBound(seg.upperBound.flipUpper),
              s"expected $seg doesn't have lower bound ${seg.upperBound.flipUpper}"
            )
            assert(
              seg.hasUpperBound(seg.upperBound),
              s"expected $seg has upper bound ${seg.upperBound}"
            )
            assert(
              !seg.hasUpperBound(seg.lowerBound.flipLower),
              s"expected $seg doesn't have upper bound ${seg.lowerBound.flipLower}"
            )
            sample.moveToBoundSeq.foreach { s =>
              val bound = s._1
              if (boundOrd.lt(bound.provideLower, seg.lowerBound) || boundOrd.gt(bound.provideUpper, seg.upperBound)) {
                assert(
                  !seg.hasLowerBound(bound.provideLower),
                  s"expected $seg doesn't have lower bound ${bound.provideLower}"
                )
                assert(
                  !seg.hasUpperBound(bound.provideUpper),
                  s"expected $seg doesn't have upper bound ${bound.provideUpper}"
                )
              }
            }
  
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
  
            assert(
              seg.hasUpperBound(seg.upperBound),
              s"expected $seg has upper bound ${seg.upperBound}"
            )
            sample.moveToBoundSeq.foreach { s =>
              val bound = s._1
              if (boundOrd.gt(bound.provideUpper, seg.upperBound)) {
                assert(
                  !seg.hasLowerBound(bound.provideLower),
                  s"expected $seg doesn't have lower bound ${bound.provideLower}"
                )
                assert(
                  !seg.hasUpperBound(bound.provideUpper),
                  s"expected $seg doesn't have upper bound ${bound.provideUpper}"
                )
              }
            }
  
            loop(seg.moveNext)
  
          case seg: Segment.Terminal[E, D, V] =>
            assert(seg.isTerminal, s"expected $seg is terminal segment")
            assert(seg.isLast, s"expected $seg is last segment")
            assert(seg.hasPrev, s"expected $seg has previous segment")
            assert(!seg.isSingle, s"expected $seg is not single segment")
            assert(!seg.isInner, s"expected $seg is not inner segment")
            assert(!seg.isFirst, s"expected $seg is not first segment")
            assert(!seg.hasNext, s"expected $seg does not have next segment")
            assert(!seg.isInitial, s"expected $seg is not initial segment")
  
            assert(
              seg.hasLowerBound(seg.lowerBound),
              s"expected $seg has lower bound ${seg.lowerBound}"
            )
            sample.moveToBoundSeq.foreach { s =>
              val bound = s._1
              if (boundOrd.lt(bound.provideLower, seg.lowerBound)) {
                assert(
                  !seg.hasLowerBound(bound.provideLower),
                  s"expected $seg doesn't have lower bound ${bound.provideLower}"
                )
                assert(
                  !seg.hasUpperBound(bound.provideUpper),
                  s"expected $seg doesn't have upper bound ${bound.provideUpper}"
                )
              }
            }
  
          case seg: Segment.Single[E, D, V] =>
            assert(seg.isSingle, s"expected $seg is single segment")
            assert(seg.isFirst, s"expected $seg is first segment")
            assert(seg.isLast, s"expected $seg is last segment")
            assert(!seg.isInitial, s"expected $seg is not initial segment")
            assert(!seg.isTerminal, s"expected $seg is not terminal segment")
            assert(!seg.hasNext, s"expected $seg does not have next segment")
            assert(!seg.hasPrev, s"expected $seg does not have previous segment")
            assert(!seg.isInner, s"expected $seg is not inner segment")
            sample.moveToBoundSeq.foreach { s =>
              val bound = s._1
              assert(
                !seg.hasLowerBound(bound.provideLower),
                s"expected $seg doesn't have lower bound ${bound.provideLower}"
              )
              assert(
                !seg.hasUpperBound(bound.provideUpper),
                s"expected $seg doesn't have upper bound ${bound.provideUpper}"
              )
            }
  
          case _ => sys.error("Unexpected case")
        }
  
        loop(sample.sequence.firstSegment)
      }
    }
}
