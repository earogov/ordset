package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.SegmentSeqAssertions._
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

trait InspectionBehaviors[E, D <: Domain[E], V] {
  this: AnyFunSpec =>

  import ordset._
  import ordset.core._

  import scala.annotation.tailrec

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

  def segmentsSupportContains(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentContainsTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>

      it(s"should define whether elements and bounds belong to segment for $sample") {
        sample.containsSeq.foreach { testCase =>

          val seg = sample.sequence.getSegment(testCase.bound)
          testCase.includedBounds.foreach { bound =>

            assert(seg.containsExtended(bound), s"expected $seg contains extended bound $bound")

            bound match {
              case bound: Bound[E] =>
                assert(seg.containsBound(bound), s"expected $seg contains extended bound $bound")

                val element = bound.element
                if (shouldContainElement(bound, seg))
                  assert(seg.containsElement(element), s"expected $seg contains element $element")
                else
                  assert(!seg.containsElement(element), s"expected $seg doesn't contain element $element")

              case _ => // no additional checks for unlimited bound
            }
          }
          testCase.excludedBounds.foreach { bound =>

            assert(!seg.containsExtended(bound), s"expected $seg doesn't contain extended bound $bound")

            bound match {
              case bound: Bound[E] =>
                assert(!seg.containsBound(bound), s"expected $seg doesn't contain extended bound $bound")

                val element = bound.element
                if (shouldNotContainElement(bound, seg))
                  assert(!seg.containsElement(element), s"expected $seg doesn't contain element $element")
                else
                  assert(seg.containsElement(element), s"expected $seg contains element $element")

              case _ => // no additional checks for unlimited bound
            }
          }
        }
      }

      /**
       * Precondition:
       * {{{
       *  segment.contains(bound) == true
       * }}}
       *
       * @return `true` if `segment` should contain element of `bound`.
       */
      def shouldContainElement(bound: Bound[E], segment: Segment[E, D, V]) =
        if (bound.isInclusive) true
        else bound match {
          //    )5
          // (-----)
          // => segment contains 5
          case bound: Bound.Upper[E] if !segment.hasUpperBound(bound) => true
          //    5(
          // (-----)
          // => segment contains 5
          case bound: Bound.Lower[E] if !segment.hasLowerBound(bound) => true
          //       )5   5(
          // (-----)     (-----)
          // => segment doesn't contain 5
          case _ => false
        }

      /**
       * Precondition:
       * {{{
       *  segment.contains(bound) == false
       * }}}
       *
       * @return `true` if `segment` should contain element of `bound`.
       */
      def shouldNotContainElement(bound: Bound[E], segment: Segment[E, D, V]) =
        if (bound.isInclusive) true
        else bound match {
          //  )5
          //      [-----]
          // => segment contains 5
          case bound: Bound.Upper[E] if !segment.hasLowerBound(bound.flipUpper) => true
          //          5(
          // [-----]
          // => segment contains 5
          case bound: Bound.Lower[E] if !segment.hasUpperBound(bound.flipLower) => true
          //       5(   )5
          // [-----]     [-----]
          // => segment doesn't contain 5
          case _ => false
        }
    }

}
