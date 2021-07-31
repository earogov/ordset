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
      val extendedOrd = sample.domainOps.extendedOrd
      val segmentOrd = sample.domainOps.segmentUpperOrd
  
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
              seg.hasNextSuchThat(segmentOrd.eqv(_, seg.moveNext)),
              s"expected $seg has next segment ${seg.moveNext}"
            )
            assert(
              !seg.hasNextSuchThat(_ => false),
              s"expected $seg doesn't have next segment that satisfies predicate (_) => false"
            )
            assert(
              seg.hasPrevSuchThat(segmentOrd.eqv(_, seg.movePrev)),
              s"expected $seg has previous segment ${seg.movePrev}"
            )
            assert(
              !seg.hasPrevSuchThat(_ => false),
              s"expected $seg doesn't have previous segment that satisfies predicate (_) => false"
            )

            assert(
              seg.hasLowerBound(seg.lowerBound),
              s"expected $seg has lower bound ${seg.lowerBound}"
            )
            assert(
              !seg.hasLowerBound(seg.upperBound.flipUpper),
              s"expected $seg doesn't have lower bound ${seg.upperBound.flipUpper}"
            )
            assert(
              seg.hasLowerExtended(seg.lowerBound),
              s"expected $seg has lower extended bound ${seg.lowerBound}"
            )
            assert(
              !seg.hasLowerExtended(ExtendedBound.BelowAll),
              s"expected $seg doesn't have lower extended bound ${ExtendedBound.BelowAll}"
            )
            assert(
              seg.hasUpperBound(seg.upperBound),
              s"expected $seg has upper bound ${seg.upperBound}"
            )
            assert(
              !seg.hasUpperBound(seg.lowerBound.flipLower),
              s"expected $seg doesn't have upper bound ${seg.lowerBound.flipLower}"
            )
            assert(
              seg.hasUpperExtended(seg.upperBound),
              s"expected $seg has upper extended bound ${seg.upperBound}"
            )
            assert(
              !seg.hasUpperExtended(ExtendedBound.AboveAll),
              s"expected $seg doesn't have upper extended bound ${ExtendedBound.AboveAll}"
            )
            sample.moveToBoundCases.foreach { s =>
              s._1 match {
                case bound: Bound[E] =>
                  if (
                    boundOrd.lt(bound.provideLower, seg.lowerBound) ||
                    boundOrd.gt(bound.provideUpper, seg.upperBound)
                  ) {
                    assertSegmentDoesNotHaveBounds(seg, bound)
                  }
                case _ => // skip extended bounds
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
              seg.hasNextSuchThat(segmentOrd.eqv(_, seg.moveNext)),
              s"expected $seg has next segment ${seg.moveNext}"
            )
            assert(
              !seg.hasNextSuchThat(_ => false),
              s"expected $seg doesn't have next segment that satisfies predicate (_) => false"
            )
            assert(
              !seg.hasPrevSuchThat(_ => true),
              s"expected $seg doesn't have previous segment that satisfies predicate (_) => true"
            )

            assert(
              seg.hasUpperBound(seg.upperBound),
              s"expected $seg has upper bound ${seg.upperBound}"
            )
            assert(
              seg.hasUpperExtended(seg.upperBound),
              s"expected $seg has upper extended bound ${seg.upperBound}"
            )
            assert(
              seg.hasLowerExtended(ExtendedBound.BelowAll),
              s"expected $seg has lower extended bound ${ExtendedBound.BelowAll}"
            )
            assert(
              !seg.hasUpperExtended(ExtendedBound.AboveAll),
              s"expected $seg doesn't have upper extended bound ${ExtendedBound.AboveAll}"
            )
            sample.moveToBoundCases.foreach { s =>
              s._1 match {
                case bound: Bound[E] =>
                  if (boundOrd.gt(bound.provideUpper, seg.upperBound)) {
                    assertSegmentDoesNotHaveBounds(seg, bound)
                  }
                case _ => // skip extended bounds
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
              !seg.hasNextSuchThat(_ => true),
              s"expected $seg doesn't have next segment that satisfies predicate (_) => true"
            )
            assert(
              seg.hasPrevSuchThat(segmentOrd.eqv(_, seg.movePrev)),
              s"expected $seg has previous segment ${seg.movePrev}"
            )
            assert(
              !seg.hasPrevSuchThat(_ => false),
              s"expected $seg doesn't have previous segment that satisfies predicate (_) => false"
            )

            assert(
              seg.hasLowerBound(seg.lowerBound),
              s"expected $seg has lower bound ${seg.lowerBound}"
            )
            assert(
              seg.hasLowerExtended(seg.lowerBound),
              s"expected $seg has lower extended bound ${seg.lowerBound}"
            )
            assert(
              seg.hasUpperExtended(ExtendedBound.AboveAll),
              s"expected $seg has upper extended bound ${ExtendedBound.AboveAll}"
            )
            assert(
              !seg.hasLowerExtended(ExtendedBound.BelowAll),
              s"expected $seg doesn't have lower extended bound ${ExtendedBound.BelowAll}"
            )
            sample.moveToBoundCases.foreach { s =>
              s._1 match {
                case bound: Bound[E] =>
                  if (boundOrd.lt(bound.provideLower, seg.lowerBound)) {
                    assertSegmentDoesNotHaveBounds(seg, bound)
                  }
                case _ => // skip extended bounds
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

            assert(
              !seg.hasNextSuchThat(_ => true),
              s"expected $seg doesn't have next segment that satisfies predicate (_) => true"
            )
            assert(
              !seg.hasPrevSuchThat(_ => true),
              s"expected $seg doesn't have previous segment that satisfies predicate (_) => true"
            )

            assert(
              seg.hasLowerExtended(ExtendedBound.BelowAll),
              s"expected $seg has lower extended bound ${ExtendedBound.BelowAll}"
            )
            assert(
              seg.hasUpperExtended(ExtendedBound.AboveAll),
              s"expected $seg has upper extended bound ${ExtendedBound.AboveAll}"
            )

            sample.moveToBoundCases.foreach { s =>
              s._1 match {
                case bound: Bound[E] => assertSegmentDoesNotHaveBounds(seg, bound)
                case _ => // skip extended bounds
              }
            }
  
          case _ => sys.error("Unexpected case")
        }

        def assertSegmentDoesNotHaveBounds(seg: Segment[E, D, V], bound: Bound[E]): Unit = {
          assert(
            !seg.hasLowerBound(bound.provideLower),
            s"expected $seg doesn't have lower bound ${bound.provideLower}"
          )
          assert(
            !seg.hasLowerExtended(bound.provideLower),
            s"expected $seg doesn't have lower extended bound ${bound.provideLower}"
          )
          assert(
            !seg.hasUpperBound(bound.provideUpper),
            s"expected $seg doesn't have upper bound ${bound.provideUpper}"
          )
          assert(
            !seg.hasUpperExtended(bound.provideUpper),
            s"expected $seg doesn't have upper extended bound ${bound.provideUpper}"
          )
        }
  
        loop(sample.sequence.firstSegment)
      }
    }

  def segmentsSupportContains(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentContainsTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>

      it(s"should define whether elements and bounds belong to segment for $sample") {
        sample.containsCases.foreach { testCase =>

          val seg = sample.sequence.getSegmentForBound(testCase.bound)
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

  def segmentsCanRestrictBound(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentRestrictBoundTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>

      it(s"should restrict bounds for $sample") {
        sample.restrictCases.foreach { testCase =>

          val seg = sample.sequence.getSegmentForBound(testCase.bound)
          testCase.restrictedBounds.foreach { boundTuple =>

            val input = boundTuple.input
            val actual = seg.restrictExtended(input)
            assert(
              boundTuple.expected == actual,
              s"expected segment $seg restricts extended bound $input to ${boundTuple.expected} but got $actual"
            )
            boundTuple.input match {
              case input: Bound[E] =>
                val actual = seg.restrictBound(input)
                assert(
                  boundTuple.expected == actual,
                  s"expected segment $seg restricts bound $input to ${boundTuple.expected} but got $actual"
                )
              case _ => // no additional checks
            }
          }
        }
      }
    }

  def supportReturnValueForBound(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]]]
  ): Unit =
    samples.foreach { sample =>

      implicit val valueHash: Hash[V] = sample.valueOps.valueHash

      it(s"should return value associated with segment for $sample") {

        def checkSequenceValueForBound(bound: ExtendedBound[E], refRelation: IntervalRelation[E, D, V]): Unit = {
          val refValue = refRelation.value
          val seqValue = sample.sequence.getValueForExtended(bound)
          assert(valueHash.eqv(seqValue, refValue), s"sequence should return $refValue for extended bound $bound")
          bound match {
            case bound: Bound[E] =>
              val seqValue = sample.sequence.getValueForBound(bound)
              assert(valueHash.eqv(seqValue, refValue), s"sequence should return $refValue for bound $bound")
              if (bound.isInclusive) {
                val element = bound.element
                val seqValue = sample.sequence.getValueForElement(element)
                assert(valueHash.eqv(seqValue, refValue), s"sequence should return $refValue for element $element")
              }
            case _ => // no additional checks
          }
        }

        val iterator = sample.sequence.firstSegment.forwardIterator
        sample.reference.foreach { refRelation =>

          assert(iterator.hasNext, s"no segment corresponds to reference interval relation $refRelation")

          val refValue = refRelation.value

          // `segment.value` is correct
          val segment = iterator.next()
          assert(valueHash.eqv(segment.value, refValue), s"segment $segment should have value $refValue")

          // `sequence.getValueForBound` etc are correct
          checkSequenceValueForBound(segment.lowerExtended, refRelation)
          checkSequenceValueForBound(segment.upperExtended, refRelation)
        }
      }
    }
}
