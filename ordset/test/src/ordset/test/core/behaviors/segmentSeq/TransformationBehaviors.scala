package ordset.test.core.behaviors.segmentSeq

import ordset.Hash
import ordset.core.value.ValueOps
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.{Bound, ExtendedBound, Segment, SegmentSeq}
import org.scalatest.funspec.AnyFunSpec
import ordset.test.core.SegmentSeqAssertions._
import ordset.test.core.behaviors.SegmentSeqValidator
import ordset.test.core.samples.segmentSeq.SegmentSeqSample

trait TransformationBehaviors[E, D[X] <: Domain[X], V] {
  this: AnyFunSpec =>

  def segmentSeqCanBePrepended(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentSeqPrependTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>
      sample.prependCases.foreach { testCase =>

        implicit val domainOps: DomainOps[E, D] = sample.domainOps
        implicit val valueOps: ValueOps[V] = sample.valueOps

        it(s"should prepend $testCase below $sample") {
          val actual = sample.sequence.prepend(testCase.otherSeq)
          assertSameRelationAndSegmentSeq(testCase.expected, actual)
        }
      }
    }
  
  def segmentSeqCanBePrependedBelowBound(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentSeqPrependTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>
      sample.prependBelowBoundCases.foreach { testCase =>
        
        implicit val domainOps: DomainOps[E, D] = sample.domainOps
        implicit val valueOps: ValueOps[V] = sample.valueOps

        val seqValidator = new SegmentSeqValidator(sample.sequence)

        it(s"should prepend $testCase below bound ${testCase.bound} of $sample") {
          // `sequence.prependBelowBound(bound, otherSeq)` should be correct
          val actual1 = seqValidator.prependBelowExtended(testCase.bound, testCase.otherSeq)
          assertSameRelationAndSegmentSeq(testCase.expected, actual1)

          // `sequence.prependBelowBound(bound, otherSeq)` should equals to
          // `sequence.prependBelowBound(bound.flip, otherSeq)`
          testCase.bound match {
            case bound: Bound[E] =>
              val actual2 = seqValidator.prependBelowExtended(bound.flip, testCase.otherSeq)
              assertSameRelationAndSegmentSeq(testCase.expected, actual2)
            case _ =>
              // no additional checks
          }

          // `sequence.prependBelowBound(bound, otherSeq)` should equals to
          // `otherSeq.appendAboveBound(bound, sequence)`
          val actual3 = new SegmentSeqValidator(testCase.otherSeq).appendAboveExtended(testCase.bound, sample.sequence)
          assertSameRelationAndSegmentSeq(testCase.expected, actual3)

          // 1. If `segment` has previous segment:
          // `segment.prepend(otherSeq)` should equals to `sequence.prependBelowBound(segment.lower, otherSeq)`
          // 2. If `segment` is first:
          // `segment.prepend(otherSeq)` should equals to `sequence`.
          val boundSegment = sample.sequence.getSegmentForExtended(testCase.bound)
          val actual4 = boundSegment.prepend(testCase.otherSeq)
          boundSegment match {
            case s: Segment.WithPrev[E, D, V] =>
              val expected = seqValidator.prependBelowExtended(s.lower, testCase.otherSeq)
              assertSameSegmentSeq(expected, actual4)
            case _ =>
              assertSameSegmentSeq(sample.sequence, actual4)
          }

          // `segment.truncation(bound).prepend(otherSeq)` should equals to
          // `sequence.prependBelowBound(bound, otherSeq)`
          val actual5 = boundSegment.truncation(testCase.bound).prepend(testCase.otherSeq)
          assertSameRelationAndSegmentSeq(testCase.expected, actual5)

          // `segment.truncation(bound.flip).prepend(otherSeq)` should equals to
          // `sequence.prependBelowBound(bound, otherSeq)`
          testCase.bound match {
            case bound: Bound[E] =>
              val actual6 = boundSegment.truncation(bound.flip).prepend(testCase.otherSeq)
              assertSameRelationAndSegmentSeq(testCase.expected, actual6)
            case _ =>
              // no additional checks
          }

          // 1. If `segment` has previous segment:
          // `segment.truncation(segment.lower.flip).prepend(otherSeq)` should equals to
          // `segment.prepend(otherSeq)`
          //
          //      boundSegment.lower.flip
          //          v
          // ---------](-----------------](---------
          //               boundSegment
          //
          // 2. If `segment` has next segment:
          // `segment.truncation(segment.upper.flip).prepend(otherSeq)` should equals to
          // `segment.moveNext.prepend(otherSeq)`
          //
          //      boundSegment.upper.flip
          //                              v
          // ---------](-----------------](---------
          //               boundSegment
          boundSegment match {
            case s: Segment.WithPrev[E, D, V] =>
              val actual7 = s.truncation(s.lower.flip).prepend(testCase.otherSeq)
              assertSameSegmentSeq(actual4, actual7)

            case s: Segment.WithNext[E, D, V] =>
              val actual7 = s.truncation(s.upper.flip).prepend(testCase.otherSeq)
              val expected7 = s.moveNext.prepend(testCase.otherSeq)
              assertSameSegmentSeq(expected7, actual7)

            case _ => // nothing to do
          }
        }
      }
    }

  def segmentSeqCanBeAppended(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentSeqAppendTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>
      sample.appendCases.foreach { testCase =>

        implicit val domainOps: DomainOps[E, D] = sample.domainOps
        implicit val valueOps: ValueOps[V] = sample.valueOps

        it(s"should append $testCase above $sample") {
          val actual = sample.sequence.append(testCase.otherSeq)
          assertSameRelationAndSegmentSeq(testCase.expected, actual)
        }
      }
    }
  
  def segmentSeqCanBeAppendedAboveBound(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentSeqAppendTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>
      sample.appendAboveBoundCases.foreach { testCase =>

        implicit val domainOps: DomainOps[E, D] = sample.domainOps
        implicit val valueOps: ValueOps[V] = sample.valueOps

        val seqValidator = new SegmentSeqValidator(sample.sequence)

        it(s"should append $testCase above bound ${testCase.bound} of $sample") {
          // `sequence.appendAboveBound(bound, otherSeq)` should be correct
          val actual1 = seqValidator.appendAboveExtended(testCase.bound, testCase.otherSeq)
          assertSameRelationAndSegmentSeq(testCase.expected, actual1)

          // `sequence.appendAboveBound(bound, otherSeq)` should equals to
          // `sequence.appendAboveBound(bound.flip, otherSeq)`
          testCase.bound match {
            case bound: Bound[E] =>
              val actual2 = seqValidator.appendAboveExtended(bound.flip, testCase.otherSeq)
              assertSameRelationAndSegmentSeq(testCase.expected, actual2)
            case _ =>
              // no additional checks
          }

          // `sequence.appendAboveBound(bound, otherSeq)` should equals to
          // `otherSeq.prependBelowBound(bound, sequence)`
          val actual3 = new SegmentSeqValidator(testCase.otherSeq).prependBelowExtended(testCase.bound, sample.sequence)
          assertSameRelationAndSegmentSeq(testCase.expected, actual3)

          // 1. If `segment` has next segment:
          // `segment.append(otherSeq)` should equals to `sequence.appendAboveBound(segment.upper, otherSeq)`
          // 2. If `segment` is last:
          // `segment.append(otherSeq)` should equals to `sequence`.
          val boundSegment = sample.sequence.getSegmentForExtended(testCase.bound)
          val actual4 = boundSegment.append(testCase.otherSeq)
          boundSegment match {
            case s: Segment.WithNext[E, D, V] =>
              val expected = seqValidator.appendAboveExtended(s.upper, testCase.otherSeq)
              assertSameSegmentSeq(expected, actual4)
            case _ =>
              assertSameSegmentSeq(sample.sequence, actual4)
          }

          // `segment.truncation(bound).append(otherSeq)` should equals to
          // `sequence.appendAboveBound(bound, otherSeq)`
          val actual5 = boundSegment.truncation(testCase.bound).append(testCase.otherSeq)
          assertSameRelationAndSegmentSeq(testCase.expected, actual5)

          // `segment.truncation(bound.flip).append(otherSeq)` should equals to
          // `sequence.appendAboveBound(bound, otherSeq)`
          testCase.bound match {
            case bound: Bound[E] =>
              val actual6 = boundSegment.truncation(bound.flip).append(testCase.otherSeq)
              assertSameRelationAndSegmentSeq(testCase.expected, actual6)
            case _ =>
              // no additional checks
          }

          // 1. If `segment` has previous segment:
          // `segment.truncation(segment.lower.flip).append(otherSeq)` should equals to
          // `segment.movePrev.append(otherSeq)`
          //
          //      boundSegment.lower.flip
          //          v
          // ---------](-----------------](---------
          //               boundSegment
          //
          // 2. If `segment` has next segment:
          // `segment.truncation(segment.upper.flip).append(otherSeq)` should equals to
          // `segment.append(otherSeq)`
          //
          //      boundSegment.upper.flip
          //                              v
          // ---------](-----------------](---------
          //               boundSegment
          boundSegment match {
            case s: Segment.WithPrev[E, D, V] =>
              val actual7 = s.truncation(s.lower.flip).append(testCase.otherSeq)
              val expected7 = s.movePrev.append(testCase.otherSeq)
              assertSameSegmentSeq(expected7, actual7)

            case s: Segment.WithNext[E, D, V] =>
              val actual7 = s.truncation(s.upper.flip).append(testCase.otherSeq)
              assertSameSegmentSeq(actual4, actual7)

            case _ => // nothing to do
          }
        }
      }
    }

  def segmentSeqCanBeSliced(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentSeqSliceTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>
      sample.sliceCases.foreach { testCase =>

        implicit val domainOps: DomainOps[E, D] = sample.domainOps
        implicit val valueOps: ValueOps[V] = sample.valueOps

        val seqValidator = new SegmentSeqValidator(sample.sequence)

        it(s"should slice $sample at bound ${testCase.bound}") {
          // `sequence.takeBelowBound(bound)` should be correct
          val actualBelow1 = seqValidator.takeBelowExtended(testCase.bound)
          assertSameRelationAndSegmentSeq(testCase.expectedBelow, actualBelow1)

          // `sequence.takeAboveBound(bound)` should be correct
          val actualAbove1 = seqValidator.takeAboveExtended(testCase.bound)
          assertSameRelationAndSegmentSeq(testCase.expectedAbove, actualAbove1)

          // `sequence.takeBelowBound(bound).appendAboveBound(bound, sequence.takeAboveBound(bound))`
          // should equals to `sequence`
          val actualSeq1 = actualBelow1.appendAboveExtended(testCase.bound, actualAbove1)
          assertSameSegmentSeq(sample.sequence, actualSeq1)

          // `sequence.sliceAtBound(bound)` should be correct
          val actualSliced1 = seqValidator.sliceAtExtended(testCase.bound)
          assertSameRelationAndSegmentSeq(testCase.expectedBelow, actualSliced1._1)
          assertSameRelationAndSegmentSeq(testCase.expectedAbove, actualSliced1._2)

          // `segment.takeBelow` should be correct
          val actualBelow2 = sample.sequence.getSegmentForExtended(testCase.bound).takeBelow
          assertSameRelationAndSegmentSeq(testCase.expectedBelow, actualBelow2)

          // `segment.takeAbove` should be correct
          val actualAbove2 = sample.sequence.getSegmentForExtended(testCase.bound).takeAbove
          assertSameRelationAndSegmentSeq(testCase.expectedAbove, actualAbove2)

          // `segment.slice` should be correct
          val actualSliced2 = sample.sequence.getSegmentForExtended(testCase.bound).slice
          assertSameRelationAndSegmentSeq(testCase.expectedBelow, actualSliced2._1)
          assertSameRelationAndSegmentSeq(testCase.expectedAbove, actualSliced2._2)
        }
      }
    }
    
  def segmentCanBePatched(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentPatchTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>
      sample.patchCases.foreach { testCase =>

        implicit val domainOps: DomainOps[E, D] = sample.domainOps
        implicit val valueOps: ValueOps[V] = sample.valueOps

        it(s"should patch segment of $sample at bound ${testCase.bound} with $testCase") {
          val actual = sample.sequence.getSegmentForBound(testCase.bound).patch(testCase.patch)
          assertSameRelationAndSegmentSeq(testCase.expected, actual)
        }
      }
    }
}
