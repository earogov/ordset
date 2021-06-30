package test.ordset.core.behaviors.segmentSeq

import ordset.Hash
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.{Segment, SegmentSeq}
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.SegmentSeqAssertions._
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

trait TransformationBehaviors[E, D <: Domain[E], V] {
  this: AnyFunSpec =>

  def segmentSeqCanBePrepended(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentSeqPrependedTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>
      sample.prependedCases.foreach { testCase =>

        implicit val domainOps: DomainOps[E, D] = sample.domainOps
        implicit val valueHash: Hash[V] = sample.valueOps.valueHash

        it(s"should prepend $testCase to $sample") {
          val actual = sample.sequence.prepended(testCase.prepended)
          assertSameRelationAndSegmentSeq(testCase.expected, actual)
        }
      }
    }
  
  def segmentSeqCanBePrependedWithBound(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentSeqPrependedTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>
      sample.prependedWithBoundCases.foreach { testCase =>
        
        implicit val domainOps: DomainOps[E, D] = sample.domainOps
        implicit val valueHash: Hash[V] = sample.valueOps.valueHash
        
        it(s"should prepend $testCase to $sample before bound ${testCase.bound}") {
          // `sequence.prepended(bound, otherSeq)` should be correct
          val actual1 = sample.sequence.prepended(testCase.bound, testCase.prepended)
          assertSameRelationAndSegmentSeq(testCase.expected, actual1)

          // `sequence.prepended(bound, otherSeq)` should equals to `sequence.prepended(bound.flip, otherSeq)`
          val actual2 = sample.sequence.prepended(testCase.bound.flip, testCase.prepended)
          assertSameRelationAndSegmentSeq(testCase.expected, actual2)

          // `sequence.prepended(bound, otherSeq)` should equals to `otherSeq.appended(bound, sequence)`
          val actual3 = testCase.prepended.appended(testCase.bound, sample.sequence)
          assertSameRelationAndSegmentSeq(testCase.expected, actual3)

          // 1. If `segment` has previous segment:
          // `segment.prepended(otherSeq)` should equals to `sequence.prepended(segment.lowerBound, otherSeq)`
          // 2. If `segment` is first:
          // `segment.prepended(otherSeq)` should equals to `sequence`.
          val boundSegment = sample.sequence.getSegment(testCase.bound)
          val actual4 = boundSegment.prepended(testCase.prepended)
          boundSegment match {
            case s: Segment.WithPrev[E, D, V] =>
              val expected = sample.sequence.prepended(s.lowerBound, testCase.prepended)
              assertSameSegmentSeq(expected, actual4)
            case _ =>
              assertSameSegmentSeq(sample.sequence, actual4)
          }

          // `segment.truncation(bound).prepended(otherSeq)` should equals to `sequence.prepended(bound, otherSeq)`
          val actual5 = boundSegment.truncation(testCase.bound).prepended(testCase.prepended)
          assertSameRelationAndSegmentSeq(testCase.expected, actual5)

          // `segment.truncation(bound.flip).prepended(otherSeq)` should equals to `sequence.prepended(bound, otherSeq)`
          val actual6 = boundSegment.truncation(testCase.bound.flip).prepended(testCase.prepended)
          assertSameRelationAndSegmentSeq(testCase.expected, actual6)

          // 1. If `segment` has previous segment:
          // `segment.truncation(segment.lowerBound.flip).prepended(otherSeq)` should equals to
          // `segment.prepended(otherSeq)`
          //
          //      boundSegment.lowerBound.flip
          //          v
          // ---------](-----------------](---------
          //               boundSegment
          //
          // 2. If `segment` has next segment:
          // `segment.truncation(segment.upperBound.flip).prepended(otherSeq)` should equals to
          // `segment.moveNext.prepended(otherSeq)`
          //
          //      boundSegment.upperBound.flip
          //                              v
          // ---------](-----------------](---------
          //               boundSegment
          boundSegment match {
            case s: Segment.WithPrev[E, D, V] =>
              val actual7 = s.truncation(s.lowerBound.flip).prepended(testCase.prepended)
              assertSameSegmentSeq(actual4, actual7)

            case s: Segment.WithNext[E, D, V] =>
              val actual7 = s.truncation(s.upperBound.flip).prepended(testCase.prepended)
              val expected7 = s.moveNext.prepended(testCase.prepended)
              assertSameSegmentSeq(expected7, actual7)

            case _ => // nothing to do
          }
        }
      }
    }

  def segmentSeqCanBeAppended(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentSeqAppendedTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>
      sample.appendedCases.foreach { testCase =>

        implicit val domainOps: DomainOps[E, D] = sample.domainOps
        implicit val valueHash: Hash[V] = sample.valueOps.valueHash

        it(s"should append $testCase to $sample") {
          val actual = sample.sequence.appended(testCase.appended)
          assertSameRelationAndSegmentSeq(testCase.expected, actual)
        }
      }
    }
  
  def segmentSeqCanBeAppendedWithBound(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentSeqAppendedTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>
      sample.appendedWithBoundCases.foreach { testCase =>

        implicit val domainOps: DomainOps[E, D] = sample.domainOps
        implicit val valueHash: Hash[V] = sample.valueOps.valueHash

        it(s"should append $testCase to $sample after bound ${testCase.bound}") {
          // `sequence.appended(bound, otherSeq)` should be correct
          val actual1 = sample.sequence.appended(testCase.bound, testCase.appended)
          assertSameRelationAndSegmentSeq(testCase.expected, actual1)

          // `sequence.appended(bound, otherSeq)` should equals to `sequence.appended(bound.flip, otherSeq)`
          val actual2 = sample.sequence.appended(testCase.bound.flip, testCase.appended)
          assertSameRelationAndSegmentSeq(testCase.expected, actual2)

          // `sequence.appended(bound, otherSeq)` should equals to `otherSeq.prepended(bound, sequence)`
          val actual3 = testCase.appended.prepended(testCase.bound, sample.sequence)
          assertSameRelationAndSegmentSeq(testCase.expected, actual3)

          // 1. If `segment` has next segment:
          // `segment.appended(otherSeq)` should equals to `sequence.appended(segment.upperBound, otherSeq)`
          // 2. If `segment` is last:
          // `segment.appended(otherSeq)` should equals to `sequence`.
          val boundSegment = sample.sequence.getSegment(testCase.bound)
          val actual4 = boundSegment.appended(testCase.appended)
          boundSegment match {
            case s: Segment.WithNext[E, D, V] =>
              val expected = sample.sequence.appended(s.upperBound, testCase.appended)
              assertSameSegmentSeq(expected, actual4)
            case _ =>
              assertSameSegmentSeq(sample.sequence, actual4)
          }

          // `segment.truncation(bound).appended(otherSeq)` should equals to `sequence.appended(bound, otherSeq)`
          val actual5 = boundSegment.truncation(testCase.bound).appended(testCase.appended)
          assertSameRelationAndSegmentSeq(testCase.expected, actual5)

          // `segment.truncation(bound.flip).appended(otherSeq)` should equals to `sequence.appended(bound, otherSeq)`
          val actual6 = boundSegment.truncation(testCase.bound.flip).appended(testCase.appended)
          assertSameRelationAndSegmentSeq(testCase.expected, actual6)

          // 1. If `segment` has previous segment:
          // `segment.truncation(segment.lowerBound.flip).appended(otherSeq)` should equals to
          // `segment.movePrev.appended(otherSeq)`
          //
          //      boundSegment.lowerBound.flip
          //          v
          // ---------](-----------------](---------
          //               boundSegment
          //
          // 2. If `segment` has next segment:
          // `segment.truncation(segment.upperBound.flip).appended(otherSeq)` should equals to
          // `segment.appended(otherSeq)`
          //
          //      boundSegment.upperBound.flip
          //                              v
          // ---------](-----------------](---------
          //               boundSegment
          boundSegment match {
            case s: Segment.WithPrev[E, D, V] =>
              val actual7 = s.truncation(s.lowerBound.flip).appended(testCase.appended)
              val expected7 = s.movePrev.appended(testCase.appended)
              assertSameSegmentSeq(expected7, actual7)

            case s: Segment.WithNext[E, D, V] =>
              val actual7 = s.truncation(s.upperBound.flip).appended(testCase.appended)
              assertSameSegmentSeq(actual4, actual7)

            case _ => // nothing to do
          }
        }
      }
    }

  def segmentSeqCanBeSliced(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentSeqSlicedTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>
      sample.slicedCases.foreach { testCase =>

        implicit val domainOps: DomainOps[E, D] = sample.domainOps
        implicit val valueHash: Hash[V] = sample.valueOps.valueHash

        it(s"should slice $sample at bound ${testCase.bound}") {
          // `sequence.takenBelow(bound)` should be correct
          val actualBelow1 = sample.sequence.takenBelow(testCase.bound)
          assertSameRelationAndSegmentSeq(testCase.expectedBelow, actualBelow1)

          // `sequence.takenAbove(bound)` should be correct
          val actualAbove1 = sample.sequence.takenAbove(testCase.bound)
          assertSameRelationAndSegmentSeq(testCase.expectedAbove, actualAbove1)

          // `sequence.takenBelow(bound).appended(bound, sequence.takenAbove(bound))` should equals to `sequence`
          val actualSeq1 = actualBelow1.appended(testCase.bound, actualAbove1)
          assertSameSegmentSeq(sample.sequence, actualSeq1)

          // `sequence.sliced(bound)` should be correct
          val actualSliced1 = sample.sequence.sliced(testCase.bound)
          assertSameRelationAndSegmentSeq(testCase.expectedBelow, actualSliced1._1)
          assertSameRelationAndSegmentSeq(testCase.expectedAbove, actualSliced1._2)

          // `segment.takenBelow` should be correct
          val actualBelow2 = sample.sequence.getSegment(testCase.bound).takenBelow
          assertSameRelationAndSegmentSeq(testCase.expectedBelow, actualBelow2)

          // `segment.takenAbove` should be correct
          val actualAbove2 = sample.sequence.getSegment(testCase.bound).takenAbove
          assertSameRelationAndSegmentSeq(testCase.expectedAbove, actualAbove2)

          // `segment.sliced` should be correct
          val actualSliced2 = sample.sequence.getSegment(testCase.bound).sliced
          assertSameRelationAndSegmentSeq(testCase.expectedBelow, actualSliced2._1)
          assertSameRelationAndSegmentSeq(testCase.expectedAbove, actualSliced2._2)
        }
      }
    }
    
  def segmentCanBePatched(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]] with SegmentPatchedTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>
      sample.patchedCases.foreach { testCase =>

        implicit val domainOps: DomainOps[E, D] = sample.domainOps
        implicit val valueHash: Hash[V] = sample.valueOps.valueHash

        it(s"should patch segment of $sample at bound ${testCase.bound} with $testCase") {
          val actual = sample.sequence.getSegment(testCase.bound).patched(testCase.patch)
          assertSameRelationAndSegmentSeq(testCase.expected, actual)
        }
      }
    }
}
