package test.ordset.core.behaviors.segmentSeq

import ordset.Hash
import ordset.core.domain.Domain
import ordset.core.Segment
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

trait TransformationBehaviors[E, D <: Domain[E], V]
  extends SegmentSeqAssert[E, D, V] { this: AnyFunSpec =>

  def segmentSeqCanBePrepended(
    samples: Iterable[SegmentSeqSample[E, D, V] with SegmentSeqPrependedTest[E, D, V]]
  )(
    implicit valueHash: Hash[V]
  ): Unit =
    samples.foreach { sample =>
      sample.prependedCases.foreach { prependedCase =>

        it(s"should prepend $prependedCase to $sample before bound ${prependedCase.bound}") {
          // `sequence.prepended(bound, otherSeq)` should be correct
          val actual1 = sample.sequence.prepended(prependedCase.bound, prependedCase.prepended)
          assertEqualSequences(prependedCase.expected, actual1)(sample.domainOps, valueHash)

          // `sequence.prepended(bound, otherSeq)` should equals to `sequence.prepended(bound.flip, otherSeq)`
          val actual2 = sample.sequence.prepended(prependedCase.bound.flip, prependedCase.prepended)
          assertEqualSequences(prependedCase.expected, actual2)(sample.domainOps, valueHash)

//          // `segment.prepended(otherSeq)` should be correct
//          val boundSegment = sample.sequence.getSegment(prependedCase.bound)
//          val actual3 = boundSegment.prepended(prependedCase.prepended)
//          boundSegment match {
//            case s: Segment.WithPrev[E, D, V] =>
//              val expected = sample.sequence.prepended(s.lowerBound, prependedCase.prepended)
//              assertEqualSequences(expected, actual3)(sample.domainOps, valueHash)
//            case _ =>
//              assertEqualSequences(sample.sequence, actual3)(sample.domainOps, valueHash)
//          }
        }
      }
    }

  def segmentSeqCanBeAppended(
    samples: Iterable[SegmentSeqSample[E, D, V] with SegmentSeqAppendedTest[E, D, V]]
  )(
    implicit valueHash: Hash[V]
  ): Unit =
    samples.foreach { sample =>
      sample.appendedCases.foreach { appendedCase =>

        it(s"should append $appendedCase to $sample after bound ${appendedCase.bound}") {
          // `sequence.appended(bound, otherSeq)` should be correct
          val actual1 = sample.sequence.appended(appendedCase.bound, appendedCase.appended)
          assertEqualSequences(appendedCase.expected, actual1)(sample.domainOps, valueHash)

          // `sequence.appended(bound, otherSeq)` should equals to `sequence.appended(bound.flip, otherSeq)`
          val actual2 = sample.sequence.appended(appendedCase.bound.flip, appendedCase.appended)
          assertEqualSequences(appendedCase.expected, actual2)(sample.domainOps, valueHash)

          // `segment.appended(otherSeq)` should be correct
          val boundSegment = sample.sequence.getSegment(appendedCase.bound)
          val actual3 = boundSegment.appended(appendedCase.appended)
          boundSegment match {
            case s: Segment.WithNext[E, D, V] =>
              val expected = sample.sequence.appended(s.upperBound, appendedCase.appended)
              assertEqualSequences(expected, actual3)(sample.domainOps, valueHash)
            case _ =>
              assertEqualSequences(sample.sequence, actual3)(sample.domainOps, valueHash)
          }
        }
      }
    }

  def segmentSeqCanBeSliced(
    samples: Iterable[SegmentSeqSample[E, D, V] with SegmentSeqSlicedTest[E, D, V]]
  )(
    implicit valueHash: Hash[V]
  ): Unit =
    samples.foreach { sample =>
      sample.slicedCases.foreach { slicedCase =>

        it(s"should slice $sample at bound ${slicedCase.bound}") {
          // `sequence.takenBelow(bound)` should be correct
          val actualBelow1 = sample.sequence.takenBelow(slicedCase.bound)
          assertEqualSequences(slicedCase.expectedBelow, actualBelow1)(sample.domainOps, valueHash)

          // `sequence.takenAbove(bound)` should be correct
          val actualAbove1 = sample.sequence.takenAbove(slicedCase.bound)
          assertEqualSequences(slicedCase.expectedAbove, actualAbove1)(sample.domainOps, valueHash)

          // `sequence.sliced(bound)` should be correct
          val actualSliced1 = sample.sequence.sliced(slicedCase.bound)
          assertEqualSequences(slicedCase.expectedBelow, actualSliced1._1)(sample.domainOps, valueHash)
          assertEqualSequences(slicedCase.expectedAbove, actualSliced1._2)(sample.domainOps, valueHash)
          
          // `segment.takenBelow` should be correct
          val actualBelow2 = sample.sequence.getSegment(slicedCase.bound).takenBelow
          assertEqualSequences(slicedCase.expectedBelow, actualBelow2)(sample.domainOps, valueHash)

          // `segment.takenAbove` should be correct
          val actualAbove2 = sample.sequence.getSegment(slicedCase.bound).takenAbove
          assertEqualSequences(slicedCase.expectedAbove, actualAbove2)(sample.domainOps, valueHash)

          // `segment.sliced` should be correct
          val actualSliced2 = sample.sequence.getSegment(slicedCase.bound).sliced
          assertEqualSequences(slicedCase.expectedBelow, actualSliced2._1)(sample.domainOps, valueHash)
          assertEqualSequences(slicedCase.expectedAbove, actualSliced2._2)(sample.domainOps, valueHash)
        }
      }
    }
    
  def segmentCanBePatched(
    samples: Iterable[SegmentSeqSample[E, D, V] with SegmentPatchedTest[E, D, V]]
  )(
    implicit valueHash: Hash[V]
  ): Unit =
    samples.foreach { sample =>
      sample.patchedCases.foreach { patchedCase =>
        
        it(s"should patch segment of $sample at bound ${patchedCase.bound} with $patchedCase") {
          val actual = sample.sequence.getSegment(patchedCase.bound).patched(patchedCase.patch)
          assertEqualSequences(patchedCase.expected, actual)(sample.domainOps, valueHash)
        }
      }
    }
}
