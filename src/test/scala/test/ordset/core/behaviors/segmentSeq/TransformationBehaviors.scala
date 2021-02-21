package test.ordset.core.behaviors.segmentSeq

import ordset.Hash
import ordset.core.domain.Domain
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.samples.segmentSeq.SegmentSeqSample

trait TransformationBehaviors[E, D <: Domain[E], V]
  extends SegmentSeqAssert[E, D, V] { this: AnyFunSpec =>

  def segmentSeqCanBeAppended(
    samples: Iterable[SegmentSeqSample[E, D, V] with SegmentSeqAppendedTest[E, D, V]]
  )(
    implicit valueHash: Hash[V]
  ): Unit =
    samples.foreach { sample =>
      sample.appendedCases.foreach { appendedCase =>

        it(s"should append $appendedCase to $sample") {
          val actual = sample.sequence.appended(appendedCase.appended)
          assertEqualSequences(appendedCase.expected, actual)(sample.domainOps, valueHash)
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
          val actualBelow = sample.sequence.takenBelow(slicedCase.bound)
          assertEqualSequences(slicedCase.expectedBelow, actualBelow)(sample.domainOps, valueHash)

          val actualAbove = sample.sequence.takenAbove(slicedCase.bound)
          assertEqualSequences(slicedCase.expectedAbove, actualAbove)(sample.domainOps, valueHash)

          val actualSliced = sample.sequence.sliced(slicedCase.bound)
          assertEqualSequences(slicedCase.expectedBelow, actualSliced._1)(sample.domainOps, valueHash)
          assertEqualSequences(slicedCase.expectedAbove, actualSliced._2)(sample.domainOps, valueHash)
        }
      }
    }
}
