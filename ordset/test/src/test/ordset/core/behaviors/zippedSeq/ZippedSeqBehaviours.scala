package test.ordset.core.behaviors.zippedSeq

import ordset.Hash
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.{SegmentSeq, ZippedSegmentSeq}
import test.ordset.core.SegmentSeqAssertions.assertSameRelationAndSegmentSeq
import test.ordset.core.samples.segmentSeq.ZippedSeqSample
import org.scalatest.funspec.AnyFunSpec

trait ZippedSeqBehaviours[E, D <: Domain[E], U1, U2, V] {
  this: AnyFunSpec =>

  def segmentCanPatchOriginalSeq(
    samples: Iterable[ZippedSeqSample[E, D, U1, U2, V] with OriginalSeqPatchedTest[E, D, U1, U2]]
  ): Unit =
    samples.foreach { sample =>

      implicit val domainOps: DomainOps[E, D] = sample.domainOps
      implicit val valueHash1: Hash[U1] = sample.sequence.firstSeq.valueOps.valueHash
      implicit val valueHash2: Hash[U2] = sample.sequence.secondSeq.valueOps.valueHash

      sample.firstSeqPatchedCases.foreach { testCase =>

        val segment = sample.sequence.getSegmentForBound(testCase.bound)

        it(s"should patch first original sequence of $sample within a segment $segment with $testCase") {
          val actual = segment.patchedFirstSeq(testCase.patch)
          assertSameRelationAndSegmentSeq(testCase.expected, actual)
        }
      }

      sample.secondSeqPatchedCases.foreach { testCase =>

        val segment = sample.sequence.getSegmentForBound(testCase.bound)

        it(s"should patch second original sequence of $sample within a segment $segment with $testCase") {
          val actual = segment.patchedSecondSeq(testCase.patch)
          assertSameRelationAndSegmentSeq(testCase.expected, actual)
        }
      }
    }
}
