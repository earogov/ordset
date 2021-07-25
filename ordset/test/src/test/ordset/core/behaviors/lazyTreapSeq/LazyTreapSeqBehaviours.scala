package test.ordset.core.behaviors.lazyTreapSeq

import ordset.Hash
import ordset.core.AbstractLazyTreapSegmentSeq._
import ordset.core.domain.{Domain, DomainOps}
import test.ordset.core.samples.segmentSeq.LazyTreapSeqSample
import test.ordset.core.SegmentSeqAssertions._
import org.scalatest.funspec.AnyFunSpec

trait LazyTreapSeqBehaviours[E, D <: Domain[E], V] {
  this: AnyFunSpec =>

  def sequenceProperlyCacheLazyValues(
    samples: Iterable[LazyTreapSeqSample[E, D, V] with LazyTreapSeqCacheTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>

      implicit val domainOps: DomainOps[E, D] = sample.domainOps
      implicit val valueHash: Hash[V] = sample.valueOps.valueHash
      implicit val zvalueHash: Hash[ZValue[E, D, V]] = sample.testZvalueOps.valueHash

      sample.lazyCacheCases.foreach { testPackage =>

        it(s"should return segments of $sample and properly cache lazy values for all cases in $testPackage") {

          // Restore initial state of lazy sequence.
          sample.restoreSequence

          testPackage.cases.foreach {
            case testCase: LazyTreapSeqCacheTest.SegmentTestCase[_, _, _] =>
              // Repeated call must return the same segment and leave the same state.
              (1 to 2) foreach { i =>
                val segment = sample.sequence.getSegmentForExtended(testCase.bound)
                assertSameRelationAndSegment(testCase.expectedSegment, segment, s"iteration $i")
                assertSameRelationAndSegmentSeq(testCase.expectedState, sample.sequence.getZippedSeq, s"iteration $i")
              }
            case testCase: LazyTreapSeqCacheTest.ValueTestCase[_, _, _] =>
              // Repeated call must return the same value and leave the same state.
              (1 to 2) foreach { i =>
                val seqValue = sample.sequence.getValueForExtended(testCase.bound)
                assert(
                  valueHash.eqv(seqValue, testCase.expectedValue),
                  s"expected value ${testCase.expectedValue} for bound ${testCase.bound} at iteration $i"
                )
                assertSameRelationAndSegmentSeq(testCase.expectedState, sample.sequence.getZippedSeq, s"iteration $i")
              }
          }
        }
      }
    }
}
