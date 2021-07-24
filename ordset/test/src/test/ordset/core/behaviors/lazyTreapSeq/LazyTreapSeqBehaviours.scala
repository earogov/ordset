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
        
        // Respore initial state of lazy sequence.
        sample.restoreSequence
        
        testPackage.cases.foreach { testCase =>
          
          it(s"should return segment of $sample at bound ${testCase.bound} ($testPackage) and properly cache lazy value") {
            
            val segment1 = sample.sequence.getSegmentForExtended(testCase.bound)
            assertSameRelationAndSegment(testCase.expectedSegment, segment1)
            assertSameRelationAndSegmentSeq(testCase.expectedState, sample.sequence.getZippedSeq)

            // Repeated call must return the same segment and leave the same state.
            val segment2 = sample.sequence.getSegmentForExtended(testCase.bound)
            assertSameRelationAndSegment(testCase.expectedSegment, segment2)
            assertSameRelationAndSegmentSeq(testCase.expectedState, sample.sequence.getZippedSeq)
          }
        }
      }
    }
}
