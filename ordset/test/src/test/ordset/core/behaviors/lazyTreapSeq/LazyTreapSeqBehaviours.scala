package test.ordset.core.behaviors.lazyTreapSeq

import ordset.Hash
import ordset.core.AbstractLazyTreapSegmentSeq._
import ordset.core.{ExtendedBound, SegmentSeq}
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.{RngManager, UnsafeUniformRng}
import test.ordset.core.samples.segmentSeq.LazyTreapSeqSample
import test.ordset.core.SegmentSeqAssertions._
import org.scalatest.funspec.AnyFunSpec
import test.ordset.core.RandomUtil

import java.util.concurrent.{Executors, TimeUnit}
import scala.collection.immutable.{ArraySeq, HashMap}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait LazyTreapSeqBehaviours[E, D <: Domain[E], V] {
  this: AnyFunSpec =>

  private val tasksNum = 3
  private val timeout = Duration(5, TimeUnit.SECONDS)

  private implicit val fixedExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(tasksNum))

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

  def sequenceHasValidStateAfterSequentialRandomAccess(
    samples: Iterable[LazyTreapSeqSample[E, D, V]]
  ): Unit =
    samples.foreach { sample =>

      implicit val domainOps: DomainOps[E, D] = sample.domainOps
      implicit val valueHash: Hash[V] = sample.valueOps.valueHash
      implicit val zvalueHash: Hash[ZValue[E, D, V]] = sample.testZvalueOps.valueHash

      it(s"should have valid state of $sample after sequential random access") {

        // Restore initial state of lazy sequence.
        sample.restoreSequence

        val rng =  sample.rngManager.newUnsafeUniformRng()
        val boundsArr = ArraySeq.from(sample.extendedBounds)

        randomAccess(sample.sequence, boundsArr, rng)

        assertSameRelationAndSegmentSeq(sample.reference, sample.sequence)
        // After first assertion sequence will be totally stable => we can compare zipped sequence with reference.
        assertSameRelationAndSegmentSeq(sample.zippedReference, sample.sequence.getZippedSeq)
      }
    }

  def sequenceHasValidStateAfterConcurrentRandomAccess(
    samples: Iterable[LazyTreapSeqSample[E, D, V]]
  ): Unit =
    samples.foreach { sample =>

      implicit val domainOps: DomainOps[E, D] = sample.domainOps
      implicit val valueHash: Hash[V] = sample.valueOps.valueHash
      implicit val zvalueHash: Hash[ZValue[E, D, V]] = sample.testZvalueOps.valueHash

      it(s"should have valid state of $sample after concurrent random access") {

        // Restore initial state of lazy sequence.
        sample.restoreSequence

        val boundsArr = ArraySeq.from(sample.extendedBounds)
        val future = Future.sequence((1 to tasksNum).map( _ =>
          val rng =  sample.rngManager.newUnsafeUniformRng()
          Future {
            try {
              randomAccess(sample.sequence, boundsArr, rng)
            } catch {
              case e: AssertionError => fail(e)
            }
          }
        ))
        Await.result(future, timeout)

        assertSameRelationAndSegmentSeq(sample.reference, sample.sequence)
        // After first assertion sequence will be totally stable => we can compare zipped sequence with reference.
        assertSameRelationAndSegmentSeq(sample.zippedReference, sample.sequence.getZippedSeq)
      }
    }

  private def randomAccess(
    sequence: SegmentSeq[E, D, V],
    bounds: ArraySeq[ExtendedBound.Upper[E]],
    rng: UnsafeUniformRng
  ): Unit = {
    (1 to (2 * bounds.length)).foreach { _ =>
      val rnd = rng.nextInt()
      RandomUtil.randomPick(bounds, rnd).foreach { bound =>
        if (rnd > 0) sequence.getSegmentForExtended(bound)
        else sequence.getValueForExtended(bound)
      }
    }
  }
}
