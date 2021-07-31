package test.ordset.core.behaviors.lazyTreapSeq

import ordset.Hash
import ordset.core.AbstractLazyTreapSegmentSeq._
import ordset.core.{ExtendedBound, SegmentSeq}
import ordset.core.domain.{Domain, DomainOps}
import ordset.random.RngManager
import test.ordset.core.samples.segmentSeq.LazyTreapSeqSample
import test.ordset.core.SegmentSeqAssertions._
import org.scalatest.funspec.AnyFunSpec

import java.util.concurrent.{Executors, TimeUnit}
import scala.collection.immutable.HashMap
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

        val boundsArray = sample.extendedBounds.toArray
        randomAccess(sample.sequence, boundsArray, sample.rngManager)

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

        val boundsArray = sample.extendedBounds.toArray
        val future = Future.sequence((1 to tasksNum).map( _ =>
          Future {
            try {
              randomAccess(sample.sequence, boundsArray, sample.rngManager)
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
    boundsArray: Array[ExtendedBound.Upper[E]],
    rngManager: RngManager
  ): Unit = {
    val rng = rngManager.newUnsafeUniformRng()
    (1 to (2 * boundsArray.length)).foreach { _ =>
      val rnd = rng.nextInt()
      val index = math.abs(rnd) % boundsArray.length
      val bound = boundsArray(index)
      if (rnd > 0) sequence.getSegmentForExtended(bound)
      else sequence.getValueForExtended(bound)
    }
  }
}
