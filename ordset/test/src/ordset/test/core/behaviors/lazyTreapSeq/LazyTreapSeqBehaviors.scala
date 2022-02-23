package ordset.test.core.behaviors.lazyTreapSeq

import ordset.Hash
import ordset.core.segmentSeq.internal.lazySeq.*
import ordset.core.ExtendedBound
import ordset.core.segmentSeq.*
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.random.{RngManager, UnsafeUniformRng}
import ordset.test.core.samples.segmentSeq.LazyTreapSeqSample
import ordset.test.core.SegmentSeqAssertions._
import org.scalatest.funspec.AnyFunSpec
import ordset.test.core.RandomUtil
import ordset.test.core.implementations.segmentSeq.lazyTreap.LazyTreapSeqUtil

import java.util.concurrent.{Executors, TimeUnit}
import scala.collection.immutable.{ArraySeq, HashMap}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait LazyTreapSeqBehaviors[E, D[X] <: Domain[X], V] {
  this: AnyFunSpec =>

  implicit private lazy val fixedExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newCachedThreadPool.nn)

  /**
   * Validates cache of lazy sequence after access operations.
   *
   * Initial sequence must be in totally lazy state.
   */
  def sequenceProperlyCacheLazyValues(
    samples: Iterable[LazyTreapSeqSample.Fixed[E, D, V] with LazyTreapSeqCacheTest[E, D, V]]
  ): Unit =
    samples.foreach { sample =>

      import ordset.test.core.behaviors.lazyTreapSeq.LazyTreapSeqCacheTest._

      implicit val domainOps: DomainOps[E, D] = sample.domainOps
      implicit val valueOps: ValueOps[V] = sample.valueOps
      implicit val zvalueOps: ValueOps[ZValue[E, D, V]] = sample.testZvalueOps

      sample.lazyCacheCases.foreach { testPackage =>

        it(s"should return segments of $sample and properly cache lazy values for all cases in $testPackage") {

          // Get copy of sequence. Further operation will not affect on state of original.
          val seq = sample.sequence

          testPackage.cases.foreach {
            case testCase: SegmentTestCase[_, _, _] =>
              // Repeated call must return the same segment and leave the same state.
              (1 to 2) foreach { i =>
                val segment = seq.getSegmentForExtended(testCase.bound)
                assertSameRelationAndSegment(testCase.expectedSegment, segment, s"iteration $i")
                assertSameRelationAndSegmentSeq(testCase.expectedState, seq.getZippedSeq, s"iteration $i")
              }
            case testCase: ValueTestCase[_, _, _] =>
              // Repeated call must return the same value and leave the same state.
              (1 to 2) foreach { i =>
                val seqValue = seq.getValueForExtended(testCase.bound)
                assert(
                  valueOps.eqv(seqValue, testCase.expectedValue),
                  s"expected value ${testCase.expectedValue} for bound ${testCase.bound} at iteration $i"
                )
                assertSameRelationAndSegmentSeq(testCase.expectedState, seq.getZippedSeq, s"iteration $i")
              }
          }
        }
      }
    }

  def sequenceHasValidStateAfterSequentialRandomAccess(
    samples: Iterable[LazyTreapSeqSample.Fixed[E, D, V]]
  )(
    implicit rngManager: RngManager
  ): Unit =
    samples.foreach { sample =>

      implicit val domainOps: DomainOps[E, D] = sample.domainOps
      implicit val valueOps: ValueOps[V] = sample.valueOps
      implicit val zvalueOps: ValueOps[ZValue[E, D, V]] = sample.testZvalueOps

      it(s"should have valid state of $sample after sequential random access") {

        // Get copy of sequence. Further operation will not affect on state of original.
        val seq = sample.sequence

        LazyTreapSeqUtil.shuffleLazySeq(seq, sample.extendedBounds)

        assertSameRelationAndSegmentSeq(sample.reference, seq)
        // After first assertion sequence will be totally stable => we can compare zipped sequence with reference.
        assertSameRelationAndSegmentSeq(sample.zippedReference, seq.getZippedSeq)
      }
    }

  def sequenceHasValidStateAfterConcurrentRandomAccess(
    samples: Iterable[LazyTreapSeqSample.Fixed[E, D, V]]
  )(
    implicit rngManager: RngManager
  ): Unit =
    samples.foreach { sample =>

      val tasksNum = 10
      val timeout = Duration(5, TimeUnit.SECONDS)

      implicit val domainOps: DomainOps[E, D] = sample.domainOps
      implicit val valueOps: ValueOps[V] = sample.valueOps
      implicit val zvalueOps: ValueOps[ZValue[E, D, V]] = sample.testZvalueOps

      it(s"should have valid state of $sample after concurrent random access") {
        runTest(withStrictConversion = false)
      }

      it(s"should have valid state of $sample after concurrent random access and conversion to strict sequence") {
        runTest(withStrictConversion = true)
      }

      def runTest(withStrictConversion: Boolean): Unit = {
        // Get copy of sequence. Further operation will not affect on state of original.
        val seq = sample.sequence

        // One of the tasks should execute conversion into strict sequence.
        val rng = sample.rngManager.newUnsafeUniformRng()
        val strictTaskNum = rng.nextInt(tasksNum) + 1

        val future = Future.sequence((1 to tasksNum).map { i =>
          Future {
            try {
              if (withStrictConversion && i == strictTaskNum) seq.strict
              else LazyTreapSeqUtil.shuffleLazySeq(seq, sample.extendedBounds)
            } catch {
              case e: AssertionError => fail(e)
            }
          }
        })
        Await.result(future, timeout)

        assertSameRelationAndSegmentSeq(sample.reference, seq)
        // After first assertion sequence will be totally stable => we can compare zipped sequence with reference.
        assertSameRelationAndSegmentSeq(sample.zippedReference, seq.getZippedSeq)
      }
    }

  /**
   * Validates state of lazy sequence after multiple `takeAbove` and `takeBelow` operations.
   * 
   * Initial sequence must be in totally lazy state.
   */
  def sequenceProperlyHandleMultipleTakeAboveAndBelow(
    samples: Iterable[LazyTreapSeqSample.Fixed[E, D, V] with LazyTreapSeqMultipleTakeTest[E, D, V]]
  ): Unit = {
    samples.foreach { sample =>

      import ordset.test.core.behaviors.lazyTreapSeq.LazyTreapSeqMultipleTakeTest._

      implicit val domainOps: DomainOps[E, D] = sample.domainOps
      implicit val valueOps: ValueOps[V] = sample.valueOps

      sample.multipleTakeCases.foreach { testPackage =>

        it(
          s"should have valid state of $sample after applying `takeAbove` and `takeBelow` operations " +
          s"of test package $testPackage"
        ) {
          // Get copy of sequence. Further operation will not affect on state of original.
          val seq: SegmentSeq[E, D, V] = sample.sequence
          val commands = List.from(testPackage.commands)
          val result = commands.zipWithIndex.foldLeft(CommandResult.init(seq)) { (result, ci) =>
            val command = ci._1
            val commandIndex = ci._2
            command match {
              case c: TakeBelowCommand[e, d, v] => result.unchecked(_.takeBelowExtended(c.bound))
              case c: TakeAboveCommand[e, d, v] => result.unchecked(_.takeAboveExtended(c.bound))
              case c: Validation[e, d, v] => {
                assertSameRelationAndSegmentSeq(c.expected, result.seq, s"command index: ${ci._2}")
                result.init()
              }
            }
          }
          if (result.uncheckedCount > 0) {
            fail(s"Found ${result.uncheckedCount} command(s) in test package. Add validation command in the end.")
          }
        }
      }
    }

    case class CommandResult[E, D[X] <: Domain[X], V](
      val seq: SegmentSeq[E, D, V],
      val uncheckedCount: Int
    ) {
      def init(): CommandResult[E, D, V] = CommandResult(seq, 0)

      def unchecked(f: SegmentSeq[E, D, V] => SegmentSeq[E, D, V]): CommandResult[E, D, V] =
        CommandResult(f(seq), uncheckedCount + 1)
    }

    object CommandResult {

      def init[E, D[X] <: Domain[X], V](seq: SegmentSeq[E, D, V]): CommandResult[E, D, V] = CommandResult(seq, 0)
    }
  }
}
