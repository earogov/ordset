package ordset.test.core.behaviors.segmentSeq

import ordset.core.{SegmentSeq, Segment}
import ordset.core.domain.Domain
import ordset.core.ExtendedBound
import ordset.test.core.samples.segmentSeq.SegmentSeqSample
import ordset.test.core.RandomUtil
import java.util.concurrent.{ConcurrentHashMap, Executors, TimeUnit}
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration.Duration
import org.scalatest.funspec.AnyFunSpec
import java.util.function.BiFunction
import ordset.test.core.RandomUtil
import ordset.test.core.implementations.segmentSeq.lazyTreap.LazyTreapSeqUtil
import scala.jdk.CollectionConverters.*
import ordset.test.AssertionsUtil

trait LazyEvalProperties[E, D <: Domain[E], V] {
  this: AnyFunSpec =>

  implicit private lazy val fixedExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newCachedThreadPool.nn)

  def sequenceCallsFunctionToComputeLazyValueOnlyOnce(
    samples: Iterable[SegmentSeqSample[E, D, V, SegmentSeq[E, D, V]]],
    values: Seq[V]
  ): Unit =
    samples.foreach { sample =>

      val tasksNum = 20
      val timeout = Duration(10, TimeUnit.SECONDS)

      implicit val domainOps = sample.domainOps
      implicit val valueOps = sample.valueOps
      implicit val rngManager = sample.rngManager
      implicit val boundSelector = sample.boundSelector
      implicit val valueHash = valueOps.valueHash

      it(s"should call only once function to compute lazy value of $sample") {

        val seq = sample.sequence

        val bounds = seq.extendedUpperBounds.toIndexedSeq
        val requestsNum = bounds.size

        val countMap = new ConcurrentHashMap[ExtendedBound[E], Int]
        seq.firstSegment.forwardIterator.foreach { s => countMap.put(s.upper, 0) }

        val updFunc = new BiFunction[ExtendedBound[E], Int, Int] { 
          override def apply(b: ExtendedBound[E] | Null, v: Int | Null): Int = v.nn + 1
        }

        val valuesSeq = values.toIndexedSeq

        val lazySeq: SegmentSeq[E, D, V] = seq.flatMapSegments { s => 
          countMap.computeIfPresent(s.upper, updFunc)
          val seq = LazyTreapSeqUtil.makeRandomTreapSeq(
            s.lower,
            s.upper,
            valuesSeq
          )
          seq
        }

        val future = Future.sequence((1 to tasksNum).map( i =>
          Future {
            (1 to requestsNum).foreach { r =>
              // Apply random action at random bound.
              val rng = sample.rngManager.newUnsafeUniformRng()
              val boundOpt = RandomUtil.randomPick(bounds, rng.nextInt())
              boundOpt.map { b =>  
                if (rng.nextBoolean()) lazySeq.getValueForExtended(b)
                else {
                  val s: Segment[E, D, V] = lazySeq.getSegmentForExtended(b)
                  if (rng.nextBoolean()) {
                    s match {
                      case s: Segment.WithNext[E, D, V] => s.moveNext
                      case s: Segment.WithPrev[E, D, V] => s.movePrev
                      case _ => // nothing to do
                    }
                  }
                } 
              }
            }
          }
        ))
        Await.result(future, timeout)

        var zeroCount = 0
        var oneCount = 0
        var errorCount = 0
        for (v <- countMap.values.nn.asScala) {
          if (v == 0) zeroCount = zeroCount + 1
          else if (v == 1) oneCount = oneCount + 1
          else errorCount = errorCount + 1
        }

        assert(
          errorCount == 0, 
          s"Expected none function was called more than once, but found $errorCount"
        )

        info(s"Tasks number: $tasksNum")
        info(s"Requests per task: $requestsNum")
        info(s"Functions with zero calls: $zeroCount")
        info(s"Functions with one call: $oneCount")
        info(s"Initial bounds number: ${bounds.size}")
        info(s"Final bounds number: ${lazySeq.extendedUpperBounds.size}")
      }
    }
}
