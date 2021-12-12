package ordset.test.random.commons

import ordset.random.commons.{KISSSynchronizedRngManager, KISSThreadLocalRngManager}
import org.scalatest.funspec.AnyFunSpec
import ordset.test.random.commons.internal.TestRngUtil.{genAsyncSamples, uniqueSamples}
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import java.util.concurrent.{Executors, TimeUnit}
import scala.concurrent.duration.Duration

@RunWith(classOf[JUnitRunner])
class KISSRngManagersSpec extends AnyFunSpec {

  private val sampleSize = 10
  private val tasksNum = 10
  private val timeout = Duration(5, TimeUnit.SECONDS)

  private val seed1 = 1234L
  private val seed2 = -4321L

  it(
    s"${classOf[KISSSynchronizedRngManager]} should provide same sequences when accessed concurrently and sequentially"
  ) {
    val fixedExecutionContext = scala.concurrent.ExecutionContext.fromExecutorService(
      Executors.newFixedThreadPool(20).nn
    )
    val singleExecutionContext = scala.concurrent.ExecutionContext.fromExecutorService(
      Executors.newSingleThreadExecutor().nn
    )
    val rngManager1 = new KISSSynchronizedRngManager(seed1, seed2)
    val rngManager2 = new KISSSynchronizedRngManager(seed1, seed2)

    val samples1 = genAsyncSamples(rngManager1, sampleSize, tasksNum, timeout)(fixedExecutionContext)
    val samples2 = genAsyncSamples(rngManager2, sampleSize, tasksNum, timeout)(singleExecutionContext)

    assert(samples1 == samples2)
  }

  it(
    s"${classOf[KISSSynchronizedRngManager]} should not provide repeated sequences"
  ) {
    val fixedExecutionContext = scala.concurrent.ExecutionContext.fromExecutorService(
      Executors.newFixedThreadPool(20).nn
    )
    val singleExecutionContext = scala.concurrent.ExecutionContext.fromExecutorService(
      Executors.newSingleThreadExecutor().nn
    )
    val rngManager = new KISSSynchronizedRngManager(seed2, seed1)

    val samples1 = genAsyncSamples(rngManager, sampleSize, tasksNum, timeout)(fixedExecutionContext)
    assert(uniqueSamples(samples1))

    val samples2 = genAsyncSamples(rngManager, sampleSize, tasksNum, timeout)(singleExecutionContext)
    assert(uniqueSamples(samples2))
  }

  it(
    s"${classOf[KISSThreadLocalRngManager]} should provide thread local behavior"
  ) {
    // Here we compare sequences that receives thread `S` in `singleExecutionContext`.
    // In one case random generation is executed exclusively for `S`, in other it's mixed with generation
    // for other thread in `fixedExecutionContext`.
    // Thread local behavior means that other threads should affect on the random sequences that
    // receives thread `S`.

    import Ordering.Implicits.seqOrdering

    val fixedExecutionContext = scala.concurrent.ExecutionContext.fromExecutorService(
      Executors.newFixedThreadPool(20).nn
    )
    val singleExecutionContext = scala.concurrent.ExecutionContext.fromExecutorService(
      Executors.newSingleThreadExecutor().nn
    )
    val rngManager1 = new KISSThreadLocalRngManager(seed1, seed2)
    val rngManager2 = new KISSThreadLocalRngManager(seed1, seed2)

    val samples1 = genAsyncSamples(rngManager1, sampleSize, tasksNum, timeout)(singleExecutionContext)
    genAsyncSamples(rngManager1, sampleSize, tasksNum, timeout)(fixedExecutionContext)
    val samples2 = genAsyncSamples(rngManager1, sampleSize, tasksNum, timeout)(singleExecutionContext)
    val samples3 = genAsyncSamples(rngManager2, sampleSize, 2 * tasksNum, timeout)(singleExecutionContext)

    assert(samples1.appendedAll(samples2).sorted == samples3)
  }

  it(
    s"${classOf[KISSThreadLocalRngManager]} should not provide repeated sequences"
  ) {
    val fixedExecutionContext = scala.concurrent.ExecutionContext.fromExecutorService(
      Executors.newFixedThreadPool(20).nn
    )
    val singleExecutionContext = scala.concurrent.ExecutionContext.fromExecutorService(
      Executors.newSingleThreadExecutor().nn
    )
    val rngManager = new KISSThreadLocalRngManager(seed2, seed1)

    val samples1 = genAsyncSamples(rngManager, sampleSize, tasksNum, timeout)(fixedExecutionContext)
    assert(uniqueSamples(samples1))

    val samples2 = genAsyncSamples(rngManager, sampleSize, tasksNum, timeout)(singleExecutionContext)
    assert(uniqueSamples(samples2))
  }
}
