package ordset.test.random.commons

import ordset.random.{RngManager, UnsafeUniformRng}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException}

object TestRngUtil {

  /**
   * Checks whether two generators provides same random sequences (on some finite length).
   */
  def assertSameRng(rng1: UnsafeUniformRng, rng2: UnsafeUniformRng): Unit =
    assert(Range(1, 100).forall(_ => rng1.nextLong() == rng2.nextLong()))

  /**
   * Checks whether two generators provides different random sequences (on some finite length).
   */
  def assertDifferentRng(rng1: UnsafeUniformRng, rng2: UnsafeUniformRng): Unit = {
    assert(Range(1, 3).exists(_ => rng1.nextLong() != rng2.nextLong()))
  }

  /**
   * Calls all methods of the given generator to make sure there is no exceptions.
   */
  def assertSaneRng(rng: UnsafeUniformRng): Unit = {
    val bound = 100

    val int = rng.nextInt()

    val intBounded = rng.nextInt(bound)
    assert(intBounded >= 0 && intBounded < bound)

    val long = rng.nextLong()

    val longBounded = rng.nextLong(100)
    assert(longBounded >= 0 && longBounded < bound)

    val boolean = rng.nextBoolean()

    val double = rng.nextDouble()

    val float = rng.nextFloat()

    val bytes = new Array[Byte](16)
    rng.nextBytes(bytes)

    val moreBytes = new Array[Byte](32)
    rng.nextBytes(moreBytes, 15, 16)
    // Check that first half of array is empty.
    assert(Range(0, 15).map(moreBytes(_)).forall(_ == 0.toByte))
  }

  /**
   * Generates random sequences of length `sampleSize`.
   *
   * Each sequence is generated asynchronously with the given `executionContext`.
   * The total number of sequences is `tasksNum`.
   *
   * The output list of sequences is sorted according to natural ascending ordering
   * to provide convenient comparison of all received sequences.
   *
   * @param rngManager generator of random sequences
   * @param sampleSize length of each random sequence
   * @param tasksNum total number of generated sequences
   * @param timeout timeout for all tasks to complete
   * @param debugPrint print thread id with generated number
   * @param executionContext execution context
   */
  @throws[TimeoutException]
  @throws[InterruptedException]
  def genAsyncSamples(
    rngManager: RngManager,
    sampleSize: Int,
    tasksNum: Int,
    timeout: Duration,
    debugPrint: Boolean = false
  )(
    implicit executionContext: ExecutionContext
  ): IndexedSeq[IndexedSeq[Long]] = {

    import Ordering.Implicits.seqOrdering

    val future = Future.sequence(Range(0, tasksNum).map( _ =>
      Future {
        if (debugPrint) println(
          s"${Console.BLUE}${Thread.currentThread()}${Console.RESET} started generation of random sequence"
        )
        val rng = rngManager.newUnsafeUniformRng()
        Range(0, sampleSize).map( _ => rng.nextLong() )
      }
    ))
    Await.result(future, timeout).sorted
  }

  /**
   * Returns `true` if list contains no duplicate sequences (only full equality is considered).
   *
   * Precondition: list of sequences must be sorted according to natural ascending ordering.
   *
   * @param samples sorted list of random sequences
   */
  @throws[IllegalArgumentException]("if list of samples is not sorted according to natural ascending ordering")
  def uniqueSamples(
    samples: IndexedSeq[IndexedSeq[Long]]
  ): Boolean = {

    val ordering = Ordering.Implicits.seqOrdering[IndexedSeq, Long]

    samples.zip(samples.tail)
      .map { pair =>
        val cmp = ordering.compare(pair._1, pair._2)
        if (cmp > 0)
          throw new IllegalArgumentException("List of samples must be sorted according to natural ascending ordering.")
        cmp != 0
      }
      .takeWhile(identity[Boolean])
      .headOption.getOrElse(true)
  }
}
