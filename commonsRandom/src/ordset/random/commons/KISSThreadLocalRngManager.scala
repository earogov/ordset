package ordset.random.commons

import ordset.random.{RngManager, UnsafeUniformRng}

/**
 * [[RngManager]] for [[https://en.wikipedia.org/wiki/KISS_(algorithm) KISS]] random number generator.
 *
 * Provides for each thread its own [[RngManager]] instance.
 * So thread `A` can't affect on the random sequence that will receive thread `B`.
 *
 * Current implementation requires less requests to shared state to perform seed management
 * compared to [[KISSSynchronizedRngManager]].
 */
final class KISSThreadLocalRngManager(seed1: Long, seed2: Long) extends RngManager {

  override def newUnsafeUniformRng(): UnsafeUniformRng = threadLocal.get().newUnsafeUniformRng()

  // Private section ---------------------------------------------------------- //
  private val seedManager = new KISSSynchronizedRngManager(seed1, seed2)

  private val threadLocal: ThreadLocal[UnsafeRngManager] = ThreadLocal.withInitial { () =>
    new UnsafeRngManager(seedManager.newUnsafeUniformRng())
  }

  private class UnsafeRngManager(private val seedRng: UnsafeUniformRng) extends RngManager {

    override def newUnsafeUniformRng(): UnsafeUniformRng = {
      val seed1 = seedRng.nextLong()
      val seed2 = seedRng.nextLong()
      KISSRngFactory.create(seed1, seed2)
    }
  }
}