package ordset.random.commons

import ordset.random.{RngManager, UnsafeUniformRng}

/**
 * [[RngManager]] for [[https://en.wikipedia.org/wiki/KISS_(algorithm) KISS]] random number generator.
 *
 * Uses shared RNG with synchronized access for seed management.
 */
final class KISSSynchronizedRngManager(seed1: Long, seed2: Long) extends RngManager {

  override def newUnsafeUniformRng(): UnsafeUniformRng = {
    var seed1 = 0L
    var seed2 = 0L
    // Acquire lock to modify internal state of `seedRng`.
    lock.synchronized {
      seed1 = seedRng.nextLong()
      seed2 = seedRng.nextLong()
    }
    KISSRngFactory.create(seed1, seed2)
  }

  // Private section ---------------------------------------------------------- //
  private val lock: Object = new Object()

  private val seedRng = KISSRngFactory.create(seed1, seed2)
}