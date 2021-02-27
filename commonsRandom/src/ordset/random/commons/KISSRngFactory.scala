package ordset.random.commons

import ordset.random.UnsafeUniformRng
import ordset.random.commons.internal.NumbersUtil
import org.apache.commons.rng.simple.RandomSource
import org.apache.commons.rng.simple.internal.NativeSeedType

/**
 * Factory for [[https://en.wikipedia.org/wiki/KISS_(algorithm) KISS]] random number generator.
 *
 * This generator is a good default choice - it combines high performance with excellent quality.
 *
 * The implementation is provided by [[org.apache.commons.rng.simple.RandomSource.KISS]].
 */
object KISSRngFactory {

  val nativeSeedType: NativeSeedType = NativeSeedType.INT_ARRAY

  val nativeSeedSize: Int = 4

  val nativeSeedBytes: Int = 4 * NativeSeedType.INT_ARRAY.getBytes

  /**
   * Creates [[https://en.wikipedia.org/wiki/KISS_(algorithm) KISS]] random number generator with a random seed.
   */
  def create(): UnsafeUniformRng =
    CommonsRngFactory.create(RandomSource.KISS)

  /**
   * Creates [[https://en.wikipedia.org/wiki/KISS_(algorithm) KISS]] random number generator with the given seeds.
   * @param seed1   First seed value.
   * @param seed2   Second seed value.
   */
  def create(seed1: Long, seed2: Long): UnsafeUniformRng =
    CommonsRngFactory.create(RandomSource.KISS, NumbersUtil.makeIntArray(seed1, seed2))
}