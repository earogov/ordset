package ordset.random.commons

import ordset.random.UnsafeUniformRandom
import org.apache.commons.rng.core.util.NumberFactory
import org.apache.commons.rng.simple.RandomSource

/**
 * Random number generator factory based on [[org.apache.commons.rng.simple.RandomSource]].
 */
object ApacheCommonsRng { rng =>

  /**
   * Creates default random number generator with a random seed.
   */
  def default(): UnsafeUniformRandom = DefaultSource.create()

  /**
   * Creates a random number generator with the given seed.
   *
   * @param seed   Seed value.
   */
  def default(seed: Long): UnsafeUniformRandom = DefaultSource.create(seed)

  /**
   * Creates a random number generator with a random seed.
   * See [[RandomSource.create]].
   *
   * @param source RNG type.
   * @return the RNG.
   */
  def create(source: RandomSource): UnsafeUniformRandom =
    new UniformRandomProviderWrapper(RandomSource.create(source))

  /**
   * Creates a random number generator with the given {@code seed}.
   * See [[RandomSource.create]].
   *
   * @param source RNG type.
   * @param seed   Seed value.  It can be {@code null} (in which case a
   *               random value will be used).
   * @param data   Additional arguments to the implementation's constructor.
   *               Please refer to the documentation of each specific implementation.
   * @return the RNG.
   *
   * @throws UnsupportedOperationException if the type of the {@code seed}
   *                                       is invalid.
   * @throws IllegalStateException         if data is missing to initialize the
   *                                       generator implemented by the given {@code source}.
   */
  def create(source: RandomSource, seed: Any, data: Any*): UnsafeUniformRandom =
    new UniformRandomProviderWrapper(RandomSource.create(source, seed, data: _*))

  private object DefaultSource {

    /**
     * Default random source.
     */
    private val source: RandomSource = RandomSource.ISAAC

    /**
     * Native seed for [[RandomSource.ISAAC]] is `Int` array of size 256.
     */
    private val nativeSeedSize = 256

    /**
     * Size of native seed array in bytes.
     */
    private val nativeSeedSizeInBytes = nativeSeedSize * Integer.BYTES

    /**
     * Random source to generate native seed for `source` from `Long`.
     */
    private val seedSource: RandomSource = RandomSource.SPLIT_MIX_64

    /**
     * Creates default random number generator with a random seed.
     */
    def create(): UnsafeUniformRandom = rng.create(source)

    /**
     * Creates a random number generator with the given seed.
     *
     * @param seed   Seed value.
     */
    def create(seed: Long): UnsafeUniformRandom = {
      val ISSACSeed = new Array[Byte](nativeSeedSizeInBytes)
      RandomSource.create(seedSource, seed).nextBytes(ISSACSeed)
      val arr = NumberFactory.makeIntArray(ISSACSeed)
      rng.create(source, arr)
    }
  }
}
