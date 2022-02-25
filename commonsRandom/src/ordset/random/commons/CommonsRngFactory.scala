package ordset.random.commons

import ordset.random.UnsafeUniformRng
import ordset.random.commons.internal.NumbersUtil
import org.apache.commons.rng.core.util.NumberFactory
import org.apache.commons.rng.simple.RandomSource

/**
 * Factory for random number generators.
 *
 * The implementations are provided by [[org.apache.commons.rng.simple.RandomSource]].
 */
object CommonsRngFactory {

  import scala.language.unsafeNulls

  /**
   * Creates a random number generator with a random seed.
   *
   * @param source RNG type.
   * @return the RNG.
   */
  def create(source: RandomSource): UnsafeUniformRng =
    new UniformRandomProviderProxy(source.create)

  /**
   * Creates a random number generator with the given `seed`.
   *
   * @param source RNG type.
   * @param seed   Seed value. It can be `null` (in which case a random value will be used).
   * @param data   Additional arguments to the implementation's constructor.
   *               Please refer to the documentation of each specific implementation.
   * @return the RNG.
   *
   * @throws UnsupportedOperationException if the type of the `seed`
   *                                       is invalid.
   * @throws IllegalStateException if data is missing to initialize the
   *                               generator implemented by the given `source`.
   */
  def create(source: RandomSource, seed: Any, data: Any*): UnsafeUniformRng =
    new UniformRandomProviderProxy(source.create(seed, data: _*))
}
