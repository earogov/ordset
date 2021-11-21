package ordset.random.commons

import org.apache.commons.rng.simple.RandomSource

/**
 * Random seed factory.
 *
 * The implementations are provided by [[org.apache.commons.rng.simple.RandomSource]].
 */
object CommonsSeedFactory {

  import scala.language.unsafeNulls

  /**
   * Creates a number for use as a seed.
   *
   * @return a random number.
   */
  def createInt(): Int = RandomSource.createInt()

  /**
   * Creates a number for use as a seed.
   *
   * @return a random number.
   */
  def createLong(): Long = RandomSource.createLong()

  /**
   * Creates an array of numbers for use as a seed.
   *
   * @param n Size of the array to create.
   * @return an array of `n` random numbers.
   */
  def createIntArray(n: Int): Array[Int] = RandomSource.createIntArray(n)

  /**
   * Creates an array of numbers for use as a seed.
   *
   * @param n Size of the array to create.
   * @return an array of `n` random numbers.
   */
  def createLongArray(n: Int): Array[Long] = RandomSource.createLongArray(n)
}
