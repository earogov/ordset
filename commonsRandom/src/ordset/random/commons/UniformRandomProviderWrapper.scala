package ordset.random.commons

import ordset.random.UnsafeUniformRandom
import org.apache.commons.rng.UniformRandomProvider

/**
 * Simple wrapper for [[org.apache.commons.rng.UniformRandomProvider]].
 */
class UniformRandomProviderWrapper(delegate: UniformRandomProvider) extends UnsafeUniformRandom {

  override def nextBytes(bytes: Array[Byte]): Unit = delegate.nextBytes(bytes)

  override def nextBytes(bytes: Array[Byte], start: Int, len: Int): Unit = delegate.nextBytes(bytes, start, len)

  override def nextInt: Int = delegate.nextInt()

  override def nextInt(n: Int): Int = delegate.nextInt(n)

  override def nextLong: Long = delegate.nextLong()

  override def nextLong(n: Long): Long = delegate.nextLong(n)

  override def nextBoolean: Boolean = delegate.nextBoolean()

  override def nextFloat: Float = delegate.nextFloat()

  override def nextDouble: Double = delegate.nextDouble()
}
