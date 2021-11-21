package ordset.random.commons

import ordset.random.UnsafeUniformRng
import org.apache.commons.rng.UniformRandomProvider

/**
 * Simple proxy for [[org.apache.commons.rng.UniformRandomProvider]].
 */
final class UniformRandomProviderProxy(original: UniformRandomProvider) extends UnsafeUniformRng {

  override def nextBytes(bytes: Array[Byte]): Unit = original.nextBytes(bytes)

  override def nextBytes(bytes: Array[Byte], start: Int, len: Int): Unit = original.nextBytes(bytes, start, len)

  override def nextInt(): Int = original.nextInt()

  override def nextInt(n: Int): Int = original.nextInt(n)

  override def nextLong(): Long = original.nextLong()

  override def nextLong(n: Long): Long = original.nextLong(n)

  override def nextBoolean(): Boolean = original.nextBoolean()

  override def nextFloat(): Float = original.nextFloat()

  override def nextDouble(): Double = original.nextDouble()
}
