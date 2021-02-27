package test.ordset.core

import ordset.random.UnsafeUniformRng
import ordset.random.commons.KISSRngFactory

object TestRngUtil {

  /**
   * Don't use random seed for reproducibility of test.
   */
  def defaultRng(): UnsafeUniformRng =
    KISSRngFactory.create(0xa19f0b2d0c48ea51L, 0x0e87fa311d2d0fL)

  /**
   * Don't use random seed for reproducibility of test.
   */
  def defaultRng(seed: Long): UnsafeUniformRng =
    KISSRngFactory.create(seed, seed)

  /**
   * Don't use random seed for reproducibility of test.
   */
  def defaultRng(seed1: Long, seed2: Long): UnsafeUniformRng =
    KISSRngFactory.create(seed1, seed2)
}
