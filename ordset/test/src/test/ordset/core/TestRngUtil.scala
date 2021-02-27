package test.ordset.core

import ordset.random.RngManager
import ordset.random.commons.{KISSSynchronizedRngManager, KISSThreadLocalRngManager}

object TestRngUtil {

  def defaultRngManager(seed: Long): RngManager = new KISSThreadLocalRngManager(seed, seed)

  object Implicits {

    /**
     * Constant seeds are used for tests reproducibility.
     */
    implicit lazy val defaultRngManager: RngManager =
      new KISSThreadLocalRngManager(0xa19f0b2d0c48ea51L, 0x0e87fa311d2d0fL)
  }
}
