package ordset.test.core

import ordset.random.RngManager
import ordset.random.commons.{KISSSynchronizedRngManager, KISSThreadLocalRngManager}

object TestRngUtil {

  def defaultRngManager(seed: Long): RngManager = new KISSThreadLocalRngManager(seed, seed)

  object Givens {

    /**
     * Default random manager for tests.
     * 
     * To provide reproducibility:
     * <div>
     *   - Constant seeds are used.
     * </div>
     * <div>
     *   - `def` is used instead of `lazy val`. Instance, shared between multiple tests, may have unpredictable 
     *   state that depends on tests order, concurrent access in one of the tests, etc.
     * </div>
     */
    implicit def defaultRngManager: RngManager =
      new KISSThreadLocalRngManager(0xa19f0b2d0c48ea51L, 0x0e87fa311d2d0fL)
  }
}
