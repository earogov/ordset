package ordset.random

/**
 * Instances of [[UnsafeUniformRng]] provide maximal performance due to absence of synchronization penalty.
 * This implies requirement that each instance must be owned only by one thread at the same time.
 *
 * [[RngManager]] is intended to simplify distribution of random generators across application.
 * The general rules of usage are the follows:
 * <div>
 * - [[UnsafeUniformRng]] should never be passed between methods, especially if there any possibility that it
 * will be called from another thread in the result. This restriction may be relaxed for cases when
 * generators are passed between private methods of the same class.
 * </div>
 * <div>
 * - Whenever other method requires random generator [[RngManager]] should be passed. Then receiving method
 * requests new random generator from manager. Instance of [[RngManager]] may be passed further if needed.
 * </div>
 * <div>
 * Proper implementation of [[RngManager]] must provide next guarantee^*1^:
 * </div>
 * <div>
 * - all returned instances of [[UnsafeUniformRng]] must have new unique state no matter they were requested
 *   sequentially by one thread or concurrently.
 * </div>
 * <div>
 * *1 for some reasonably high amount of requests.
 * </div>
 */
trait RngManager {

  /**
   * Creates new instance of thread unsafe random generator with uniform distribution.
   *
   * Subsequent calls of method must provide instance of generators with different states
   * to avoid generating of overlapping random sequences.
   */
  def newUnsafeUniformRng(): UnsafeUniformRng
}
