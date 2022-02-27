package ordset.test.core

import ordset.random.{RngManager, UnsafeUniformRng}

import scala.collection.immutable.ArraySeq

object RandomUtil {

  /**
   * @return random element from collection. If `seq` is empty returns [[Option.empty]].
   */
  def randomPick[E](seq: IndexedSeq[E], rnd: Int): Option[E] = {
    if (seq.isEmpty) Option.empty
    else Option(seq(math.abs(rnd) % seq.length))
  }

  /**
   * Breaks input `sum` into random positive terms such that:
   * {{{
   *   outputSeq.reduce(_ + _) == `sum`
   * }}}
   * If `sum` `<=` 0 then returns [[Seq.empty]].
   * <div></div>
   *
   * Probability distribution of terms is specified by input function `p`:
   * <div>
   *   1. Assuming that we have already generated terms that give T in total, calculate rest: R = `sum` - T.
   *   If R == 0 then stop.
 *   </div>
   * <div>
   *   2. Select candidate term randomly with uniform distribution in range [1, R].
   * </div>
   * <div>
   *   3. Generate random double with uniform distribution in range [0, 1].
   * </div>
   * <div>
   *   4. Call function `p` with arguments (candidate term / R, random double).
   *   If result is `true` term is accepted and return to step 1, otherwise return to step 2.
   * </div>
   * <div></div>
   * 
   * Note, infinite loop is possible if `p` always returns `false`.
   * <div></div>
   * 
   * Finally received sequence of terms is sorted randomly.
   */
  def randomTerms(sum: Int, p: (Double, Double) => Boolean)(implicit rngManager: RngManager): Seq[Int] = {

    def genTerm(max: Int, rng: UnsafeUniformRng): Int =
      if (max == 1) max
      else {
        var term = 0
        var stop = false
        while (!stop) {
          term = rng.nextInt(max) + 1
          stop = p((term: Double) / max, rng.nextDouble())
        }
        term
      }

    if (sum <= 0) Seq.empty
    else {
      var rest = sum
      val builder = ArraySeq.newBuilder[Int]
      val rng = rngManager.newUnsafeUniformRng()
      while (rest > 0) {
        val term = genTerm(rest, rng)
        builder.addOne(term)
        rest = rest - term
      }
      builder.result()
        .map(term => (rng.nextDouble(), term))
        .sorted(Ordering.by(_._1))
        .map(_._2)
    }
  }

  /**
   * Functions that define probability distribution of some random quantity x that belongs to [0, 1] (double).
   * <div>First argument - some random value of quantity x.</div>
   * <div>Second argument - random double that belong to [0, 1] which is used as a source of randomness.</div>
   * <div>If function returns `true` then random value of quantity x should be accepted, otherwise - rejected.</div>
   */
  object Distribution {

    /**
     * Small values of quantity x are more likely.
     * {{{
     *     y
     *  1 |'  '  '  '  ' 1
     *    |   
     *    |      
     *    |         
     *    |
     *    |------------|
     *    0            1 x
     *
     *    where
     *    x - random value for test.
     *    y - probability to get `true`, i.e. to accept value x.
     * }}}
     */
    val uniform: (Double, Double) => Boolean = (_, _) => true

    /**
     * Small values of quantity x are more likely.
     * {{{
     *     y
     *  1 |'
     *    |   '
     *    |      '
     *    |         '
     *    |            ' 0.1
     *    |------------|
     *    0            1 x
     *
     *    where
     *    x - random value for test.
     *    y - probability to get `true`, i.e. to accept value x.
     * }}}
     */
    val smallPreferablePow1: (Double, Double) => Boolean = (x, d) => d <= 1 - x * 0.9

    /**
     * Small values of quantity x are more likely.
     * {{{
     *     y
     *  1 |'
     *    | '
     *    |   '
     *    |       '
     *    |            ' 0.09
     *    |------------|
     *    0            1 x
     *
     *    where
     *    x - random value for test.
     *    y - probability to get `true`, i.e. to accept value x.
     * }}}
     */
    val smallPreferablePow2: (Double, Double) => Boolean = (x, d) => d <= math.pow(x - 1.3, 2)
  }
}
