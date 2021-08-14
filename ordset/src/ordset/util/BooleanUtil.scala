package ordset.util

object BooleanUtil {

  /**
   * Takes `initial` value and applies inversion operation to it `n` times.
   *
   * Each inversion flips boolean value:
   * {{{
   *   false -> true
   *   true -> false
   * }}}
   *
   * Note, for negative `n`:
   * {{{
   *   inverseN(initial, -n) == inverseN(initial, n)
   * }}}
   */
  def inverseN(initial: Boolean, n: Int): Boolean = initial ^ ((n & 0x00000001) == 0x00000001)
}
