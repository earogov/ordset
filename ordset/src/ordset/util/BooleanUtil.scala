package ordset.util

object BooleanUtil {

  val truePredicate1: Any => Boolean = _ => true

  val falsePredicate1: Any => Boolean = _ => false

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
