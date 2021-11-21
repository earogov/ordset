package ordset

/**
 * Typeclass specifying discrete ordered set.
 * 
 * Implementations must enforce conditions:
 * <tr>1. successor(x) `>` x according to the current order.</tr>
 * <tr>2. predecessor(x) `<` x according to the current order.</tr>
 * 
 * See also [[Discrete]] conditions.
 */ 
trait DiscreteOrder[E] extends Order[E] with Discrete[E] {

  /**
   * Returns `true`, if `y` is successor of `x`.
   */
  def isSuccessor(x: E, y: E): Boolean = {
    val sx: E | Null = successorOrNull(x)
    if sx != null then eqv(sx, y) else false
  }

  /**
   * Returns `true`, if `y` is successor of `x` or vice versa.
   */
  def isAdjacent(x: E, y: E): Boolean =
    isSuccessor(x, y) || isSuccessor(y, x)
}