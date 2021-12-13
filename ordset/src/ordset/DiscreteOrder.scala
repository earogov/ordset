package ordset

/**
 * Typeclass specifying discrete ordered set.
 * 
 * Implementations MUST enforce conditions:
 * <tr>
 *   1.a If `x` has successor, then: `successor(x) > x` according to the current order.
 * </tr>
 * <tr>
 *   1.b Otherwise `∄` `y` included in set such that: `y > x`. 
 * </tr>
 * <tr>
 *   2.a If `x` has predecessor, then: `predecessor(x) < x` according to the current order.
 * </tr>
 * <tr>
 *   2.b Otherwise `∄` `y` included in set such that: `y < x`. 
 * </tr>
 * 
 * See also [[Discrete]] conditions.
 * 
 * For any discrete ordered set conditions 1 and 2 of current class with conditions 1, 2, 3 of [[Discrete]] 
 * gives the only valid output for `successor` and `predecessor` operators.
 * 
 * Let's consider the case when typeclass represents a set `A` which is a union of disjoint subsets:
 * 
 * `A = {2, 3} U {5, 6}`
 * 
 * Then `successor` and `predecessor` operators must return:
 * <tr>- `predecessor(1) = null`</tr>   
 * <tr>- `successor(1) = null`  </tr>   
 * <tr>- `predecessor(2) = null`</tr> 
 * <tr>- `successor(2) = 3`     </tr> 
 * <tr>- `predecessor(3) = 2`   </tr> 
 * <tr>- `successor(3) = 5`     </tr> 
 * <tr>- `predecessor(4) = null`</tr>   
 * <tr>- `successor(4) = null`  </tr>   
 * <tr>- `predecessor(5) = 3`   </tr> 
 * <tr>- `successor(5) = 6`     </tr> 
 * <tr>- `predecessor(6) = 5`   </tr>
 * <tr>- `successor(6) = null`  </tr>
 * <tr>- `predecessor(7) = null`</tr>   
 * <tr>- `successor(7) = null`  </tr>   
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

object DiscreteOrder {

  /**
   * Typeclass specifying discrete ordered set with finite number of elements. Set has both lower and upper bounds.
   * 
   * See conditions of [[DiscreteOrder]], [[Finite.Below]] and [[Finite.Above]].
   */
  trait Finite[E, +L <: E, +U <: E] 
    extends BoundedOrder.Including[E, L, U]
    with Finite.Below[E, L] 
    with Finite.Above[E, U] 

  object Finite {

    /**
     * Typeclass specifying discrete ordered set such that:
     * <tr>
     *   1. it has lower bound;
     * </tr>
     * <tr>
     *   2. there is some element `x` included in set such that: 
     *      number of elements between lower bound and `x` is finite.
     * </tr>
     * 
     * See also conditions of [[DiscreteOrder]].
     * 
     * Due to condition 2 of current class greatest lower bound is always included in set 
     * (see [[BoundedOrder.Below.Including]]). 
     * This allows to provide general implementation of [[Finite.Below.hasPredecessor]].
     * 
     * <h3>Examples of finite from below bounded sets</h3>
     * <tr>1. `{x ∈ N}` - natural numbers, lower bound is 0.</tr>
     * <tr>2. `{x ∈ String}` - all strings with lexicographical order, lower bound is empty string.</tr>
     * 
     * <h3>Examples of infinite from below bounded sets</h3>
     * <tr>
     *   1. `{1/x | x ∈ N and x ≥ 1}` - sequence of rational fractions ... 1/3, 1/2, 1/1.
     *      Lower bound is 0, but there is infinite number of elements.
     * </tr>
     */
    trait Below[E, +L <: E] extends DiscreteOrder[E] with BoundedOrder.Below.Including[E, L] {

      override def hasPredecessor(x: E): Boolean = includes(x) && !isLeastElement(x)
    }

    /**
     * Typeclass specifying discrete ordered set such that:
     * <tr>
     *   1. it has upper bound;
     * </tr>
     * <tr>
     *   2. there is some element `x` included in set such that: 
     *      number of elements between `x` and upper bound is finite.
     * </tr>
     * 
     * See also conditions of [[DiscreteOrder.]].
     * 
     * Due to condition 2 of current class greatest lower bound is always included in set 
     * (see [[BoundedOrder.Above.Including]]). 
     * This allows to provide general implementation of [[Finite.Above.hasSuccessor]].
     * 
     * <h3>Examples of finite from above bounded sets</h3>
     * <tr>1. `{-x | x ∈ N}` - non-positive numbers, upper bound is 0.</tr>
     * <tr>2. `{x ∈ String}` - all strings with reversed lexicographical order, upper bound is empty string.</tr>
     * 
     * <h3>Examples of infinite from above bounded sets</h3>
     * <tr>
     *   1. `{-1/x | x ∈ N and x ≥ 1}` - sequence of rational fractions -1/1, -1/2, -1/3 ...
     *      Upper bound is 0, but there is infinite number of elements.
     * </tr>
     */
    trait Above[E, +U <: E] extends DiscreteOrder[E] with BoundedOrder.Above.Including[E, U] {

      override def hasSuccessor(x: E): Boolean = includes(x) && !isGreatestElement(x)
    }
  }
}