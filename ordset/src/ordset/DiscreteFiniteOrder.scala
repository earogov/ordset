package ordset

/**
 * Typeclass specifying discrete ordered set with finite number of elements. Set has both lower and upper bounds.
 * 
 * @see [[DiscreteFiniteOrder.Below]] and [[DiscreteFiniteOrder.Above]]
 */
trait DiscreteFiniteOrder[E, +L <: E, +U <: E] 
  extends DiscreteFiniteOrder.Below[E, L] 
  with DiscreteFiniteOrder.Above[E, U] 
  with BoundedOrder.Including[E, L, U]

object DiscreteFiniteOrder {

  /**
   * Typeclass specifying discrete ordered set such that:
   * <tr>- it has lower bound;</tr>
   * <tr>- it has finite number of elements between lower bound and any element included in set.</tr>
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
   * <tr>- it has upper bound;</tr>
   * <tr>- it has finite number of elements between upper bound and any element included in set.</tr>
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