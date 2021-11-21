package ordset

object DiscreteBoundedOrder {

  /**
   * Typeclass specifying discrete ordered set such that:
   * <tr>- it has lower bound;</tr>
   * <tr>- it has finite number of elements between lower bound and any element included in set.</tr>
   * 
   * <h3>Examples of finite (from below) bounded sets</h3>
   * <tr>1. {x ∈ N} - natural numbers, lower bound is 0.</tr>
   * <tr>2. {x ∈ N | x ≥ 5} - natural numbers starting from 5, lower bound is 5.</tr>
   * 
   * <h3>Examples of infinite (from below) bounded sets</h3>
   * <tr>
   *   1. {1/x | x ∈ N and x ≥ 1} - sequence of rational fractions ... 1/3, 1/2, 1/1.
   *      Lower bound is 0, but there is infinite number of elements.
   * </tr>
   */
  trait FiniteBelow[E] extends DiscreteOrder[E] with BoundedOrder.BoundedBelow.Including[E] {

    override def hasPredecessor(x: E): Boolean = includes(x) && !isLeastElement(x)
  }

  trait FiniteAbove[E] extends DiscreteOrder[E] with BoundedOrder.BoundedAbove.Including[E] {

    override def hasSuccessor(x: E): Boolean = includes(x) && !isGreatestElement(x)
  }

  trait Finite[E] extends FiniteBelow[E] with FiniteAbove[E]
}