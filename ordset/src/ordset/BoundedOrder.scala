package ordset

/**
 * Typeclass specifying ordered set bounded from below and above.
 * 
 * Implementations must enforce conditions:
 * <tr>1. [[lowerBound]] `≤` [[upperBound]] according to the current order.</tr>
 */ 
trait BoundedOrder[E, +L <: E, +U <: E] extends BoundedOrder.Below[E, L] with BoundedOrder.Above[E, U] {

  /**
   * Returns `true`, if element is included in set specified by typeclass.
   * 
   * <p>More strictly:</p>
   * <tr>- returns `true`, if [[lowerBound]] `<` x `<` [[upperBound]];</tr>
   * <tr>- returns [[lowerBoundIncluded]], if x `=` [[lowerBound]];</tr>
   * <tr>- returns [[upperBoundIncluded]], if x `=` [[upperBound]];</tr>
   * <tr>- returns `false` otherwise.</tr>
   */
  override def includes(x: E): Boolean = aboveLowerBound(x) && belowUpperBound(x)
}

object BoundedOrder {

  /**
   * Typeclass specifying ordered set bounded from below.
   */ 
  trait Below[E, +L <: E] extends Order[E] with Bounded.Below[L] {

    /**
     * Returns `true`, if element is included in set specified by typeclass.
     * 
     * <p>More strictly:</p>
     * <tr>- returns `true`, if x `>` [[lowerBound]];</tr>
     * <tr>- returns [[lowerBoundIncluded]], if x `=` [[lowerBound]];</tr>
     * <tr>- returns `false` otherwise.</tr>
     */
    def includes(x: E): Boolean = aboveLowerBound(x)

    /**
     * Returns `true`, if element is below lower bound of the set.
     * 
     * <p>More strictly:</p>
     * <tr>- returns `true`, if x `<` [[lowerBound]];</tr>
     * <tr>- returns [[lowerBoundExcluded]], if x `=` [[lowerBound]];</tr>
     * <tr>- returns `false` otherwise.</tr>
     */
    def belowLowerBound(x: E): Boolean = 
      if lowerBoundIncluded then lt(x, lowerBound)
      else lteqv(x, lowerBound)

    /**
     * Returns `true`, if element is above lower bound of the set.
     * 
     * <p>More strictly:</p>
     * <tr>- returns `true`, if x `>` [[lowerBound]];</tr>
     * <tr>- returns [[lowerBoundIncluded]], if x `=` [[lowerBound]];</tr>
     * <tr>- returns `false` otherwise.</tr>
     */
    def aboveLowerBound(x: E): Boolean =
      if lowerBoundIncluded then gteqv(x, lowerBound)
      else gt(x, lowerBound)
  }

  object Below {

    /**
     * Typeclass specifying ordered set bounded from below. Lower bound is included in the set.
     */ 
    trait Including[E, +L <: E] extends BoundedOrder.Below[E, L] with Bounded.Below.Including[L] {

      /**
       * Returns `true`, if x is the least element included in the set.
       */ 
      def isLeastElement(x: E): Boolean = eqv(x, lowerBound)

      /**
       * Returns:
       * <tr>- [[lowerBound]], if x is below lower bound (see [[belowLowerBound]]);</tr>
       * <tr>- x otherwise.</tr>
       */ 
      def restrict(x: E): E = if belowLowerBound(x) then lowerBound else x
    }
  }

  /**
   * Typeclass specifying ordered set bounded from above.
   */ 
  trait Above[E, +U <: E] extends Order[E] with Bounded.Above[U] {

    /**
     * Returns `true`, if element is included in set specified by typeclass.
     * 
     * <p>More strictly:</p>
     * <tr>- returns `true`, if x `<` [[upperBound]];</tr>
     * <tr>- returns [[upperBoundIncluded]], if x `=` [[upperBound]];</tr>
     * <tr>- returns `false` otherwise.</tr>
     */
    def includes(x: E): Boolean = belowUpperBound(x)

    /**
     * Returns `true`, if element is below upper bound of the set.
     * 
     * <p>More strictly:</p>
     * <tr>- returns `true`, if x `<` [[upperBound]];</tr>
     * <tr>- returns [[upperBoundIncluded]], if x `=` [[upperBound]];</tr>
     * <tr>- returns `false` otherwise.</tr>
     */
    def belowUpperBound(x: E): Boolean =
      if upperBoundIncluded then lteqv(x, upperBound)
      else lt(x, upperBound)

    /**
     * Returns `true`, if element is above upper bound of the set.
     * 
     * <p>More strictly:</p>
     * <tr>- returns `true`, if x `>` [[upperBound]];</tr>
     * <tr>- returns [[upperBoundExcluded]], if x `=` [[upperBound]];</tr>
     * <tr>- returns `false` otherwise.</tr>
     */
    def aboveUpperBound(x: E): Boolean =
      if upperBoundIncluded then gt(x, upperBound)
      else gteqv(x, upperBound)
  }

  object Above {

    /**
     * Typeclass specifying ordered set bounded from above. Upper bound is included in the set.
     */ 
    trait Including[E, +U <: E] extends BoundedOrder.Above[E, U] with Bounded.Above.Including[U] {

      /**
       * Returns `true`, if x is the greatest element included in the set.
       */ 
      def isGreatestElement(x: E): Boolean = eqv(x, upperBound)

      /**
       * Returns:
       * <tr>- [[upperBound]], if x is above upper bound (see [[aboveUpperBound]]);</tr>
       * <tr>- x otherwise.</tr>
       */ 
      def restrict(x: E): E = if aboveUpperBound(x) then upperBound else x
    }
  }

  /**
   * Typeclass specifying ordered set bounded from below and above. Lower and upper bounds are included in the set.
   */ 
  trait Including[E, +L <: E, +U <: E] 
    extends BoundedOrder[E, L, U] 
    with BoundedOrder.Below.Including[E, L] 
    with BoundedOrder.Above.Including[E, U] 
    with Bounded.Including[L, U] {

    /**
     * Returns:
     * <tr>- [[lowerBound]], if x is below lower bound (see [[belowLowerBound]]);</tr>
     * <tr>- [[upperBound]], if x is above upper bound (see [[aboveUpperBound]]);</tr>
     * <tr>- x otherwise.</tr>
     */ 
    override def restrict(x: E): E = 
      if belowLowerBound(x) then lowerBound
      else if aboveUpperBound(x) then upperBound
      else x
  }
}