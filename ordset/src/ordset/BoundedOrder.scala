package ordset

/**
 * Typeclass specifying ordered set bounded from below and above.
 * 
 * Implementations must enforce conditions:
 * <tr>1. [[lowerBound]] `≤` [[upperBound]] according to the current order.</tr>
 */ 
trait BoundedOrder[E, +L <: E, +U <: E] 
  extends BoundedOrder.Below[E, L] 
  with BoundedOrder.Above[E, U]
  with Reversible[BoundedOrder[E, L, U], BoundedOrder[E, U, L]] {

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

  override def reversed: BoundedOrder[E, U, L] = new BoundedOrder.ReversedImpl(this)
}

object BoundedOrder {

  /**
   * Checks condition 1 of [[BoundedOrder]] and throws [[IllegalArgumentException]] if validation is failed.
   */
  @throws[IllegalArgumentException]("if validation is failed")
  def validateBounds[E, L <: E, U <: E](order: BoundedOrder[E, L, U]): order.type = {
    if order.gt(order.lowerBound, order.upperBound) then 
      throw new IllegalArgumentException(
        s"Invalid bounded order: lower bound ${order.lowerBound} is greater than upper bound ${order.upperBound}"
      )
    else order
  }

  /**
   * Typeclass specifying ordered set bounded from below.
   */ 
  trait Below[E, +L <: E] extends Order[E] with Bounded.Below[L] with Reversible[Below[E, L], Above[E, L]] {

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

    override def reversed: Above[E, L] = new Above.ReversedImpl(this)
  }

  object Below {

    /**
     * Typeclass specifying ordered set bounded from below. Lower bound is included in the set.
     */ 
    trait Including[E, +L <: E] 
      extends Below[E, L] 
      with Bounded.Below.Including[L]
      with Reversible[Below.Including[E, L], Above.Including[E, L]] {

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

      override def reversed: Above.Including[E, L] = new Above.Including.ReversedImpl(this)
    }

    object Including {

      /**
       * [[BoundedOrder.Below.Including]] typeclass received by reverting [[BoundedOrder.Above.Including]] instance.
       */
      trait Reversed[E, +L <: E] 
        extends Below.Reversed[E, L] 
        with Bounded.Below.Including.Reversed[L] 
        with Below.Including[E, L]

      class ReversedImpl[E, +L <: E](original: Above.Including[E, L]) extends Reversed[E, L] {

        override val reversed: Above.Including[E, L] = original
      }
    }

    /**
     * [[BoundedOrder.Below]] typeclass received by reverting [[BoundedOrder.Above]] instance.
     */
    trait Reversed[E, +L <: E] extends Additional.Order.Reversed[E] with Bounded.Below.Reversed[L] with Below[E, L]

    class ReversedImpl[E, +L <: E](original: Above[E, L]) extends Reversed[E, L] {

      override val reversed: Above[E, L] = original
    }
  }

  /**
   * Typeclass specifying ordered set bounded from above.
   */ 
  trait Above[E, +U <: E] extends Order[E] with Bounded.Above[U] with Reversible[Above[E, U], Below[E, U]] {

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

    override def reversed: Below[E, U] = new Below.ReversedImpl(this)
  }

  object Above {

    /**
     * Typeclass specifying ordered set bounded from above. Upper bound is included in the set.
     */ 
    trait Including[E, +U <: E] 
      extends Above[E, U] 
      with Bounded.Above.Including[U]
      with Reversible[Above.Including[E, U], Below.Including[E, U]] {

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

      override def reversed: Below.Including[E, U] = new Below.Including.ReversedImpl(this)
    }

    object Including {

      /**
       * [[BoundedOrder.Above.Including]] typeclass received by reverting [[BoundedOrder.Below.Including]] instance.
       */
      trait Reversed[E, +U <: E] 
        extends Above.Reversed[E, U] 
        with Bounded.Above.Including.Reversed[U]
        with Above.Including[E, U]

      class ReversedImpl[E, +U <: E](original: Below.Including[E, U]) extends Reversed[E, U] {

        override val reversed: Below.Including[E, U] = original
      }
    }

    /**
     * [[BoundedOrder.Above]] typeclass received by reverting [[BoundedOrder.Below]] instance.
     */
    trait Reversed[E, +U <: E] extends Additional.Order.Reversed[E] with Bounded.Above.Reversed[U] with Above[E, U]

    class ReversedImpl[E, +U <: E](original: Below[E, U]) extends Reversed[E, U] {

      override val reversed: Below[E, U] = original
    }
  }

  /**
   * Typeclass specifying ordered set bounded from below and above. Lower and upper bounds are included in the set.
   */ 
  trait Including[E, +L <: E, +U <: E] 
    extends BoundedOrder[E, L, U] 
    with Below.Including[E, L] 
    with Above.Including[E, U] 
    with Bounded.Including[L, U]
    with Reversible[Including[E, L, U], Including[E, U, L]] {

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

    override def reversed: Including[E, U, L] = new Including.ReversedImpl(this)
  }

  object Including {

    /**
     * [[BoundedOrder.Including]] typeclass received by reverting another [[BoundedOrder.Including]] instance.
     */
    trait Reversed[E, +L <: E, +U <: E]
      extends BoundedOrder.Reversed[E, L, U]
      with Below.Including.Reversed[E, L] 
      with Above.Including.Reversed[E, U]
      with Bounded.Including.Reversed[L, U]
      with Including[E, L, U]

    class ReversedImpl[E, +L <: E, +U <: E](original: Including[E, U, L]) extends Reversed[E, L, U] {

      override def reversed: Including[E, U, L] = original
    }
  }

  /**
   * [[BoundedOrder]] typeclass received by reverting another [[BoundedOrder]] instance.
   */
  trait Reversed[E, +L <: E, +U <: E] 
    extends Below.Reversed[E, L] 
    with Above.Reversed[E, U] 
    with BoundedOrder[E, L, U]

  class ReversedImpl[E, +L <: E, +U <: E](original: BoundedOrder[E, U, L]) extends Reversed[E, L, U] {

    override def reversed: BoundedOrder[E, U, L] = original
  }
}