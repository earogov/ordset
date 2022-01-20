package ordset.core.range

import ordset.{ContravariantOrder, ContravariantBoundedOrder}

/**
 * Abstract factory to construct [[SimpleRange]].
 * 
 * @tparam E type of range elements.
 */
abstract class SimpleRangeFactory[E] extends RangeFactory[E, SimpleRange] {

  final override val empty: SimpleRange.Empty.type = SimpleRange.Empty

  protected final def cons(lower: E, upper: E): SimpleRange.NonEmpty[E] = SimpleRange(lower, upper)
}

object SimpleRangeFactory {
  
  /**
   * Returns factory of [[SimpleRange]] on unbounded domain.
   */
  implicit def unbounded[E](implicit order: ContravariantOrder[E]): UnboundedImpl[E] = 
    new UnboundedImpl(order)

  /**
   * Returns factory of [[SimpleRange]] on bounded from below domain.
   */
  implicit def boundedBelow[E, L <: E](implicit order: ContravariantBoundedOrder.Below[E, L]): BoundedBelowImpl[E, L] = 
    new BoundedBelowImpl(order)

  /**
   * Returns factory of [[SimpleRange]] on bounded from above domain.
   */
  implicit def boundedAbove[E, U <: E](implicit order: ContravariantBoundedOrder.Above[E, U]): BoundedAboveImpl[E, U] = 
    new BoundedAboveImpl(order)

  /**
   * Returns factory of [[SimpleRange]] on bounded domain.
   */
  implicit def bounded[E, L <: E, U <: E](implicit order: ContravariantBoundedOrder[E, L, U]): BoundedImpl[E, L, U] = 
    new BoundedImpl(order)

  /**
   * Constructs [[SimpleRange]] on unbounded domain.
   * 
   * @tparam E type of range elements.
   */
  class UnboundedImpl[E](
    override val order: ContravariantOrder[E]
  ) extends SimpleRangeFactory[E] 
    with RangeFactory.Unbounded[E, SimpleRange]

  /**
   * Constructs [[SimpleRange]] on bounded from below domain.
   * 
   * @tparam E type of range elements.
   * @tparam L type of lower bound of domain.
   */
  class BoundedBelowImpl[E, +L <: E](
    override val order: ContravariantBoundedOrder.Below[E, L]
  ) extends SimpleRangeFactory[E]
    with RangeFactory.BoundedBelow[E, L, SimpleRange]

  /**
   * Constructs [[SimpleRange]] on bounded from above domain.
   * 
   * @tparam E type of range elements.
   * @tparam U type of upper bound of domain.
   */
  class BoundedAboveImpl[E, +U <: E](
    override val order: ContravariantBoundedOrder.Above[E, U]
  ) extends SimpleRangeFactory[E] 
    with RangeFactory.BoundedAbove[E, U, SimpleRange]

  /**
   * Constructs [[SimpleRange]] on bounded domain.
   * 
   * @tparam E type of range elements.
   * @tparam L type of lower bound of domain.
   * @tparam U type of upper bound of domain.
   */
  class BoundedImpl[E, +L <: E, +U <: E](
    override val order: ContravariantBoundedOrder[E, L, U]
  ) extends SimpleRangeFactory[E] 
    with RangeFactory.Bounded[E, L, U, SimpleRange]
}
