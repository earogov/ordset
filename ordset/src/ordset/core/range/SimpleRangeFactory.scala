package ordset.core.range

import ordset.{Order, BoundedOrder, ContravariantOrder, ContravariantBoundedOrder}
import ordset.OrderExtensions._

/**
 * Abstract factory to construct [[SimpleRange]].
 * 
 * @tparam E1 lower type bound of range elements.
 * @tparam E2 upper type bound of range elements.
 */
abstract class SimpleRangeFactory[+E1 <: E2, -E2] extends RangeFactory[E1, E2, SimpleRange] {

  final override val empty: SimpleRange.Empty.type = SimpleRange.Empty

  /**
   * Returns range of elements between `lower` (including) and `upper` (including).
   * 
   * Preconditions:
   * <tr>1. `lower ≤ upper`</tr>
   * 
   * @tparam E type of range elements.
   */
  protected final def cons[E >: E1 <: E2](lower: E, upper: E): SimpleRange.NonEmpty[E] = SimpleRange(lower, upper)
}

object SimpleRangeFactory {
  
  /**
   * Returns factory of [[SimpleRange]] on unbounded domain.
   * 
   * @tparam E type of range elements.
   */
  implicit def unbounded[E](implicit order: Order[E]): UnboundedImpl[Nothing, E] = 
    new UnboundedImpl(order.contravariant)

  /**
   * Returns factory of [[SimpleRange]] on bounded from below domain.
   * 
   * @tparam E type of range elements.
   * @tparam L type of domain lower bound.
   */
  implicit def boundedBelow[E, L <: E](implicit order: BoundedOrder.Below[E, L]): BoundedBelowImpl[L, E] = 
    new BoundedBelowImpl(order.contravariant)

  /**
   * Returns factory of [[SimpleRange]] on bounded from above domain.
   * 
   * @tparam E type of range elements.
   * @tparam U type of domain upper bound.
   */
  implicit def boundedAbove[E, U <: E](implicit order: BoundedOrder.Above[E, U]): BoundedAboveImpl[U, E] = 
    new BoundedAboveImpl(order.contravariant)

  /**
   * Returns factory of [[SimpleRange]] on bounded domain.
   * 
   * @tparam E type of range elements.
   * @tparam B type of domain bounds.
   */
  implicit def bounded[E, B <: E](implicit order: BoundedOrder[E, B, B]): BoundedImpl[B, E] = 
    new BoundedImpl(order.contravariant)

  /**
   * Constructs [[SimpleRange]] on unbounded domain.
   * 
   * @tparam E1 lower type bound of range elements.
   * @tparam E2 upper type bound of range elements.
   */
  class UnboundedImpl[+E1 <: E2, -E2](
    override val order: ContravariantOrder[E2]
  ) extends SimpleRangeFactory[E1, E2] 
    with RangeFactory.Unbounded[E1, E2, SimpleRange]

  /**
   * Constructs [[SimpleRange]] on bounded from below domain.
   * 
   * @tparam E1 lower type bound of range elements.
   * @tparam E2 upper type bound of range elements.
   */
  class BoundedBelowImpl[+E1 <: E2, -E2](
    override val order: ContravariantBoundedOrder.Below[E2, E1]
  ) extends SimpleRangeFactory[E1, E2]
    with RangeFactory.BoundedBelow[E1, E2, SimpleRange]

  /**
   * Constructs [[SimpleRange]] on bounded from above domain.
   * 
   * @tparam E1 lower type bound of range elements.
   * @tparam E2 upper type bound of range elements.
   */
  class BoundedAboveImpl[+E1 <: E2, -E2](
    override val order: ContravariantBoundedOrder.Above[E2, E1]
  ) extends SimpleRangeFactory[E1, E2] 
    with RangeFactory.BoundedAbove[E1, E2, SimpleRange]

  /**
   * Constructs [[SimpleRange]] on bounded domain.
   * 
   * @tparam E1 lower type bound of range elements.
   * @tparam E2 upper type bound of range elements.
   */
  class BoundedImpl[+E1 <: E2, -E2](
    override val order: ContravariantBoundedOrder[E2, E1, E1]
  ) extends SimpleRangeFactory[E1, E2] 
    with RangeFactory.Bounded[E1, E2, SimpleRange]
}
