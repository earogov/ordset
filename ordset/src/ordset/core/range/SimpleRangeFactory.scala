package ordset.core.range

import ordset.{Order, BoundedOrder}

/**
 * Abstract factory to construct [[SimpleRange]].
 * 
 * @tparam E type of range elements.
 */
abstract class SimpleRangeFactory[E] extends RangeFactory[E, SimpleRange[E]] {

  final override val empty: SimpleRange.Empty.type = SimpleRange.Empty

  protected final def cons(lower: E, upper: E): SimpleRange.NonEmpty[E] = SimpleRange(lower, upper)
}

object SimpleRangeFactory {
  
  /**
   * Returns factory of [[SimpleRange]] on unbounded domain.
   */
  implicit def unbounded[E](implicit order: Order[E]): UnboundedImpl[E] = 
    new UnboundedImpl(order)

  /**
   * Returns factory of [[SimpleRange]] on bounded from below domain.
   */
  implicit def boundedBelow[E](implicit order: BoundedOrder.Below[E, E]): BoundedBelowImpl[E] = 
    new BoundedBelowImpl(order)

  /**
   * Returns factory of [[SimpleRange]] on bounded from above domain.
   */
  implicit def boundedAbove[E](implicit order: BoundedOrder.Above[E, E]): BoundedAboveImpl[E] = 
    new BoundedAboveImpl(order)

  /**
   * Returns factory of [[SimpleRange]] on bounded domain.
   */
  implicit def bounded[E](implicit order: BoundedOrder[E, E, E]): BoundedImpl[E] = 
    new BoundedImpl(order)

  /**
   * Constructs [[SimpleRange]] on unbounded domain.
   * 
   * @tparam E type of range elements.
   */
  class UnboundedImpl[E](
    override val order: Order[E]
  ) extends SimpleRangeFactory[E] 
    with RangeFactory.Unbounded[E, SimpleRange[E]]

  /**
   * Constructs [[SimpleRange]] on bounded from below domain.
   * 
   * @tparam E type of range elements.
   */
  class BoundedBelowImpl[E](
    override val order: BoundedOrder.Below[E, E]
  ) extends SimpleRangeFactory[E] 
    with RangeFactory.BoundedBelow[E, SimpleRange[E]]

  /**
   * Constructs [[SimpleRange]] on bounded from above domain.
   * 
   * @tparam E type of range elements.
   */
  class BoundedAboveImpl[E](
    override val order: BoundedOrder.Above[E, E]
  ) extends SimpleRangeFactory[E] 
    with RangeFactory.BoundedAbove[E, SimpleRange[E]]

  /**
   * Constructs [[SimpleRange]] on bounded domain.
   * 
   * @tparam E type of range elements.
   */
  class BoundedImpl[E](
    override val order: BoundedOrder[E, E, E]
  ) extends SimpleRangeFactory[E] 
    with RangeFactory.Bounded[E, SimpleRange[E]]
}
