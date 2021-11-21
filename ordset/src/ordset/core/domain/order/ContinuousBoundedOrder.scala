package ordset.core.domain.order

import ordset.{Bounded, Directed, Direction, Order, Hash}

trait ContinuousBoundedOrder[E, Dir <: Direction] extends ContinuousOrder[E, Dir] with Bounded[E]

object ContinuousBoundedOrder {

  import ordset.{OrderComponents, HashComponents}

  abstract class ProxyImpl[E, Dir <: Direction](
    override val original: Order[E] with Hash[E]
  )(
    implicit dirValue: ValueOf[Dir]
  ) extends ContinuousBoundedOrder[E, Dir] 
    with OrderComponents.OrderProxy[E] 
    with HashComponents.HashProxy[E] {

    final override val direction: Dir = dirValue.value
  }
}