package ordset.core.domain.order

import ordset.{Bounded, Directed, Order, Hash, Direction}

trait DiscreteBoundedOrder[E, Dir <: Direction] extends DiscreteOrder[E, Dir] with Bounded[E]

object DiscreteBoundedOrder {

  import ordset.{OrderComponents, HashComponents}

  abstract class ProxyImpl[E, Dir <: Direction](
    override val original: Order[E] with Hash[E]
  )(
    implicit dirValue: ValueOf[Dir]
  ) extends DiscreteBoundedOrder[E, Dir] 
    with OrderComponents.OrderProxy[E] 
    with HashComponents.HashProxy[E] {

    final override val direction: Dir = dirValue.value
  }
}