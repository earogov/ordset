package ordset.core.domain.order

import cats.kernel.{Order, Hash}
import ordset.{Directed, Discrete, Direction}

trait DiscreteOrder[E, Dir <: Direction] extends Order[E] with Hash[E] with Discrete[E] with Directed[Dir]

object DiscreteOrder {

  import ordset.{OrderComponents, HashComponents}

  abstract class ProxyImpl[E, Dir <: Direction](
    override val original: Order[E] with Hash[E]
  )(
    implicit dirValue: ValueOf[Dir]
  ) extends DiscreteOrder[E, Dir] 
    with OrderComponents.OrderProxy[E] 
    with HashComponents.HashProxy[E] {

    final override val direction: Dir = dirValue.value
  }
}