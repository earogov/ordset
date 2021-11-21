package ordset.core.domain.order

import cats.kernel.{Order, Hash}
import ordset.{Directed, Direction}

trait ContinuousOrder[E, Dir <: Direction] extends Order[E] with Hash[E] with Directed[Dir]

object ContinuousOrder {

  import ordset.{OrderComponents, HashComponents}

  class ProxyImpl[E, Dir <: Direction](
    override val original: Order[E] with Hash[E]
  )(
    implicit dirValue: ValueOf[Dir]
  ) extends ContinuousOrder[E, Dir] 
    with OrderComponents.OrderProxy[E] 
    with HashComponents.HashProxy[E] {

    final override val direction: Dir = dirValue.value
  }
}