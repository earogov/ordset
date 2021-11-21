package ordset.core

import ordset.{AscOrder, Hash, AscDiscrete, AscBounded}
import ordset.util.label.Label

package object domain {

  object DomainLabels {
    val Bounded: Label = Label("Bounded")
    val Unbounded: Label = Label("Unbounded")
    val Continuous: Label = Label("Continuous")
    val Discrete: Label = Label("Discrete")
  }

  type OrderDir = OrderDirection.Type
  type AscDir = OrderDirection.Asc.Type
  type DescDir = OrderDirection.Desc.Type

  val AscDir: AscDir = OrderDirection.Asc.value
  val DescDir: DescDir = OrderDirection.Desc.value

  type AscOrder[E] = DirectedOrder[E, AscDir]
  type DescOrder[E] = DirectedOrder[E, DescDir]

  type AscDiscrete[E] = DirectedDiscrete[E, AscDir]
  type DescDiscrete[E] = DirectedDiscrete[E, DescDir]


  type ContinuousOrder[E] = AscOrder[E] with Hash[E]

  type ContinuousBoundedOrder[E] = AscOrder[E] with Hash[E] with AscBounded[E]

  type DiscreteOrder[E] = AscOrder[E] with Hash[E] with AscDiscrete[E]

  type DiscreteBoundedOrder[E] = AscOrder[E] with Hash[E] with AscDiscrete[E] with AscBounded[E]
}
