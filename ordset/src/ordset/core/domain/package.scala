package ordset.core

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
}
