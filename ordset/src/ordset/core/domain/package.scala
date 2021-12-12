package ordset.core

import ordset.util.label.Label

package object domain {

  object DomainLabels {
    val Bounded: Label = Label("Bounded")
    val Unbounded: Label = Label("Unbounded")
    val Continuous: Label = Label("Continuous")
    val Discrete: Label = Label("Discrete")
  }
}
