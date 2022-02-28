package ordset.test.core.behaviors

import ordset.test.Label

class TestCaseBase(
  val labels: Set[Label]
) {

  override def toString: String = Label.showCaseSet(labels)
}
