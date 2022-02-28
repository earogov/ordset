package ordset.test.core.behaviors

import ordset.test.Label

class TestPackageBase(
  val labels: Set[Label]
) {

  override def toString: String = Label.showPackageSet(labels)
}
