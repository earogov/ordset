package test.ordset.core.behaviors

import ordset.util.label.Label
import test.ordset.core.Labels

class TestPackageBase(
  val labels: Set[Label]
) {

  override def toString: String = Labels.packageShow.show(labels)
}
