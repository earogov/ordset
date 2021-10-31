package ordset.test.core.behaviors

import ordset.util.label.Label
import ordset.test.core.Labels

class TestPackageBase(
  val labels: Set[Label]
) {

  override def toString: String = Labels.packageShow.show(labels)
}
