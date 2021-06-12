package test.ordset.core.behaviors

import ordset.util.label.Label
import test.ordset.core.Labels

class TestCaseBase(
  val labels: Set[Label]
) {

  override def toString: String = Labels.caseShow.show(labels)
}
