package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.{IntervalRelation, SegmentSeq}
import ordset.util.label.Label
import test.ordset.core.Labels

trait SegmentSeqAppendedTest[E, D <: Domain[E], V] {

  def appendedCases: Seq[SegmentSeqAppendedTest.TestCase[E, D, V]]
}

object SegmentSeqAppendedTest {

  case class TestCase[E, D <: Domain[E], V](
    labels: Set[Label],
    appended: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) {

    override def toString: String = Labels.caseShow.show(labels)
  }
}
