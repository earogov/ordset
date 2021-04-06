package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.{IntervalRelation, SegmentSeq}
import ordset.util.label.Label
import test.ordset.core.Labels

trait SegmentSeqAppendedV0Test[E, D <: Domain[E], V] {

  def appendedV0Cases: Seq[SegmentSeqAppendedV0Test.TestCase[E, D, V]]
}

object SegmentSeqAppendedV0Test {

  case class TestCase[E, D <: Domain[E], V](
    labels: Set[Label],
    appended: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) {

    override def toString: String = Labels.caseShow.show(labels)
  }
}
