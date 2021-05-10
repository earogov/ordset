package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.{Bound, IntervalRelation, SegmentSeq}
import ordset.util.label.Label
import test.ordset.core.Labels

trait SegmentSeqPrependedTest[E, D <: Domain[E], V] {

  def prependedCases: Seq[SegmentSeqPrependedTest.TestCase[E, D, V]]
}

object SegmentSeqPrependedTest {

  case class TestCase[E, D <: Domain[E], V](
    labels: Set[Label],
    bound: Bound[E],
    prepended: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) {

    override def toString: String = Labels.caseShow.show(labels)
  }
}