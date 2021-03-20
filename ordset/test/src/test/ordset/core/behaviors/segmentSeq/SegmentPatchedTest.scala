package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.{Bound, IntervalRelation, SegmentSeq}
import ordset.util.label.Label
import test.ordset.core.Labels

trait SegmentPatchedTest[E, D <: Domain[E], V] {

  def patchedCases: Seq[SegmentPatchedTest.TestCase[E, D, V]]
}

object SegmentPatchedTest {
  
  case class TestCase[E, D <: Domain[E], V](
    labels: Set[Label],
    bound: Bound[E],
    patch: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) {

    override def toString: String = Labels.caseShow.show(labels)
  }
}