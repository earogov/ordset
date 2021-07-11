package test.ordset.core.behaviors.segmentSeq

import ordset.core.{Bound, IntervalRelation}
import ordset.core.domain.Domain
import test.ordset.core.behaviors.TestShowUtil

trait SegmentSeqSlicedTest[E, D <: Domain[E], V] {

  def slicedCases: Seq[SegmentSeqSlicedTest.TestCase[E, D, V]]
}

object SegmentSeqSlicedTest {

  case class TestCase[E, D <: Domain[E], V](
    bound: Bound[E],
    expectedBelow: Seq[IntervalRelation[E, D, V]],
    expectedAbove: Seq[IntervalRelation[E, D, V]]
  ) {

    override def toString: String = TestShowUtil.caseWithBoundToString(bound)
  }
}
