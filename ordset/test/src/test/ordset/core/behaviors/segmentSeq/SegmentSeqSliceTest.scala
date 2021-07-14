package test.ordset.core.behaviors.segmentSeq

import ordset.core.{ExtendedBound, IntervalRelation}
import ordset.core.domain.Domain
import test.ordset.core.behaviors.TestShowUtil

trait SegmentSeqSliceTest[E, D <: Domain[E], V] {

  def sliceCases: Seq[SegmentSeqSliceTest.TestCase[E, D, V]]
}

object SegmentSeqSliceTest {

  case class TestCase[E, D <: Domain[E], V](
    bound: ExtendedBound[E],
    expectedBelow: Seq[IntervalRelation[E, D, V]],
    expectedAbove: Seq[IntervalRelation[E, D, V]]
  ) {

    override def toString: String = TestShowUtil.caseWithBoundToString(bound)
  }
}
