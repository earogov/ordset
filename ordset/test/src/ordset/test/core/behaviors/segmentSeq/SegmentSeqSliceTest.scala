package ordset.test.core.behaviors.segmentSeq

import ordset.core.ExtendedBound
import ordset.core.interval.IntervalRelation
import ordset.core.domain.Domain
import ordset.test.core.behaviors.TestShowUtil

trait SegmentSeqSliceTest[E, D[X] <: Domain[X], V] {

  def sliceCases: Seq[SegmentSeqSliceTest.TestCase[E, D, V]]
}

object SegmentSeqSliceTest {

  case class TestCase[E, D[X] <: Domain[X], V](
    bound: ExtendedBound[E],
    expectedBelow: Seq[IntervalRelation[E, D, V]],
    expectedAbove: Seq[IntervalRelation[E, D, V]]
  ) {

    override def toString: String = TestShowUtil.caseWithBoundToString(bound)
  }
}
