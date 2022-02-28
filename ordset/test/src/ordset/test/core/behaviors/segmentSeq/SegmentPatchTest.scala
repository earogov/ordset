package ordset.test.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.Bound
import ordset.core.segmentSeq.*
import ordset.core.interval.IntervalRelation
import ordset.test.Label
import ordset.test.Label.*
import ordset.test.core.behaviors.TestCaseBase

trait SegmentPatchTest[E, D[X] <: Domain[X], V] {

  def patchCases: Seq[SegmentPatchTest.TestCase[E, D, V]]
}

object SegmentPatchTest {
  
  case class TestCase[E, D[X] <: Domain[X], V](
    override val labels: Set[Label],
    bound: Bound[E],
    patch: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) extends TestCaseBase(labels)
}