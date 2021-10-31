package ordset.test.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.{Bound, SegmentSeq}
import ordset.core.interval.IntervalRelation
import ordset.util.label.Label
import ordset.test.core.Labels
import ordset.test.core.behaviors.TestCaseBase

trait SegmentPatchTest[E, D <: Domain[E], V] {

  def patchCases: Seq[SegmentPatchTest.TestCase[E, D, V]]
}

object SegmentPatchTest {
  
  case class TestCase[E, D <: Domain[E], V](
    override val labels: Set[Label],
    bound: Bound[E],
    patch: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) extends TestCaseBase(labels)
}