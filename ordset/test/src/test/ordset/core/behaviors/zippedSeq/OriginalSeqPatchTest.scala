package test.ordset.core.behaviors.zippedSeq

import ordset.core.{Bound, IntervalRelation, SegmentSeq}
import ordset.core.domain.Domain
import ordset.util.label.Label
import test.ordset.core.behaviors.TestCaseBase

trait OriginalSeqPatchTest[E, D <: Domain[E], U1, U2] {

  def firstSeqPatchCases: Seq[OriginalSeqPatchTest.TestCase[E, D, U1]]

  def secondSeqPatchCases: Seq[OriginalSeqPatchTest.TestCase[E, D, U2]]
}

object OriginalSeqPatchTest {
  
  case class TestCase[E, D <: Domain[E], U](
    override val labels: Set[Label],
    bound: Bound[E],
    patch: SegmentSeq[E, D, U],
    expected: Seq[IntervalRelation[E, D, U]]
  ) extends TestCaseBase(labels)
}

