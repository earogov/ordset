package ordset.test.core.behaviors.zippedSeq

import ordset.core.Bound
import ordset.core.segmentSeq.*
import ordset.core.interval.IntervalRelation
import ordset.core.domain.Domain
import ordset.test.Label
import ordset.test.core.behaviors.TestCaseBase

trait OriginalSeqPatchTest[E, D[X] <: Domain[X], U1, U2] {

  def firstSeqPatchCases: Seq[OriginalSeqPatchTest.TestCase[E, D, U1]]

  def secondSeqPatchCases: Seq[OriginalSeqPatchTest.TestCase[E, D, U2]]
}

object OriginalSeqPatchTest {
  
  case class TestCase[E, D[X] <: Domain[X], U](
    override val labels: Set[Label],
    bound: Bound[E],
    patch: SegmentSeq[E, D, U],
    expected: Seq[IntervalRelation[E, D, U]]
  ) extends TestCaseBase(labels)
}

