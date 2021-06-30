package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.{Bound, IntervalRelation, SegmentSeq}
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.behaviors.TestCaseBase

trait SegmentSeqAppendedTest[E, D <: Domain[E], V] {

  def appendedCases: Seq[SegmentSeqAppendedTest.TestCase[E, D, V]]
  
  def appendedWithBoundCases: Seq[SegmentSeqAppendedTest.TestCaseWithBound[E, D, V]]
}

object SegmentSeqAppendedTest {

  case class TestCase[E, D <: Domain[E], V](
    override val labels: Set[Label],
    appended: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) extends TestCaseBase(labels)

  case class TestCaseWithBound[E, D <: Domain[E], V](
    override val labels: Set[Label],
    bound: Bound[E],
    appended: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) extends TestCaseBase(labels) 
}
