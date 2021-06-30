package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.{Bound, IntervalRelation, SegmentSeq}
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.behaviors.TestCaseBase

trait SegmentSeqPrependedTest[E, D <: Domain[E], V] {

  def prependedCases: Seq[SegmentSeqPrependedTest.TestCase[E, D, V]]
  
  def prependedWithBoundCases: Seq[SegmentSeqPrependedTest.TestCaseWithBound[E, D, V]]
}

object SegmentSeqPrependedTest {

  case class TestCase[E, D <: Domain[E], V](
    override val labels: Set[Label],
    prepended: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) extends TestCaseBase(labels)
  
  case class TestCaseWithBound[E, D <: Domain[E], V](
    override val labels: Set[Label],
    bound: Bound[E],
    prepended: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) extends TestCaseBase(labels)
}