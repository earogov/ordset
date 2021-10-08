package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.{ExtendedBound, SegmentSeq}
import ordset.core.interval.IntervalRelation
import ordset.util.label.Label
import test.ordset.core.Labels
import test.ordset.core.behaviors.TestCaseBase

trait SegmentSeqPrependTest[E, D <: Domain[E], V] {

  def prependCases: Seq[SegmentSeqPrependTest.TestCase[E, D, V]]
  
  def prependBelowBoundCases: Seq[SegmentSeqPrependTest.TestCaseWithBound[E, D, V]]
}

object SegmentSeqPrependTest {

  case class TestCase[E, D <: Domain[E], V](
    override val labels: Set[Label],
    otherSeq: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) extends TestCaseBase(labels)
  
  case class TestCaseWithBound[E, D <: Domain[E], V](
    override val labels: Set[Label],
    bound: ExtendedBound[E],
    otherSeq: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) extends TestCaseBase(labels)
}