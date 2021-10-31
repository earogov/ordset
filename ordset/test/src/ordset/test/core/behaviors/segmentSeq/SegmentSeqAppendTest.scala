package ordset.test.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.{ExtendedBound, SegmentSeq}
import ordset.core.interval.IntervalRelation
import ordset.util.label.Label
import ordset.test.core.Labels
import ordset.test.core.behaviors.TestCaseBase

trait SegmentSeqAppendTest[E, D <: Domain[E], V] {

  def appendCases: Seq[SegmentSeqAppendTest.TestCase[E, D, V]]
  
  def appendAboveBoundCases: Seq[SegmentSeqAppendTest.TestCaseWithBound[E, D, V]]
}

object SegmentSeqAppendTest {

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
