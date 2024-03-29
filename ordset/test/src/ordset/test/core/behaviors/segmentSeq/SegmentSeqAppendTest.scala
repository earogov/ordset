package ordset.test.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.ExtendedBound
import ordset.core.segmentSeq.*
import ordset.core.interval.IntervalRelation
import ordset.test.Label
import ordset.test.Label.*
import ordset.test.core.behaviors.TestCaseBase

trait SegmentSeqAppendTest[E, D[X] <: Domain[X], V] {

  def appendCases: Seq[SegmentSeqAppendTest.TestCase[E, D, V]]
  
  def appendAboveBoundCases: Seq[SegmentSeqAppendTest.TestCaseWithBound[E, D, V]]
}

object SegmentSeqAppendTest {

  case class TestCase[E, D[X] <: Domain[X], V](
    override val labels: Set[Label],
    otherSeq: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) extends TestCaseBase(labels)

  case class TestCaseWithBound[E, D[X] <: Domain[X], V](
    override val labels: Set[Label],
    bound: ExtendedBound[E],
    otherSeq: SegmentSeq[E, D, V],
    expected: Seq[IntervalRelation[E, D, V]]
  ) extends TestCaseBase(labels) 
}
