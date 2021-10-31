package ordset.test.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.{Bound, ExtendedBound}
import ordset.test.core.behaviors.{TestShowUtil, TestTuple}

trait SegmentRestrictBoundTest[E, D <: Domain[E], V] {

  def restrictCases: Seq[SegmentRestrictBoundTest.TestCase[E, D, V]]
}

object SegmentRestrictBoundTest {

  /**
   * Test case for `restrictBound` method of segment.
   *
   * @param bound specifies segment S of segment sequence
   * @param restrictedBounds list of tuples: (input bound, expected output bound)
   */
  case class TestCase[E, D <: Domain[E], V](
    bound: Bound[E],
    restrictedBounds: List[TestTuple[ExtendedBound[E], ExtendedBound[E]]]
  ) {

    override def toString: String = TestShowUtil.caseWithBoundToString(bound)
  }
}


