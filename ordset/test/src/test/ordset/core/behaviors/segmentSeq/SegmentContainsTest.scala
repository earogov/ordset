package test.ordset.core.behaviors.segmentSeq

import ordset.core.domain.Domain
import ordset.core.{Bound, ExtendedBound}
import test.ordset.core.behaviors.TestShowUtil

trait SegmentContainsTest[E, D <: Domain[E], V] {

  def containsCases: Seq[SegmentContainsTest.TestCase[E, D, V]]
}

object SegmentContainsTest {

  /**
   * Test case for `contains` method of segment.
   * 
   * @param bound specifies segment S of segment sequence
   * @param includedBounds list of extended bounds that should be included in segment S
   * @param excludedBounds list of extended bounds that should not be included in segment S
   */
  case class TestCase[E, D <: Domain[E], V](
    bound: Bound[E],
    includedBounds: List[ExtendedBound[E]],
    excludedBounds: List[ExtendedBound[E]]
  ) {
    
    override def toString: String = TestShowUtil.caseWithBoundToString(bound)
  }
}
