package test.ordset.core.behaviors.lazyTreapSeq

import ordset.core.AbstractLazyTreapSegmentSeq.ZValue
import ordset.core.{ExtendedBound, IntervalRelation}
import ordset.core.domain.Domain
import ordset.util.label.Label
import test.ordset.core.behaviors.{TestPackageBase, TestShowUtil}

trait LazyTreapSeqCacheTest[E, D <: Domain[E], V] {

  /**
   * Preconditions: 
   * 
   * Before run of each package initial state of lazy sequence must be restored.
   */
  def lazyCacheCases: Iterable[LazyTreapSeqCacheTest.TestPackage[E, D, V]]
}

object LazyTreapSeqCacheTest {

  case class TestCase[E, D <: Domain[E], V](
    bound: ExtendedBound[E],
    expectedSegment: IntervalRelation[E, D, V],
    expectedState: Seq[IntervalRelation[E, D, ZValue[E, D, V]]]
  ) {

    override def toString: String = TestShowUtil.caseWithBoundToString(bound)
  }

  case class TestPackage[E, D <: Domain[E], V](
    override val labels: Set[Label],
    cases: Iterable[TestCase[E, D, V]]
  ) extends TestPackageBase(labels)
}