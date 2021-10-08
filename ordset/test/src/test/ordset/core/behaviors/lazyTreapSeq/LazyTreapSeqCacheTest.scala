package test.ordset.core.behaviors.lazyTreapSeq

import ordset.core.AbstractLazyTreapSegmentSeq.ZValue
import ordset.core.ExtendedBound
import ordset.core.interval.IntervalRelation
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

  sealed trait TestCase[E, D <: Domain[E], V] {

    def bound: ExtendedBound[E]

    def expectedState: Seq[IntervalRelation[E, D, ZValue[E, D, V]]]

    override def toString: String = TestShowUtil.caseWithBoundToString(bound)
  }

  case class SegmentTestCase[E, D <: Domain[E], V](
    override val bound: ExtendedBound[E],
    expectedSegment: IntervalRelation[E, D, V],
    override val expectedState: Seq[IntervalRelation[E, D, ZValue[E, D, V]]]
  ) extends TestCase[E, D, V]

  case class ValueTestCase[E, D <: Domain[E], V](
    override val bound: ExtendedBound[E],
    expectedValue: V,
    override val expectedState: Seq[IntervalRelation[E, D, ZValue[E, D, V]]]
  ) extends TestCase[E, D, V]

  case class TestPackage[E, D <: Domain[E], V](
    override val labels: Set[Label],
    cases: Iterable[TestCase[E, D, V]]
  ) extends TestPackageBase(labels)
}