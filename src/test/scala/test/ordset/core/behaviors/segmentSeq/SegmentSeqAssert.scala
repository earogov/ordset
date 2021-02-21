package test.ordset.core.behaviors.segmentSeq

import ordset.Hash
import ordset.core.domain.{Domain, DomainOps}
import ordset.core._
import ordset.util.IterableUtil

trait SegmentSeqAssert[E, D <: Domain[E], V] {

  def assertSameBounds(
    expected: Interval[E, D],
    actual: Segment[E, D, V]
  ): Unit = {
    expected match {
      case i: Interval.WithLowerBound[E, D] =>
        assert(actual.hasLowerBound, s"expected $actual has lower bound")
        assert(actual.hasLowerBound(i.lowerBound), s"expected $actual has lower bound ${i.lowerBound}")
      case _ =>
        assert(!actual.hasLowerBound, s"expected $actual does not have lower bound")
    }
    expected match {
      case i: Interval.WithUpperBound[E, D] =>
        assert(actual.hasUpperBound, s"expected $actual has upper bound")
        assert(actual.hasUpperBound(i.upperBound), s"expected $actual has upper bound ${i.upperBound}")
      case _ =>
        assert(!actual.hasUpperBound, s"expected $actual does not have upper bound")
    }
  }

  def assertEqualSequences(
    expected: SegmentSeq[E, D, V],
    actual: SegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit = {

    assert(
      domainOps.domainHash.eqv(
        expected.domainOps.domain,
        actual.domainOps.domain
      )
    )
    assert(
      IterableUtil.iteratorEq(
        expected.firstSegment.forwardIterator.map(_.intervalRelation),
        actual.firstSegment.forwardIterator.map(_.intervalRelation)
      )(
        domainOps.intervalRelationHash(valueHash)
      ),
      s"\nexpected: $expected\nactual  : $actual"
    )
  }

  def assertEqualSequences(
    expected: Seq[IntervalRelation[E, D, V]],
    actual: SegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit = {

    assert(
      IterableUtil.iteratorEq(
        actual.firstSegment.forwardIterator.map(_.intervalRelation),
        expected.iterator
      )(
        domainOps.intervalRelationHash(valueHash)
      ),
      s"\nexpected: $expected\nactual  : $actual"
    )
  }
}
