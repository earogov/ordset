package test.ordset.core.behaviors.segmentSeq

import ordset.Hash
import ordset.core.domain.{Domain, DomainOps}
import ordset.core._
import ordset.util.IterableUtil

trait SegmentSeqAssert[E, D <: Domain[E], V] {

  def assertSameBounds(
    segment: Segment[E, D, V],
    interval: Interval[E, D]
  ): Unit = {
    interval match {
      case i: Interval.WithLowerBound[E, D] =>
        assert(segment.hasLowerBound, s"expected $segment has lower bound")
        assert(segment.hasLowerBound(i.lowerBound), s"expected $segment has lower bound ${i.lowerBound}")
      case _ =>
        assert(!segment.hasLowerBound, s"expected $segment does not have lower bound")
    }
    interval match {
      case i: Interval.WithUpperBound[E, D] =>
        assert(segment.hasUpperBound, s"expected $segment has upper bound")
        assert(segment.hasUpperBound(i.upperBound), s"expected $segment has upper bound ${i.upperBound}")
      case _ =>
        assert(!segment.hasUpperBound, s"expected $segment does not have upper bound")
    }
  }

  def assertEqualSequences(
    first: SegmentSeq[E, D, V],
    second: SegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit = {

    assert(
      domainOps.domainHash.eqv(
        first.domainOps.domain,
        second.domainOps.domain
      )
    )
    assert(
      IterableUtil.iteratorEq(
        first.firstSegment.forwardIterator.map(_.intervalRelation),
        second.firstSegment.forwardIterator.map(_.intervalRelation)
      )(
        domainOps.intervalRelationHash(valueHash)
      )
    )
  }

  def assertEqualSequences(
    first: SegmentSeq[E, D, V],
    second: Seq[IntervalRelation[E, D, V]]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit = {

    assert(
      IterableUtil.iteratorEq(
        first.firstSegment.forwardIterator.map(_.intervalRelation),
        second.iterator
      )(
        domainOps.intervalRelationHash(valueHash)
      )
    )
  }
}
