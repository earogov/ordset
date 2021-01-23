package test.ordset.core.behaviors

import ordset.Hash
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.{Interval, IntervalRelation, Segment, SegmentSeq, instances}

trait SegmentSeqAssert[E, D <: Domain[E], V] {

  def assertSameBounds(segment: Segment[E, D, V], interval: Interval[E, D]): Unit = {
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

  def assertEqualSegmentSeq(
    first: SegmentSeq[E, D, V],
    second: SegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit = {

    assert(domainOps.domainHash.eqv(first.domainOps.domain, second.domainOps.domain))

    val hash: Hash[LazyList[IntervalRelation[E, D, V]]] =
      instances.LazyList.lazyListHash(domainOps.intervalRelationHash(valueHash))

    assert(
      hash.eqv(
        first.firstSegment.forwardLazyList.map(_.intervalRelation),
        second.firstSegment.forwardLazyList.map(_.intervalRelation)
      )
    )
  }
}
