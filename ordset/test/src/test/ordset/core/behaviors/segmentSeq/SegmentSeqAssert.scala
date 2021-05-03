package test.ordset.core.behaviors.segmentSeq

import ordset.Hash
import ordset.core.domain.{Domain, DomainOps}
import ordset.core._
import ordset.util.IterableUtil

trait SegmentSeqAssert[E, D <: Domain[E], V] {

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
        expected.firstSegment.forwardIterator().map(_.intervalRelation),
        actual.firstSegment.forwardIterator().map(_.intervalRelation)
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
        actual.firstSegment.forwardIterator().map(_.intervalRelation),
        expected.iterator
      )(
        domainOps.intervalRelationHash(valueHash)
      ),
      s"\nexpected: $expected\nactual  : $actual"
    )
  }
}
