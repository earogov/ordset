package test.ordset.core

import ordset.Hash
import ordset.core._
import ordset.core.domain.{Domain, DomainOps}
import ordset.util.IterableUtil
import org.scalatest.Assertions._

object SegmentSeqAssert {

  def assertEqualSegmentSeq[E, D <: Domain[E], V](
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
    assertEqualRelationSeq(
      expected.firstSegment.forwardIterable.map(_.intervalRelation),
      actual.firstSegment.forwardIterable.map(_.intervalRelation)
    )
  }

  def assertEqualRelationAndSegmentSeq[E, D <: Domain[E], V](
    expected: Iterable[IntervalRelation[E, D, V]],
    actual: SegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit =
    assertEqualRelationSeq(
      expected,
      actual.firstSegment.forwardIterable.map(_.intervalRelation)
    )

  def assertEqualRelationSeq[E, D <: Domain[E], V](
    expected: Iterable[IntervalRelation[E, D, V]],
    actual: Iterable[IntervalRelation[E, D, V]],
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit =
    assert(
      IterableUtil.iteratorEq(
        actual.iterator,
        expected.iterator
      )(
        domainOps.intervalRelationHash(valueHash)
      ),
      s"\nexpected: $expected\nactual  : $actual"
    )


  def assertEqualBoundValueIterables[E, D <: Domain[E], V](
    expected: Iterable[(Bound.Upper[E], V)],
    actual: Iterable[(Bound.Upper[E], V)]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit = {

    val expIter = expected.iterator
    val actIter = actual.iterator

    if (expIter.hasNext != actIter.hasNext) {
      fail(s"iterables have different length \nexpected: $expected\nactual  : $actual")
    }
    while (expIter.hasNext) {
      val expItem = expIter.next()
      val actItem = actIter.next()

      if (expIter.hasNext != actIter.hasNext) {
        fail(s"iterables have different length \nexpected: $expected\nactual  : $actual")
      }
      assert(
        valueHash.eqv(expItem._2, actItem._2),
        s"different values \nexpected: ${expItem._2}\nactual  : ${actItem._2}"
      )
      if (expIter.hasNext) {
        assert(
          domainOps.boundOrd.eqv(expItem._1, actItem._1),
          s"different bounds \nexpected: ${expItem._1}\nactual  : ${actItem._1}"
        )
      } else {
        assert(
          expItem._1 == null || actItem._1 == null,
          s"bound of last item must be null \nexpected: ${expItem._1}\nactual  : ${actItem._1}"
        )
      }
    }
  }
}
