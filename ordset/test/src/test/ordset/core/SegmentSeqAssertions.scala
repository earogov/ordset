package test.ordset.core

import ordset.Hash
import ordset.core._
import ordset.core.domain.{Domain, DomainOps}
import ordset.util.IterableUtil
import org.scalatest.Assertions._

object SegmentSeqAssertions {

  def assertSameSegment[E, D <: Domain[E], V](
    expected: Segment[E, D, V],
    actual: Segment[E, D, V]
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
    assertSameRelation(expected.intervalRelation, actual.intervalRelation)
  }

  def assertSameRelationAndSegment[E, D <: Domain[E], V](
    expected: IntervalRelation[E, D, V],
    actual: Segment[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit =
    assertSameRelation(expected, actual.intervalRelation)

  def assertSameRelation[E, D <: Domain[E], V](
    expected: IntervalRelation[E, D, V],
    actual: IntervalRelation[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit =
    assert(
      domainOps.intervalRelationHash(valueHash).eqv(expected, actual),
      debugInfo(expected, actual)
    )

  def assertSameSegmentSeq[E, D <: Domain[E], V](
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
    assertSameRelationSeq(
      expected.firstSegment.forwardIterable.map(_.intervalRelation),
      actual.firstSegment.forwardIterable.map(_.intervalRelation)
    )
  }

  def assertSameRelationAndSegmentSeq[E, D <: Domain[E], V](
    expected: Iterable[IntervalRelation[E, D, V]],
    actual: SegmentSeq[E, D, V]
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit =
    assertSameRelationSeq(
      expected,
      actual.firstSegment.forwardIterable.map(_.intervalRelation)
    )

  def assertSameRelationSeq[E, D <: Domain[E], V](
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
      debugInfo(expected, actual)
    )

  def assertSameBoundValueIterable[E, D <: Domain[E], V](
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
      fail("iterables have different length" + debugInfo(expected, actual))
    }
    while (expIter.hasNext) {
      val expItem = expIter.next()
      val actItem = actIter.next()

      if (expIter.hasNext != actIter.hasNext) {
        fail("iterables have different length" + debugInfo(expected, actual))
      }
      assert(
        valueHash.eqv(expItem._2, actItem._2),
        "different values" + debugInfo(expItem._2, actItem._2)
      )
      if (expIter.hasNext) {
        assert(
          domainOps.boundOrd.eqv(expItem._1, actItem._1),
          "different bounds" + debugInfo(expItem._1, actItem._1)
        )
      } else {
        assert(
          expItem._1 == null || actItem._1 == null,
          "bound of last item must be null" + debugInfo(expItem._1, actItem._1)
        )
      }
    }
  }

  // Private section ---------------------------------------------------------- //
  private def debugInfo(expected: Any, actual: Any): String = {
      s"\nexpected: $expected\nactual  : $actual"
  }
}
