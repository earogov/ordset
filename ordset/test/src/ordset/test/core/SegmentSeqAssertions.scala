package ordset.test.core

import ordset.{Hash, core}
import ordset.core._
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.interval.IntervalRelation
import ordset.util.IterableUtil
import org.scalatest.Assertions._

object SegmentSeqAssertions {

  def assertSameSegment[E, D <: Domain[E], V](
    expected: Segment[E, D, V],
    actual: Segment[E, D, V],
    info: String = ""
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
    assertSameRelation(expected.intervalRelation, actual.intervalRelation, info)
  }

  def assertSameRelationAndSegment[E, D <: Domain[E], V](
    expected: IntervalRelation[E, D, V],
    actual: Segment[E, D, V],
    info: String = ""
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit =
    assertSameRelation(expected, actual.intervalRelation, info)

  def assertSameRelation[E, D <: Domain[E], V](
    expected: IntervalRelation[E, D, V],
    actual: IntervalRelation[E, D, V],
    info: String = ""
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit =
    assert(
      domainOps.intervalRelationHash(valueHash).eqv(expected, actual),
      debugInfo(expected, actual, info)
    )

  def assertSameSegmentSeq[E, D <: Domain[E], V](
    expected: SegmentSeq[E, D, V],
    actual: SegmentSeq[E, D, V],
    info: String = ""
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
      actual.firstSegment.forwardIterable.map(_.intervalRelation),
      info
    )
  }

  def assertSameRelationAndSegmentSeq[E, D <: Domain[E], V](
    expected: Iterable[IntervalRelation[E, D, V]],
    actual: SegmentSeq[E, D, V],
    info: String = ""
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit =
    assertSameRelationSeq(
      expected,
      actual.firstSegment.forwardIterable.map(_.intervalRelation),
      info
    )

  def assertSameRelationSeq[E, D <: Domain[E], V](
    expected: Iterable[IntervalRelation[E, D, V]],
    actual: Iterable[IntervalRelation[E, D, V]],
    info: String = ""
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
      debugInfo(expected, actual, info)
    )

  def assertSameBoundValueIterable[E, D <: Domain[E], V](
    expected: Iterable[(ExtendedBound.Upper[E], V)],
    actual: Iterable[(ExtendedBound.Upper[E], V)],
    info: String = ""
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit = {

    val expIter = expected.iterator
    val actIter = actual.iterator

    if (expIter.hasNext != actIter.hasNext) {
      fail("iterables have different length" + debugInfo(expected, actual, info))
    }
    while (expIter.hasNext) {
      val expItem = expIter.next()
      val actItem = actIter.next()

      if (expIter.hasNext != actIter.hasNext) {
        fail("iterables have different length" + debugInfo(expected, actual, info))
      }
      assert(
        valueHash.eqv(expItem._2, actItem._2),
        "different values" + debugInfo(expItem._2, actItem._2, info)
      )
      assert(
        domainOps.extendedOrd.eqv(expItem._1, actItem._1),
        "different bounds" + debugInfo(expItem._1, actItem._1, info)
      )
    }
  }

  // Private section ---------------------------------------------------------- //
  private def debugInfo(expected: Any, actual: Any, info: String = ""): String = {
    val sep = "\n"
    val msg = s"${sep}expected: $expected${sep}actual: $actual"
    if (info.nonEmpty) msg + sep + info
    else msg
  }
}
