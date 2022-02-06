package ordset.test.core

import ordset.core.segmentSeq.*
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.interval.IntervalRelation
import ordset.core.segmentSeq.map.BoundValue
import ordset.util.IterableUtil
import ordset.givens.{tuple2, iterable}
import ordset.ContravariantHash

object SegmentSeqAssertions {

  import ordset.test.AssertionsUtil.debugInfo
  import org.scalatest.Assertions._

  def assertSameSegment[E, D[X] <: Domain[X], V](
    expected: Segment[E, D, V],
    actual: Segment[E, D, V],
    info: String = ""
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  ): Unit = 
    assertSameRelation(expected.intervalRelation, actual.intervalRelation, info)

  def assertSameRelationAndSegment[E, D[X] <: Domain[X], V](
    expected: IntervalRelation[E, D, V],
    actual: Segment[E, D, V],
    info: String = ""
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  ): Unit =
    assertSameRelation(expected, actual.intervalRelation, info)

  def assertSameRelation[E, D[X] <: Domain[X], V](
    expected: IntervalRelation[E, D, V],
    actual: IntervalRelation[E, D, V],
    info: String = ""
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  ): Unit =
    assert(
      domainOps.intervalRelations.hash(valueOps.valueHash).eqv(expected, actual),
      debugInfo(expected, actual, info)(domainOps.showOps.intervalRelationShow(valueOps.valueShow))
    )

  def assertSameSegmentSeq[E, D[X] <: Domain[X], V](
    expected: SegmentSeq[E, D, V],
    actual: SegmentSeq[E, D, V],
    info: String = ""
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  ): Unit =
    IntervalAssertions.assertSameRelationSeq(
      expected.firstSegment.forwardIterable.map(_.intervalRelation),
      actual.firstSegment.forwardIterable.map(_.intervalRelation),
      info
    )

  def assertSameRelationAndSegmentSeq[E, D[X] <: Domain[X], V](
    expected: Iterable[IntervalRelation[E, D, V]],
    actual: SegmentSeq[E, D, V],
    info: String = ""
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  ): Unit =
    IntervalAssertions.assertSameRelationSeq(
      expected,
      actual.firstSegment.forwardIterable.map(_.intervalRelation),
      info
    )

  def assertSameBoundValueSeq[E, D[X] <: Domain[X], V](
    expected: Iterable[BoundValue[E, V]],
    actual: Iterable[BoundValue[E, V]],
    info: String = ""
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueOps: ValueOps[V]
  ): Unit = 
    assert(
      IterableUtil.iterableEq(
        actual,
        expected
      )(
        tuple2.tuple2Hash(ContravariantHash.fromHash(domainOps.extendedOrd), valueOps.valueHash)
      ),
      debugInfo(
        expected, 
        actual, 
        info
      )(
        iterable.iterableShow(tuple2.tuple2Show(domainOps.showOps.extendedShow, valueOps.valueShow))
      )
    )   
}
