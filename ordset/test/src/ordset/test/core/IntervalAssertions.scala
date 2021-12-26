package ordset.test.core

import ordset.{Eq, Hash}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.interval.{Interval, IntervalRelation}
import ordset.util.IterableUtil

object IntervalAssertions {

  import ordset.test.AssertionsUtil.debugInfo
  import org.scalatest.Assertions._

  def assertEqvIntervals[E, D <: Domain[E]](
    expected: Interval[E, D],
    actual: Interval[E, D],
    info: String = ""
  )(
    implicit eq: Eq[Interval[E, D]]
  ): Unit = 
    assert(
      eq.eqv(expected, actual),
      debugInfo(expected, actual, info)
    ) 

  def assertSameIntervals[E, D <: Domain[E]](
    expected: Interval[E, D],
    actual: Interval[E, D],
    info: String = ""
  )(
    implicit domainOps: DomainOps[E, D]
  ): Unit = 
    assertEqvIntervals(expected, actual, info)(domainOps.intervals.hash)

  def assertSameIntervalRelations[E, D <: Domain[E], V](
    expected: IntervalRelation[E, D, V],
    actual: IntervalRelation[E, D, V],
    info: String = ""
  )(
    implicit
    domainOps: DomainOps[E, D],
    valueHash: Hash[V]
  ): Unit = 
    assert(
      domainOps.intervalRelations.hash(valueHash).eqv(expected, actual),
      debugInfo(expected, actual, info)
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
        domainOps.intervalRelations.hash(valueHash)
      ),
      debugInfo(expected, actual, info)
    ) 
}
