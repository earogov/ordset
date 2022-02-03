package ordset.test.core

import ordset.givens.iterable
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.value.ValueOps
import ordset.core.interval.{Interval, IntervalRelation}
import ordset.util.IterableUtil

object IntervalAssertions {

  import ordset.test.AssertionsUtil.debugInfo
  import org.scalatest.Assertions._

  def assertSameIntervals[E, D[X] <: Domain[X]](
    expected: Interval[E, D],
    actual: Interval[E, D],
    info: String = ""
  )(
    implicit domainOps: DomainOps[E, D]
  ): Unit = 
    assert(
      domainOps.intervals.hash.eqv(expected, actual),
      debugInfo(expected, actual, info)(domainOps.showOps.intervalShow)
    ) 

  def assertSameIntervalRelations[E, D[X] <: Domain[X], V](
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

  def assertSameRelationSeq[E, D[X] <: Domain[X], V](
    expected: Iterable[IntervalRelation[E, D, V]],
    actual: Iterable[IntervalRelation[E, D, V]],
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
        domainOps.intervalRelations.hash(valueOps.valueHash)
      ),
      debugInfo(
        expected, 
        actual, 
        info
      )(
        iterable.iterableShow(domainOps.showOps.intervalRelationShow(valueOps.valueShow))
      )
    ) 
}
