package ordset.test.core

import ordset.Hash
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.interval.IntervalRelation
import ordset.util.IterableUtil

object IntervalAssertions {

  import ordset.test.AssertionsUtil.debugInfo
  import org.scalatest.Assertions._

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
