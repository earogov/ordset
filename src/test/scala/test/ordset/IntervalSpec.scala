package test.ordset

import ordset.domain.{AscOrder, Domain, DomainOps}
import org.scalatest.funspec.AnyFunSpec

class IntervalSpec extends AnyFunSpec {

  import ordset._
  import instances.Int._
  import ordset.syntax.SetBuilderNotation._

  type Dom = Domain[Int]

  val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]
  val ops: DomainOps[Int, Dom] = DomainOps.defaultDomainOps
  val interval: IntervalBuilder[Int, Dom] = ops.interval

  it("should have valid bound indicators") {
    validateLess(x <= 0)
    validateLess(x < 0)
    validateGreater(x >= 5)
    validateGreater(x > 5)
    validateBetween(x >= 0 & x < 10)
    validateBetween(x >= 5 & x <= 5)
    validateEmpty(interval.empty)
    validateUniversal(interval.universal)
  }

  private def validateLess[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty)
    assert(!x.hasLowerBound)
    assert(x.hasUpperBound)
    assert(!x.isUniversal)
  }

  private def validateGreater[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty)
    assert(x.hasLowerBound)
    assert(!x.hasUpperBound)
    assert(!x.isUniversal)
  }

  private def validateBetween[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty)
    assert(x.hasLowerBound)
    assert(x.hasUpperBound)
    assert(!x.isUniversal)
  }

  private def validateEmpty[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(x.isEmpty)
    assert(!x.hasLowerBound)
    assert(!x.hasUpperBound)
    assert(!x.isUniversal)
  }

  private def validateUniversal[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty)
    assert(!x.hasLowerBound)
    assert(!x.hasUpperBound)
    assert(x.isUniversal)
  }
}
