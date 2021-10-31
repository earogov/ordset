package ordset.test.core.specs.interval

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.interval.{Interval, IntervalBuilder}
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec

@RunWith(classOf[JUnitRunner])
class IntervalSpec extends AnyFunSpec {

  import ordset.core.instances.int._
  import ordset.core.syntax.SetBuilderNotation._

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
    assert(!x.isEmpty, s"expected $x is not empty")
    assert(!x.hasLowerBound, s"expected $x does not have lower bound")
    assert(x.hasUpperBound, s"expected $x has upper bound")
    assert(!x.isUniversal, s"expected $x is not universal")
  }

  private def validateGreater[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty, s"expected $x is not empty")
    assert(x.hasLowerBound, s"expected $x has lower bound")
    assert(!x.hasUpperBound, s"expected $x does not have upper bound")
    assert(!x.isUniversal, s"expected $x is not universal")
  }

  private def validateBetween[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty, s"expected $x is not empty")
    assert(x.hasLowerBound, s"expected $x has lower bound")
    assert(x.hasUpperBound, s"expected $x has upper bound")
    assert(!x.isUniversal, s"expected $x is not universal")
  }

  private def validateEmpty[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(x.isEmpty, s"expected $x is empty")
    assert(!x.hasLowerBound, s"expected $x does not have lower bound")
    assert(!x.hasUpperBound, s"expected $x does not have upper bound")
    assert(!x.isUniversal, s"expected $x is not universal")
  }

  private def validateUniversal[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty, s"expected $x is not empty")
    assert(!x.hasLowerBound, s"expected $x does not have lower bound")
    assert(!x.hasUpperBound, s"expected $x does not have upper bound")
    assert(x.isUniversal, s"expected $x is universal")
  }
}
