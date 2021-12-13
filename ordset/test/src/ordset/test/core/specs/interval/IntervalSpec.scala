package ordset.test.core.specs.interval

import ordset.core.{ExtendedBound, Bound}
import ordset.core.ExtendedBound.{BelowAll, AboveAll}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.interval.{Interval, IntervalBuilder}
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec
import cats.syntax.validated

@RunWith(classOf[JUnitRunner])
class IntervalSpec extends AnyFunSpec {

  import ordset.instances.int._
  import ordset.core.syntax.SetBuilderNotation._

  type Dom = Domain.ContinuousUnbounded[Int]

  val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]
  val ops: DomainOps[Int, Dom] = DomainOps.default
  val interval: IntervalBuilder[Int, Dom] = ops.intervals.builder

  it("should have valid bound indicators") {
    // Interval.Less
    validateLess(x <= 0)
    validateLess(x < 0)
    validateLowerBound(false, x <= 0, x >= 0)
    validateLowerBound(true, x <= 0, BelowAll)
    validateUpperBound(true, x <= 0, x <= 0)
    validateUpperBound(false, x <= 0, x < 0)
    validateUpperBound(false, x <= 0, AboveAll)
    // Interval.Greater
    validateGreater(x >= 5)
    validateGreater(x > 5)
    validateLowerBound(true, x >= 5, x >= 5)
    validateLowerBound(false, x >= 5, x > 5)
    validateLowerBound(false, x >= 5, BelowAll)
    validateUpperBound(false, x >= 5, x < 5)
    validateUpperBound(true, x >= 5, AboveAll)
    // Interval.Between
    validateBetween(x >= 0 & x < 10)
    validateBetween(x >= 5 & x <= 5)
    validateLowerBound(true, x >= 5 & x <= 5, x >= 5)
    validateLowerBound(false, x >= 5 & x <= 5, x > 5)
    validateLowerBound(false, x >= 5 & x <= 5, BelowAll)
    validateUpperBound(true, x >= 5 & x <= 5, x <= 5)
    validateUpperBound(false, x >= 5 & x <= 5, x < 5)
    validateUpperBound(false, x >= 5 & x <= 5, AboveAll)
    // Interval.Empty
    validateEmpty(interval.empty)
    validateLowerBound(false, interval.empty, x > 0)
    validateLowerBound(false, interval.empty, x >= 0)
    validateLowerBound(false, interval.empty, BelowAll)
    validateUpperBound(false, interval.empty, x < 0)
    validateUpperBound(false, interval.empty, x <= 0)
    validateUpperBound(false, interval.empty, AboveAll)
    // Interval.Universal
    validateUniversal(interval.universal)
    validateLowerBound(false, interval.universal, x > 0)
    validateLowerBound(false, interval.universal, x >= 0)
    validateLowerBound(true, interval.universal, BelowAll)
    validateUpperBound(false, interval.universal, x < 0)
    validateUpperBound(false, interval.universal, x <= 0)
    validateUpperBound(true, interval.universal, AboveAll)
  }

  it("should restrict bounds") {
    // Interval.Less
    validateRestrict(x <= 0, x <= 0, x < 10)
    validateRestrict(x < -10, x <= 0, x < -10)
    validateRestrict(x >= 0, x <= 0, x >= 0)
    validateRestrict(BelowAll, x <= 0, BelowAll)
    validateRestrict(x <= 0, x <= 0, AboveAll)
    // Interval.Greater
    validateRestrict(x >= 0, x >= 0, x < 0)
    validateRestrict(x < 10, x >= 0, x < 10)
    validateRestrict(x <= 0, x >= 0, x <= 0)
    validateRestrict(x >= 0, x >= 0, BelowAll)
    validateRestrict(AboveAll, x >= 0, AboveAll)
    // Interval.Between
    validateRestrict(x >= 0, x >= 0 & x <= 10, x < -100)
    validateRestrict(x <= 10, x >= 0 & x <= 10, x > 100)
    validateRestrict(x >= 0, x >= 0 & x <= 10, x < 0)
    validateRestrict(x <= 10, x >= 0 & x <= 10, x > 10)
    validateRestrict(x >= 0, x >= 0 & x <= 10, x >= 0)
    validateRestrict(x < 5, x >= 0 & x <= 10, x < 5)
    validateRestrict(x <= 10, x >= 0 & x <= 10, x <= 10)
    validateRestrict(x <= 0, x >= 0 & x <= 10, x <= 0)
    validateRestrict(x >= 10, x >= 0 & x <= 10, x >= 10)
    validateRestrict(x >= 0, x >= 0 & x <= 10, BelowAll)
    validateRestrict(x <= 10, x >= 0 & x <= 10, AboveAll)
    // Interval.Universal
    validateRestrict(x > 0, interval.universal, x > 0)
    validateRestrict(x <= 10, interval.universal, x <= 10)
    validateRestrict(BelowAll, interval.universal, BelowAll)
    validateRestrict(AboveAll, interval.universal, AboveAll)
  }

  it("should define whether it contains bound") {
    // Interval.Less
    validateContains(true, x <= 0, x <= -1)
    validateContains(true, x <= 0, x <= 0)
    validateContains(true, x <= 0, x >= 0)
    validateContains(false, x <= 0, x > 1)
    validateContains(true, x <= 0, BelowAll)
    validateContains(false, x <= 0, AboveAll)
    // Interval.Greater
    validateContains(true, x > 0, x >= 5)
    validateContains(true, x > 0, x > 0)
    validateContains(true, x >= 0, x <= 0)
    validateContains(false, x > 0, x < -1)
    validateContains(true, x > 0, AboveAll)
    validateContains(false, x > 0, BelowAll)
    // Interval.Between
    validateContains(true, x >= 0 & x < 10, x <= 5)
    validateContains(true, x >= 0 & x < 10, x <= 0)
    validateContains(false, x >= 0 & x < 10, x < 0)
    validateContains(false, x >= 0 & x < 10, x >= 10)
    validateContains(false, x >= 0 & x < 10, AboveAll)
    validateContains(false, x >= 0 & x < 10, BelowAll)
    // Interval.Empty
    validateContains(false, interval.empty, x <= 5)
    validateContains(false, interval.empty, x <= 15)
    validateContains(false, interval.empty, BelowAll)
    validateContains(false, interval.empty, AboveAll)
    // Interval.Universal
    validateContains(true, interval.universal, x <= 5)
    validateContains(true, interval.universal, x <= 15)
    validateContains(true, interval.universal, BelowAll)
    validateContains(true, interval.universal, AboveAll)
  }

  private def validateLowerBound[E, D <: Domain[E]](
    exp: Boolean,
    x: Interval[E, D],
    b: ExtendedBound.Lower[E]
  ): Unit = {
    assert(x.hasLowerExtended(b) == exp, s"// expected $exp: $x has extended lower bound $b")
    b match {
      case b: Bound.Lower[E] =>
        assert(x.hasLowerBound(b) == exp, s"// expected $exp: $x has lower bound $b")
      case _ =>
        // nothing to do
    }
  }

  private def validateUpperBound[E, D <: Domain[E]](
    exp: Boolean,
    x: Interval[E, D],
    b: ExtendedBound.Upper[E]
  ): Unit = {
    assert(x.hasUpperExtended(b) == exp, s"// expected $exp: $x has extended upper bound $b")
    b match {
      case b: Bound.Upper[E] =>
        assert(x.hasUpperBound(b) == exp, s"// expected $exp: $x has upper bound $b")
      case _ =>
        // nothing to do
    }
  }

  private def validateContains[E, D <: Domain[E]](
    exp: Boolean, 
    x: Interval[E, D], 
    b: ExtendedBound[E]
  ): Unit = {
    assert(x.containsExtended(b) == exp, s"// expected $exp: $x contains extended bound $b")
    b match {
      case b: Bound[E] =>
        assert(x.containsBound(b) == exp, s"// expected $exp: contains bound $b")
        if (b.isIncluding) {
          assert(x.containsElement(b.element) == exp, s"// expected $exp: $x contains element $b")
        }
      case _ => 
        // nothing to do 
    }
  }

  private def validateRestrict[E, D <: Domain[E]](
    exp: ExtendedBound[E], 
    x: Interval[E, D], 
    b: ExtendedBound[E]
  ): Unit =
    x match {
      case x: Interval.NonEmpty[E, D] =>
        assert(x.domain.extendedOrd.eqv(x.restrictExtended(b), exp), s"// expected $exp: $x restricts extended bound $b")
        b match {
          case b: Bound[E] =>
            assert(x.domain.extendedOrd.eqv(x.restrictBound(b), exp), s"// expected $exp: $x restricts bound $b")
          case _ => 
            // nothing to do 
        }
      case _ => 
        fail(s"// expected $x is non-empty")
    }

  private def validateLess[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty, s"// expected $x is not empty")
    assert(!x.hasLowerBound, s"// expected $x does not have lower bound")
    assert(x.hasUpperBound, s"// expected $x has upper bound")
    assert(!x.isUniversal, s"// expected $x is not universal")
  }

  private def validateGreater[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty, s"// expected $x is not empty")
    assert(x.hasLowerBound, s"// expected $x has lower bound")
    assert(!x.hasUpperBound, s"// expected $x does not have upper bound")
    assert(!x.isUniversal, s"// expected $x is not universal")
  }

  private def validateBetween[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty, s"// expected $x is not empty")
    assert(x.hasLowerBound, s"// expected $x has lower bound")
    assert(x.hasUpperBound, s"// expected $x has upper bound")
    assert(!x.isUniversal, s"// expected $x is not universal")
  }

  private def validateEmpty[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(x.isEmpty, s"// expected $x is empty")
    assert(!x.hasLowerBound, s"// expected $x does not have lower bound")
    assert(!x.hasUpperBound, s"// expected $x does not have upper bound")
    assert(!x.isUniversal, s"// expected $x is not universal")
  }

  private def validateUniversal[E, D <: Domain[E]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty, s"// expected $x is not empty")
    assert(!x.hasLowerBound, s"// expected $x does not have lower bound")
    assert(!x.hasUpperBound, s"// expected $x does not have upper bound")
    assert(x.isUniversal, s"// expected $x is universal")
  }
}
