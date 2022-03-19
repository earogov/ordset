package ordset.test.core.specs.interval

import ordset.core.{ExtendedBound, Bound}
import ordset.core.ExtendedBound.{BelowAll, AboveAll}
import ordset.core.domain.{Domain, DomainOps}
import ordset.core.interval.{Interval, IntervalFactory}
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec

@RunWith(classOf[JUnitRunner])
class IntervalSpec extends AnyFunSpec {

  import ordset.givens.int._
  import ordset.core.syntax.SetBuilderNotation._

  type Dom[X] = Domain.ContinuousUnbounded[X]

  val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]

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
    validateEmpty(none(x))
    validateLowerBound(false, none(x), x > 0)
    validateLowerBound(false, none(x), x >= 0)
    validateLowerBound(false, none(x), BelowAll)
    validateUpperBound(false, none(x), x < 0)
    validateUpperBound(false, none(x), x <= 0)
    validateUpperBound(false, none(x), AboveAll)
    // Interval.Unbounded
    validateUnbounded(x)
    validateLowerBound(false, x, x > 0)
    validateLowerBound(false, x, x >= 0)
    validateLowerBound(true, x, BelowAll)
    validateUpperBound(false, x, x < 0)
    validateUpperBound(false, x, x <= 0)
    validateUpperBound(true, x, AboveAll)
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
    // Interval.Unbounded
    validateRestrict(x > 0, x, x > 0)
    validateRestrict(x <= 10, x, x <= 10)
    validateRestrict(BelowAll, x, BelowAll)
    validateRestrict(AboveAll, x, AboveAll)
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
    validateContains(false, none(x), x <= 5)
    validateContains(false, none(x), x <= 15)
    validateContains(false, none(x), BelowAll)
    validateContains(false, none(x), AboveAll)
    // Interval.Unbounded
    validateContains(true, x, x <= 5)
    validateContains(true, x, x <= 15)
    validateContains(true, x, BelowAll)
    validateContains(true, x, AboveAll)
  }

  it("should check whether interval is adjacent to another one and precedes it") {
    // Interval.Less
    validateAdjacentPreceding(true, x < 0, x >= 0)
    validateAdjacentPreceding(true, x < 0, x >= 0 & x < 10)
    validateAdjacentPreceding(false, x < 0, none(x))
    validateAdjacentPreceding(false, x < 0, x < 0)
    validateAdjacentPreceding(false, x < 0, x)
    validateAdjacentPreceding(false, x < 0, x > 0)
    // Interval.Greater
    validateAdjacentPreceding(false, x > 0, x > 0)
    validateAdjacentPreceding(false, x > 0, none(x))
    validateAdjacentPreceding(false, x > 0, x)
    validateAdjacentPreceding(false, x > 0, x <= 0)
    // Interval.Between
    validateAdjacentPreceding(true, x >= 0 & x < 10, x >= 10)
    validateAdjacentPreceding(true, x >= 0 & x < 10, x >= 10 & x < 20)
    validateAdjacentPreceding(false, x >= 0 & x < 10, none(x))
    validateAdjacentPreceding(false, x >= 0 & x < 10, x)
    validateAdjacentPreceding(false, x >= 0 & x < 10, x >= 0 & x < 10)
    validateAdjacentPreceding(false, x >= 0 & x < 10, x < 0)
    validateAdjacentPreceding(false, x >= 0 & x < 10, x > 10)
    validateAdjacentPreceding(false, x >= 0 & x < 10, x > 20)
    // Interval.Empty
    validateAdjacentPreceding(false, none(x), none(x))
    validateAdjacentPreceding(false, none(x), x)
    validateAdjacentPreceding(false, none(x), x > 0)
    // Interval.Unbounded
    validateAdjacentPreceding(false, x, none(x))
    validateAdjacentPreceding(false, x, x)
    validateAdjacentPreceding(false, x, x > 0)
  }

  it("should check whether interval is adjacent to another one") {
    // Interval.Less
    validateAdjacent(true, x < 0, x >= 0)
    validateAdjacent(true, x < 0, x >= 0 & x < 10)
    validateAdjacent(false, x < 0, none(x))
    validateAdjacent(false, x < 0, x)
    validateAdjacent(false, x < 0, x < 0)
    validateAdjacent(false, x < 0, x <= 0)
    validateAdjacent(false, x < 0, x > 0)
    validateAdjacent(false, x < 0, x > 1)
    // Interval.Greater
    validateAdjacent(true, x >= 0, x < 0)
    validateAdjacent(true, x >= 0, x >= -5 & x < 0)
    validateAdjacent(false, x >= 0, none(x))
    validateAdjacent(false, x >= 0, x)
    validateAdjacent(false, x >= 0, x >= 0)
    validateAdjacent(false, x >= 0, x <= 0)
    validateAdjacent(false, x >= 0, x <= -1)
    validateAdjacent(false, x >= 0, x > -5 & x < -1)
    // Interval.Between
    validateAdjacent(true, x > 0 & x < 5, x <= 0)
    validateAdjacent(true, x > 0 & x < 5, x > -5 & x <= 0)
    validateAdjacent(true, x > 0 & x < 5, x >= 5)
    validateAdjacent(true, x > 0 & x < 5, x >= 5 & x < 10)
    validateAdjacent(false, x > 0 & x < 5, none(x))
    validateAdjacent(false, x > 0 & x < 5, x)
    validateAdjacent(false, x > 0 & x < 5, x > 0 & x < 5)
    validateAdjacent(false, x > 0 & x < 5, x < 0)
    validateAdjacent(false, x > 0 & x < 5, x < 1)
    validateAdjacent(false, x > 0 & x < 5, x > 4)
    validateAdjacent(false, x > 0 & x < 5, x > 5)
    // Interval.Empty
    validateAdjacent(false, none(x), none(x))
    validateAdjacent(false, none(x), x)
    validateAdjacent(false, none(x), x < 0)
    validateAdjacent(false, none(x), x > 0)
    // Interval.Unbounded
    validateAdjacent(false, x, none(x))
    validateAdjacent(false, x, x)
    validateAdjacent(false, x, x < 0)
    validateAdjacent(false, x, x > 0)
  }

  it("should check whether interval is separated from another one and precedes it") {
    // Interval.Less
    validateSeparatedPreceding(true, x < 0, x > 0)
    validateSeparatedPreceding(true, x < 0, x > 1 & x < 2)
    validateSeparatedPreceding(false, x < 0, none(x))
    validateSeparatedPreceding(false, x < 0, x)
    validateSeparatedPreceding(false, x < 0, x >= 0)
    validateSeparatedPreceding(false, x < 0, x > -1)
    validateSeparatedPreceding(false, x < 0, x > -1 & x < 10)
    // Interval.Greater
    validateSeparatedPreceding(false, x > 0, x < 0)
    validateSeparatedPreceding(false, x > 0, x > -2 & x < -1)
    validateSeparatedPreceding(false, x > 0, none(x))
    validateSeparatedPreceding(false, x > 0, x)
    validateSeparatedPreceding(false, x > 0, x <= 0)
    validateSeparatedPreceding(false, x > 0, x < 1)
    validateSeparatedPreceding(false, x > 0, x > -10 & x < 1)
    // Interval.Between
    validateSeparatedPreceding(true, x > -10 & x < 0, x > 0)
    validateSeparatedPreceding(true, x > -10 & x < 0, x > 1 & x < 2)
    validateSeparatedPreceding(false, x > -10 & x < 0, none(x))
    validateSeparatedPreceding(false, x > -10 & x < 0, x)
    validateSeparatedPreceding(false, x > -10 & x < 0, x >= 0)
    validateSeparatedPreceding(false, x > -10 & x < 0, x > -1)
    validateSeparatedPreceding(false, x > -10 & x < 0, x > -1 & x < 10)
    validateSeparatedPreceding(false, x > -10 & x < 0, x > -100 & x < -5)
    validateSeparatedPreceding(false, x > -10 & x < 0, x > -5 & x < -1)
    validateSeparatedPreceding(false, x > -10 & x < 0, x > -15 & x < 1)
    validateSeparatedPreceding(false, x > -10 & x < 0, x < -15)
    // Interval.Empty
    validateSeparatedPreceding(false, none(x), none(x))
    validateSeparatedPreceding(false, none(x), x)
    validateSeparatedPreceding(false, none(x), x < 0)
    validateSeparatedPreceding(false, none(x), x > 0)
    validateSeparatedPreceding(false, none(x), x > 0 & x < 10)
    // Interval.Unbounded
    validateSeparatedPreceding(false, x, none(x))
    validateSeparatedPreceding(false, x, x)
    validateSeparatedPreceding(false, x, x < 0)
    validateSeparatedPreceding(false, x, x > 0)
    validateSeparatedPreceding(false, x, x > 0 & x < 10)
  }


  it("should check whether interval is separated from another one") {
    // Interval.Less
    validateSeparated(true, x < 0, x > 0)
    validateSeparated(true, x < 0, x > 1 & x < 2)
    validateSeparated(false, x < 0, none(x))
    validateSeparated(false, x < 0, x)
    validateSeparated(false, x < 0, x >= 0)
    validateSeparated(false, x < 0, x > -1)
    validateSeparated(false, x < 0, x > -1 & x < 10)
    // Interval.Greater
    validateSeparated(true, x > 0, x < 0)
    validateSeparated(true, x > 0, x > -2 & x < -1)
    validateSeparated(false, x > 0, none(x))
    validateSeparated(false, x > 0, x)
    validateSeparated(false, x > 0, x <= 0)
    validateSeparated(false, x > 0, x < 1)
    validateSeparated(false, x > 0, x > -10 & x < 1)
    // Interval.Between
    validateSeparated(true, x > -10 & x < 0, x > 0)
    validateSeparated(true, x > -10 & x < 0, x > 1 & x < 2)
    validateSeparated(true, x > -10 & x < 0, x < -10)
    validateSeparated(true, x > -10 & x < 0, x < -15)
    validateSeparated(true, x > -10 & x < 0, x > -100 & x < -15)
    validateSeparated(false, x > -10 & x < 0, none(x))
    validateSeparated(false, x > -10 & x < 0, x)
    validateSeparated(false, x > -10 & x < 0, x >= 0)
    validateSeparated(false, x > -10 & x < 0, x > -1)
    validateSeparated(false, x > -10 & x < 0, x > -1 & x < 10)
    validateSeparated(false, x > -10 & x < 0, x > -100 & x < -5)
    validateSeparated(false, x > -10 & x < 0, x > -5 & x < -1)
    validateSeparated(false, x > -10 & x < 0, x > -15 & x < 1)
    validateSeparated(false, x > -10 & x < 0, x <= -10)
    validateSeparated(false, x >= -10 & x < 0, x < -10)
    // Interval.Empty
    validateSeparated(false, none(x), none(x))
    validateSeparated(false, none(x), x)
    validateSeparated(false, none(x), x < 0)
    validateSeparated(false, none(x), x > 0)
    validateSeparated(false, none(x), x > 0 & x < 10)
    // Interval.Unbounded
    validateSeparated(false, x, none(x))
    validateSeparated(false, x, x)
    validateSeparated(false, x, x < 0)
    validateSeparated(false, x, x > 0)
    validateSeparated(false, x, x > 0 & x < 10)
  }

  it("should check whether interval overlaps another one") {
    // Interval.Less
    validateOverlapping(true, x <= 0, x <= 0)
    validateOverlapping(true, x <= 0, x <= -1)
    validateOverlapping(true, x <= 0, x <= 1)
    validateOverlapping(true, x <= 0, x >= 0)
    validateOverlapping(true, x <= 0, x > -1)
    validateOverlapping(true, x <= 0, x > -10 & x <= -1)
    validateOverlapping(true, x <= 0, x > -1 & x < 10)
    validateOverlapping(true, x <= 0, x)
    validateOverlapping(false, x <= 0, none(x))
    validateOverlapping(false, x <= 0, x > 0)
    validateOverlapping(false, x <= 0, x > 0 & x < 10)
    // Interval.Greater
    validateOverlapping(true, x >= 0, x >= 0)
    validateOverlapping(true, x >= 0, x >= 1)
    validateOverlapping(true, x >= 0, x >= -1)
    validateOverlapping(true, x >= 0, x <= 0)
    validateOverlapping(true, x >= 0, x < 1)
    validateOverlapping(true, x >= 0, x >= 1 & x < 10)
    validateOverlapping(true, x >= 0, x > -10 & x < 1)
    validateOverlapping(true, x >= 0, x)
    validateOverlapping(false, x >= 0, none(x))
    validateOverlapping(false, x >= 0, x < 0)
    validateOverlapping(false, x >= 0, x > -10 & x < 0)
    // Interval.Between
    validateOverlapping(true, x >= 0 & x <= 10, x >= 0)
    validateOverlapping(true, x >= 0 & x <= 10, x >= 1)
    validateOverlapping(true, x >= 0 & x <= 10, x >= -1)
    validateOverlapping(true, x >= 0 & x <= 10, x <= 0)
    validateOverlapping(true, x >= 0 & x <= 10, x < 1)
    validateOverlapping(true, x >= 0 & x <= 10, x < 20)
    validateOverlapping(true, x >= 0 & x <= 10, x >= 1 & x <= 10)
    validateOverlapping(true, x >= 0 & x <= 10, x > 1 & x < 10)
    validateOverlapping(true, x >= 0 & x <= 10, x > -10 & x < 1)
    validateOverlapping(true, x >= 0 & x <= 10, x > 5 & x < 20)
    validateOverlapping(true, x >= 0 & x <= 10, x > -5 & x < 20)
    validateOverlapping(true, x >= 0 & x <= 10, x)
    validateOverlapping(false, x >= 0 & x <= 10, none(x))
    validateOverlapping(false, x >= 0 & x <= 10, x < 0)
    validateOverlapping(false, x >= 0 & x <= 10, x > -10 & x < 0)
    validateOverlapping(false, x >= 0 & x <= 10, x > 10)
    validateOverlapping(false, x >= 0 & x <= 10, x > 10 & x < 20)
    // Interval.Empty
    validateOverlapping(false, none(x), none(x))
    validateOverlapping(false, none(x), x)
    validateOverlapping(false, none(x), x < 0)
    validateOverlapping(false, none(x), x > 0)
    validateOverlapping(false, none(x), x > 0 & x < 10)
    // Interval.Unbounded
    validateOverlapping(false, x, none(x))
    validateOverlapping(true, x, x)
    validateOverlapping(true, x, x < 0)
    validateOverlapping(true, x, x > 0)
    validateOverlapping(true, x, x > 0 & x < 10)
  }

  private def validateLowerBound[E, D[X] <: Domain[X]](
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

  private def validateUpperBound[E, D[X] <: Domain[X]](
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

  private def validateContains[E, D[X] <: Domain[X]](
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

  private def validateRestrict[E, D[X] <: Domain[X]](
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

  private def validateAdjacentPreceding[E, D[X] <: Domain[X]](
    exp: Boolean, 
    x: Interval[E, D],
    y: Interval[E, D]
  ): Unit = {
    assert(x.isAdjacentPreceding(y) == exp, s"// expected $exp: $x is adjacent to $y and precedes it")
    y match {
      case y: Interval.NonEmpty[E, D] => 
        assert(x.isAdjacentPrecedingNE(y) == exp, s"// expected $exp: $x is adjacent to non-empty $y and precedes it")
      case _ =>
        // nothing to do 
    }
  }

  private def validateAdjacent[E, D[X] <: Domain[X]](
    exp: Boolean, 
    x: Interval[E, D],
    y: Interval[E, D]
  ): Unit = {
    assert(x.isAdjacent(y) == exp, s"// expected $exp: $x is adjacent to $y")
    if (exp) assert(y.isAdjacent(x) == exp, s"// expected $exp: $y is adjacent to $x")
    y match {
      case y: Interval.NonEmpty[E, D] => 
        assert(x.isAdjacentNE(y) == exp, s"// expected $exp: $x is adjacent to non-empty $y")
      case _ =>
        // nothing to do 
    }
    if (exp) x match {
      case x: Interval.NonEmpty[E, D] => 
        assert(y.isAdjacentNE(x) == exp, s"// expected $exp: $y is adjacent to non-empty $x")
      case _ =>
        // nothing to do 
    }
  }

  private def validateSeparatedPreceding[E, D[X] <: Domain[X]](
    exp: Boolean, 
    x: Interval[E, D],
    y: Interval[E, D]
  ): Unit = {
    assert(x.isSeparatedPreceding(y) == exp, s"// expected $exp: $x is separated from $y and precedes it")
    y match {
      case y: Interval.NonEmpty[E, D] => 
        assert(x.isSeparatedPrecedingNE(y) == exp, s"// expected $exp: $x is separated from non-empty $y and precedes it")
      case _ =>
        // nothing to do 
    }
  }

  private def validateSeparated[E, D[X] <: Domain[X]](
    exp: Boolean, 
    x: Interval[E, D],
    y: Interval[E, D]
  ): Unit = {
    assert(x.isSeparated(y) == exp, s"// expected $exp: $x is separated from $y")
    if (exp) assert(y.isSeparated(x) == exp, s"// expected $exp: $y is separated from $x")
    y match {
      case y: Interval.NonEmpty[E, D] => 
        assert(x.isSeparatedNE(y) == exp, s"// expected $exp: $x is separated from non-empty $y")
      case _ =>
        // nothing to do 
    }
    if (exp) x match {
      case x: Interval.NonEmpty[E, D] => 
        assert(y.isSeparatedNE(x) == exp, s"// expected $exp: $y is separated from non-empty $x")
      case _ =>
        // nothing to do 
    }
  }

  private def validateOverlapping[E, D[X] <: Domain[X]](
    exp: Boolean, 
    x: Interval[E, D],
    y: Interval[E, D]
  ): Unit = {
    assert(x.isOverlapping(y) == exp, s"// expected $exp: $x overlaps $y")
    if (exp) assert(y.isOverlapping(x) == exp, s"// expected $exp: $y overlaps $x")
    y match {
      case y: Interval.NonEmpty[E, D] => 
        assert(x.isOverlappingNE(y) == exp, s"// expected $exp: $x overlaps non-empty $y")
      case _ =>
        // nothing to do 
    }
    if (exp) x match {
      case x: Interval.NonEmpty[E, D] => 
        assert(y.isOverlappingNE(x) == exp, s"// expected $exp: $y overlaps non-empty $x")
      case _ =>
        // nothing to do 
    }
  }

  private def validateLess[E, D[X] <: Domain[X]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty, s"// expected $x is not empty")
    assert(!x.isBoundedBelow, s"// expected $x does not have lower bound")
    assert(x.isBoundedAbove, s"// expected $x has upper bound")
    assert(!x.isBounded, s"// expected $x is not bounded")
  }

  private def validateGreater[E, D[X] <: Domain[X]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty, s"// expected $x is not empty")
    assert(x.isBoundedBelow, s"// expected $x has lower bound")
    assert(!x.isBoundedAbove, s"// expected $x does not have upper bound")
    assert(!x.isBounded, s"// expected $x is not bounded")
  }

  private def validateBetween[E, D[X] <: Domain[X]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty, s"// expected $x is not empty")
    assert(x.isBoundedBelow, s"// expected $x has lower bound")
    assert(x.isBoundedAbove, s"// expected $x has upper bound")
    assert(x.isBounded, s"// expected $x is bounded")
  }

  private def validateEmpty[E, D[X] <: Domain[X]](x: Interval[E, D]): Unit = {
    assert(x.isEmpty, s"// expected $x is empty")
    assert(!x.isBoundedBelow, s"// expected $x does not have lower bound")
    assert(!x.isBoundedAbove, s"// expected $x does not have upper bound")
    assert(!x.isBounded, s"// expected $x is not bounded")
  }

  private def validateUnbounded[E, D[X] <: Domain[X]](x: Interval[E, D]): Unit = {
    assert(!x.isEmpty, s"// expected $x is not empty")
    assert(!x.isBoundedBelow, s"// expected $x does not have lower bound")
    assert(!x.isBoundedAbove, s"// expected $x does not have upper bound")
    assert(!x.isBounded, s"// expected $x is not bounded")
  }
}
