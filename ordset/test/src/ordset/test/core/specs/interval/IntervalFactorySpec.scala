package ordset.test.core.specs.interval

import ordset.core.{ExtendedBound, Bound}
import ordset.core.ExtendedBound.{BelowAll, AboveAll}
import ordset.core.domain.{Domain, DomainOps}
import ordset.test.core.IntervalAssertions._
import ordset.core.interval.{Interval, IntervalFactory}
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec
import ordset.core.domain.DomainOpsComponents.Domains

@RunWith(classOf[JUnitRunner])
class IntervalFactorySpec extends AnyFunSpec {

  import ordset.givens.int._
  import ordset.implementations.int._
  import ordset.core.syntax.SetBuilderNotation._

  it("should build intervals for unbounded domain") {
    type Dom = Domain.ContinuousUnbounded[Int]

    val d: Dom = implicitly

    implicit val domainOps = DomainOps.default(d)
    val interval = domainOps.intervals.factory
    val x = BoundBuilder(domainOps)

    assertSameIntervals(Interval.Empty(d), interval.empty)

    assertSameIntervals(Interval.Unbounded(d), interval.universal)

    validateBelow(Interval.Less(x <= 3, d), x <= 3)

    validateBelow(Interval.Unbounded(d), AboveAll)

    validateAbove(Interval.Greater(x > 3, d), x > 3)

    validateAbove(Interval.Unbounded(d), BelowAll)

    validateBetween(Interval.Between(x > 5, x <= 10, d), x > 5, x <= 10)

    validateBetween(Interval.Less(x <= 0, d), BelowAll, x <= 0)

    validateBetween(Interval.Greater(x >= 0, d), x >= 0, AboveAll)

    validateBetween(Interval.Unbounded(d), BelowAll, AboveAll)


    validateRangeFactoryBetween(Interval.Between(x >= 0, x <= 10, d), x < 0, x > 10)

    validateRangeFactoryBetween(Interval.Between(x > 0, x < 10, d), x <= 0, x >= 10)

    validateRangeFactoryBetween(Interval.Less(x < 10, d), BelowAll, x < 10)

    validateRangeFactoryBetween(Interval.Greater(x > 0, d), x > 0, AboveAll)

    validateRangeFactoryBetween(Interval.Unbounded(d), BelowAll, AboveAll)

    validateRangeFactoryBetween(Interval.Empty(d), AboveAll, x < 0)

    validateRangeFactoryBetween(Interval.Empty(d), x > 0, BelowAll)
  }

  it("should build intervals for bounded below domain") {
    type Dom = Domain.BoundedBelow[Int]

    val d: Dom = Domain.ContinuousBoundedBelow.default(tryNaturalOrderWithBounds(0, Int.MaxValue).get)

    implicit val domainOps = DomainOps.default(d)
    val interval = domainOps.intervals.factory
    val x = BoundBuilder(domainOps)

    assertSameIntervals(Interval.Empty(d), interval.empty)

    assertSameIntervals(Interval.Greater(x >= 0, d), interval.universal)

    //          ---|
    // bounds:      |------
    validateBelow(Interval.Empty(d), x < 0)
    //          ----|
    // bounds:      |------
    validateBelow(Interval.Between(x >= 0, x <= 0, d), x <= 0)
    //             ---|
    // bounds:      |------
    validateBelow(Interval.Between(x >= 0, x < 3, d), x < 3)
    //                 ---X
    // bounds:      |------
    validateBelow(Interval.Greater(x >= 0, d), AboveAll)

    //          X---
    // bounds:      |------
    validateAbove(Interval.Greater(x >= 0, d), BelowAll)
    //             |---
    // bounds:      |------
    validateAbove(Interval.Greater(x >= 0, d), x >= -1)
    //              |---
    // bounds:      |------
    validateAbove(Interval.Greater(x >= 0, d), x >= 0)
    //                 |---
    // bounds:      |------
    validateAbove(Interval.Greater(x >= 10, d), x >= 10)

    //          |--|
    // bounds:      |------
    validateBetween(Interval.Empty(d), x >= -10, x < 0)
    //           |--|
    // bounds:      |------
    validateBetween(Interval.Between(x >= 0, x <= 0, d), x >= -1, x <= 0)
    //             |--|
    // bounds:      |------
    validateBetween(Interval.Between(x >= 0, x < 10, d), x >= -1, x < 10)
    //              |--|
    // bounds:      |------
    validateBetween(Interval.Between(x >= 0, x < 10, d), x >= 0, x < 10)
    //                |--|
    // bounds:      |------
    validateBetween(Interval.Between(x >= 3, x < 10, d), x >= 3, x < 10)
    //          X--|
    // bounds:      |------
    validateBetween(Interval.Empty(d), BelowAll, x < 0)
    //          X-----|†
    // bounds:      |------
    validateBetween(Interval.Between(x >= 0, x < 10, d), BelowAll, x < 10)
    //                |---X
    // bounds:      |------
    validateBetween(Interval.Greater(x >= 3, d), x >= 3, AboveAll)
    //          X---------X
    // bounds:      |------
    validateBetween(Interval.Greater(x >= 0, d), BelowAll, AboveAll)


    validateRangeFactoryBetween(Interval.Between(x >= 0, x <= 10, d), x < 0, x > 10)

    validateRangeFactoryBetween(Interval.Between(x > 0, x < 10, d), x <= 0, x >= 10)

    validateRangeFactoryBetween(Interval.Between(x >= 0, x < 10, d), BelowAll, x < 10)

    validateRangeFactoryBetween(Interval.Greater(x > 10, d), x > 10, AboveAll)

    validateRangeFactoryBetween(Interval.Greater(x >= 0, d), BelowAll, AboveAll)

    validateRangeFactoryBetween(Interval.Empty(d), AboveAll, x < 10)

    validateRangeFactoryBetween(Interval.Empty(d), x > 0, BelowAll)
  }

  it("should build intervals for bounded above domain") {
    type Dom = Domain.BoundedAbove[Int]

    val d: Dom = Domain.ContinuousBoundedAbove.default(tryNaturalOrderWithBounds(Int.MinValue, 0).get)

    implicit val domainOps = DomainOps.default(d)
    val interval = domainOps.intervals.factory
    val x = BoundBuilder(domainOps)

    assertSameIntervals(Interval.Empty(d), interval.empty)

    assertSameIntervals(Interval.Less(x <= 0, d), interval.universal)

    //              ---|
    // bounds:      ------|
    validateBelow(Interval.Less(x < -5, d), x < -5)
    //              ------|
    // bounds:      ------|
    validateBelow(Interval.Less(x <= 0, d), x <= 0)
    //                   ---|
    // bounds:      ------|
    validateBelow(Interval.Less(x <= 0, d), x < 3)
    //                      ---X
    // bounds:      ------|
    validateBelow(Interval.Less(x <= 0, d), AboveAll)

    //          X---
    // bounds:      ------|
    validateAbove(Interval.Less(x <= 0, d), BelowAll)
    //                 |---
    // bounds:      ------|
    validateAbove(Interval.Between(x >= -5, x <= 0, d), x >= -5)
    //                    |---
    // bounds:      ------|
    validateAbove(Interval.Between(x >= 0, x <= 0, d), x >= 0)
    //                      |---
    // bounds:      ------|
    validateAbove(Interval.Empty(d), x >= 10)

    //              |--|
    // bounds:      ------|
    validateBetween(Interval.Between(x > -10, x < -5, d), x > -10, x < -5)
    //                 |--|
    // bounds:      ------|
    validateBetween(Interval.Between(x > -5, x <= 0, d), x > -5, x <= 0)
    //                  |--|
    // bounds:      ------|
    validateBetween(Interval.Between(x > -5, x <= 0, d), x > -5, x < 10)
    //                    |--|
    // bounds:      ------|
    validateBetween(Interval.Between(x >= 0, x <= 0, d), x >= 0, x < 10)
    //                      |--|
    // bounds:      ------|
    validateBetween(Interval.Empty(d), x >= 3, x < 10)
    //              X--|
    // bounds:      ------|
    validateBetween(Interval.Less(x < -5, d), BelowAll, x < -5)
    //              X--------|
    // bounds:      ------|
    validateBetween(Interval.Less(x <= 0, d), BelowAll, x < 10)
    //                 |-------X
    // bounds:      ------|
    validateBetween(Interval.Between(x >= -3, x <= 0, d), x >= -3, AboveAll)
    //              X----------X
    // bounds:      ------|
    validateBetween(Interval.Less(x <= 0, d), BelowAll, AboveAll)


    validateRangeFactoryBetween(Interval.Between(x >= -10, x <= 0, d), x < -10, x > 0)

    validateRangeFactoryBetween(Interval.Between(x > -10, x < 0, d), x <= -10, x >= 0)

    validateRangeFactoryBetween(Interval.Less(x < -10, d), BelowAll, x < -10)

    validateRangeFactoryBetween(Interval.Between(x > -10, x <= 0, d), x > -10, AboveAll)

    validateRangeFactoryBetween(Interval.Less(x <= 0, d), BelowAll, AboveAll)

    validateRangeFactoryBetween(Interval.Empty(d), AboveAll, x < -10)

    validateRangeFactoryBetween(Interval.Empty(d), x > -10, BelowAll)
  }

  it("should build intervals for bounded domain") {
    type Dom = Domain.ContinuousBounded[Int]

    val d = Domain.ContinuousBounded.default(tryNaturalOrderWithBounds(0, 100).get)
    
    implicit val domainOps = DomainOps.default(d)
    val interval = domainOps.intervals.factory
    val x = BoundBuilder(domainOps)

    assertSameIntervals(Interval.Empty(d), interval.empty)

    assertSameIntervals(Interval.Between(x >= 0, x <= 100, d), interval.universal)

    //          ---|
    // bounds:      |-----|
    validateBelow(Interval.Empty(d), x < 0) 
    //          ----|
    // bounds:      |-----|
    validateBelow(Interval.Between(x >= 0, x <= 0, d), x <= 0) 
    //              ---|
    // bounds:      |-----|
    validateBelow(Interval.Between(x >= 0, x <= 3, d), x <= 3)
    //              ------|
    // bounds:      |-----|
    validateBelow(Interval.Between(x >= 0, x <= 100, d), x <= 100)
    //                   ---|
    // bounds:      |-----|
    validateBelow(Interval.Between(x >= 0, x <= 100, d), x <= 101)
    //                     ---X
    // bounds:      |-----|
    validateBelow(Interval.Between(x >= 0, x <= 100, d), AboveAll)

    //                     |---
    // bounds:      |-----|
    validateAbove(Interval.Empty(d), x > 100)
    //                    |---
    // bounds:      |-----|
    validateAbove(Interval.Between(x >= 100, x <= 100, d), x >= 100)
    //                 |---
    // bounds:      |-----|
    validateAbove(Interval.Between(x > 90, x <= 100, d), x > 90)
    //              |---
    // bounds:      |-----|
    validateAbove(Interval.Between(x >= 0, x <= 100, d), x >= 0)
    //             |---
    // bounds:      |-----|
    validateAbove(Interval.Between(x >= 0, x <= 100, d), x > -1)
    //          X---
    // bounds:      |-----|
    validateAbove(Interval.Between(x >= 0, x <= 100, d), BelowAll)

    //          |--|
    // bounds:      |-----|
    validateBetween(Interval.Empty(d), x > -5, x < 0) 
    //           |--|
    // bounds:      |-----|
    validateBetween(Interval.Between(x >= 0, x <= 0, d), x > -2, x <= 0)
    //            |--|
    // bounds:      |-----|
    validateBetween(Interval.Between(x >= 0, x < 10, d), x > -2, x < 10)
    //              |--|
    // bounds:      |-----|
    validateBetween(Interval.Between(x >= 0, x < 10, d), x >= 0, x < 10)
    //                |--|
    // bounds:      |-----|
    validateBetween(Interval.Between(x > 2, x < 90, d), x > 2, x < 90) 
    //              |-----|
    // bounds:      |-----|
    validateBetween(Interval.Between(x >= 0, x <= 100, d), x >= 0, x <= 100) 
    //                 |--|
    // bounds:      |-----|
    validateBetween(Interval.Between(x > 90, x <= 100, d), x > 90, x <= 100)
    //                   |--|
    // bounds:      |-----|
    validateBetween(Interval.Between(x > 90, x <= 100, d), x > 90, x < 102)
    //                    |--|
    // bounds:      |-----|
    validateBetween(Interval.Between(x >= 100, x <= 100, d), x >= 100, x < 102)
    //                     |--|
    // bounds:      |-----|
    validateBetween(Interval.Empty(d), x > 100, x < 105) 
    //             |-------|
    // bounds:      |-----|
    validateBetween(Interval.Between(x >= 0, x <= 100, d), x > -1, x < 101) 
    //          X--|
    // bounds:      |-----|
    validateBetween(Interval.Empty(d), BelowAll, x < 0) 
    //          X------|
    // bounds:      |-----|
    validateBetween(Interval.Between(x >= 0, x < 10, d), BelowAll, x < 10)
    //          X-----------|
    // bounds:      |-----|
    validateBetween(Interval.Between(x >= 0, x <= 100, d), BelowAll, x < 102)
    //                |-------X
    // bounds:      |-----|
    validateBetween(Interval.Between(x > 2, x <= 100, d), x > 2, AboveAll) 
    //                   |----X
    // bounds:      |-----|
    validateBetween(Interval.Between(x > 90, x <= 100, d), x > 90, AboveAll)
    //                     |--X
    // bounds:      |-----|
    validateBetween(Interval.Empty(d), x > 100, AboveAll) 
    //          X-------------X
    // bounds:      |-----|
    validateBetween(Interval.Between(x >= 0, x <= 100, d), BelowAll, AboveAll)


    validateRangeFactoryBetween(Interval.Between(x >= 10, x <= 100, d), x < 10, x > 100)

    validateRangeFactoryBetween(Interval.Between(x > 10, x < 90, d), x <= 10, x >= 90)

    validateRangeFactoryBetween(Interval.Between(x >= 0, x < 10, d), BelowAll, x < 10)

    validateRangeFactoryBetween(Interval.Between(x > 50, x <= 100, d), x > 50, AboveAll)

    validateRangeFactoryBetween(Interval.Between(x >= 0, x <= 100, d), BelowAll, AboveAll)

    validateRangeFactoryBetween(Interval.Empty(d), AboveAll, x < 10)

    validateRangeFactoryBetween(Interval.Empty(d), x > 10, BelowAll)
  }

  private def validateBelow[E, D <: Domain[E]](
    expected: Interval[E, D],
    upper: ExtendedBound.Upper[E]
  )(
    implicit domainOps: DomainOps[E, D]
  ): Unit = {
    val interval = domainOps.intervals.factory
    assertSameIntervals(expected, interval.belowExtended(upper))
    upper match {
      case bound: Bound.Upper[E] => 
        assertSameIntervals(expected, interval.belowBound(bound))
        assertSameIntervals(expected, interval.belowElement(bound.element, bound.isIncluding))
      case _ => 
        // nothing to do
    }
  }

  private def validateAbove[E, D <: Domain[E]](
    expected: Interval[E, D],
    lower: ExtendedBound.Lower[E]
  )(
    implicit domainOps: DomainOps[E, D]
  ): Unit = {
    val interval = domainOps.intervals.factory
    assertSameIntervals(expected, interval.aboveExtended(lower))
    lower match {
      case bound: Bound.Lower[E] => 
        assertSameIntervals(expected, interval.aboveBound(bound))
        assertSameIntervals(expected, interval.aboveElement(bound.element, bound.isIncluding))
      case _ => 
        // nothing to do
    }
  }

  private def validateBetween[E, D <: Domain[E]](
    expected: Interval[E, D],
    lower: ExtendedBound.Lower[E],
    upper: ExtendedBound.Upper[E]
  )(
    implicit domainOps: DomainOps[E, D]
  ): Unit = {
    val interval = domainOps.intervals.factory
    assertSameIntervals(
      expected, interval.betweenExtended(lower, upper)
    )
    (lower, upper) match {
      case (lower: Bound.Lower[E], upper: Bound.Upper[E]) => 
        assertSameIntervals(
          expected, interval.betweenBounds(lower, upper)
        )
        assertSameIntervals(
          expected, interval.betweenElements(lower.element, lower.isIncluding, upper.element, upper.isIncluding)
        )
      case _ => 
        // nothing to do
    }
  }

  private def validateRangeFactoryBetween[E, D <: Domain[E]](
    expected: Interval[E, D],
    lower: ExtendedBound[E],
    upper: ExtendedBound[E]
  )(
    implicit domainOps: DomainOps[E, D]
  ): Unit = {
    val interval = domainOps.intervals.factory.asRangeFactory
    assertSameIntervals(expected, interval.between(lower, upper))
  }
}
