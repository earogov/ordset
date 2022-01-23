package ordset.test.core.specs.interval

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.Bound
import ordset.core.interval.{Interval, IntervalFactory}
import ordset.test.core.IntervalAssertions._
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec

@RunWith(classOf[JUnitRunner])
class IntervalAlgebraSpec extends AnyFunSpec {
  
  import ordset.givens.int._
  import ordset.core.syntax.BoundSyntax._
  import ordset.core.syntax.SetBuilderNotation._

  import scala.language.postfixOps

  type Dom[X] = Domain.ContinuousUnbounded[X]

  val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]
  val ops: DomainOps[Int, Dom] = DomainOps.default
  val interval: IntervalFactory[Int, Dom] = ops.intervals.factory

  private implicit def toAssertOps[E, D[X] <: Domain[X]](interval: Interval[E, D]): AssertOps[E, D] =
    new AssertOps[E, D](interval)

  it("should support `cross` operation") {

    // Empty cases
    interval.empty.isCrossOf(interval.empty, interval.empty)
    interval.empty.isCrossOf(interval.empty, interval.universal)
    interval.empty.isCrossOf(interval.empty, x <= 0)
    interval.empty.isCrossOf(interval.empty, x > 0 & x < 5)
    interval.empty.isCrossOf(interval.empty, x >= 5)

    // Unbounded cases
    interval.empty.isCrossOf(interval.universal, interval.empty)
    interval.universal.isCrossOf(interval.universal, interval.universal)
    interval.belowBound(x <= 5).isCrossOf(interval.universal, x <= 5)
    (x >= 0 & x < 5).isCrossOf(interval.universal, x >= 0 & x < 5)
    interval.aboveBound(x >= 5).isCrossOf(interval.universal, x >= 5)

    // Left unbounded cases
    // ----|
    //        |-----
    interval.empty.isCrossOf(x <= 0, x > 0)
    interval.empty.isCrossOf(x <= 0, x > 5)
    // --------|
    //     |--------
    (x >= 0 & x < 5).isCrossOf(x < 5, x >= 0)
    // ----|
    //     |--------
    (x >= 5 & x <= 5).isCrossOf(x <= 5, x >= 5)
    // --------|
    // --------|
    interval.belowBound(x < 0).isCrossOf(x < 0, x < 0)
    // --------|
    // ------------|
    interval.belowBound(x < 0).isCrossOf(x < 0, x <= 5)
    // --------|
    //     |-------|
    (x >= 0 & x < 5).isCrossOf(x < 5, x >= 0 & x < 10)
    // ----|
    //     |-------|
    (x >= 0 & x <= 0).isCrossOf(x <= 0, x >= 0 & x < 10)
    // ------------|
    //     |---|
    (x >= 0 & x < 5).isCrossOf(x < 10, x >= 0 & x < 5)
    // ------------|
    //         |---|
    (x >= 0 & x < 5).isCrossOf(x < 5, x >= 0 & x < 5)
    // ------------|
    //             |
    (x >= 0 & x <= 0).isCrossOf(x <= 0, x >= 0 & x <= 0)

    // Right unbounded cases
    // ----|
    //        |-----
    interval.empty.isCrossOf(x > 5, x <= 0)
    interval.empty.isCrossOf(x > 5, x <= 5)
    // --------|
    //     |--------
    (x >= 0 & x < 5).isCrossOf(x >= 0, x < 5)
    // ----|
    //     |--------
    (x >= 0 & x <= 0).isCrossOf(x >= 0, x <= 0)
    //     |--------
    // |------------
    interval.aboveBound(x >= 5).isCrossOf(x > 0, x >= 5)
    //     |--------
    //     |--------
    interval.aboveBound(x >= 5).isCrossOf(x >= 5, x >= 5)
    //     |--------
    // |-------|
    (x >= 0 & x < 5).isCrossOf(x >= 0, x >= -5 & x < 5)
    // |------------
    //     |---|
    (x >= 0 & x < 5).isCrossOf(x > -10, x >= 0 & x < 5)
    // |------------
    // |---|
    (x >= 0 & x < 5).isCrossOf(x >= 0, x >= 0 & x < 5)
    // |------------
    // |
    (x >= 5 & x <= 5).isCrossOf(x >= 5, x >= 5 & x <= 5)

    // Bounded cases
    // |----|
    //        |----|
    interval.empty.isCrossOf(x >= -10 & x < 0, x >= 0 & x < 10)
    // |-----------|
    //     |---|
    (x >= 0 & x <= 5).isCrossOf(x >= -10 & x < 10, x >= 0 & x <= 5)
    // |-----------|
    // |---|
    (x >= 0 & x <= 5).isCrossOf(x >= 0 & x < 10, x >= 0 & x <= 5)
    // |-----------|
    // |
    (x >= 0 & x <= 0).isCrossOf(x >= 0 & x < 10, x >= 0 & x <= 0)
    // |-----------|
    //         |---|
    (x >= 0 & x <= 5).isCrossOf(x >= -10 & x <= 5, x >= 0 & x <= 5)
    // |-----------|
    //             |
    (x >= 5 & x <= 5).isCrossOf(x >= -10 & x <= 5, x >= 5 & x <= 5)
    // |-------|
    //     |-------|
    (x >= 0 & x <= 5).isCrossOf(x > -10 & x <= 5, x >= 0 & x < 10)
    // |-----|
    //       |-----|
    (x >= 5 & x <= 5).isCrossOf(x > -10 & x <= 5, x >= 5 & x < 10)
  }

  it("should support `takeAboveBound` operation") {

    // Empty cases
    interval.empty.isTakeAbove(0`[`, interval.empty)

    // Unbounded cases
    interval.aboveBound(x >= 0).isTakeAbove(0`[`, interval.universal)

    // Left unbounded cases
    // ------|
    //    |/////////
    (x >= -1 & x < 0).isTakeAbove(-1`[`, x < 0)

    // ------|
    //       |//////
    (x >= 0 & x <= 0).isTakeAbove(0`[`, x <= 0)

    // ------|
    //        |/////
    interval.empty.isTakeAbove(0`(`, x <= 0)

    // ------|
    //         |////
    interval.empty.isTakeAbove(3`(`, x <= 0)

    // Right unbounded cases
    //       |------
    //    |/////////
    interval.aboveBound(x >= 0).isTakeAbove(-2`(`, x >= 0)

    //       |------
    //        |/////
    interval.aboveBound(x > 0).isTakeAbove(0`(`, x >= 0)

    //       |------
    //         |////
    interval.aboveBound(x > 2).isTakeAbove(2`(`, x >= 0)

    // Bounded cases
    //    |----|
    //  |///////////
    (x >= 0 & x < 2).isTakeAbove(-1`(`, x >= 0 & x < 2)

    //    |----|
    //     |////////
    (x > 0 & x < 2).isTakeAbove(0`(`, x >= 0 & x < 2)

    //    |----|
    //       |//////
    (x > 1 & x < 2).isTakeAbove(1`(`, x >= 0 & x < 2)

    //    |----|
    //          |///
    interval.empty.isTakeAbove(2`[`, x >= 0 & x < 2)

    //    |----|
    //           |//
    interval.empty.isTakeAbove(3`[`, x >= 0 & x < 2)
  }

  it("should support `takeBelowBound` operation") {

    // Empty cases
    interval.empty.isTakeBelow(0`]`, interval.empty)

    // Unbounded cases
    interval.belowBound(x <= 0).isTakeBelow(0`]`, interval.universal)

    // Left unbounded cases
    // ----|
    // ///////|
    interval.belowBound(x <= -1).isTakeBelow(-1`]`, x < 0)

    // ----|
    // /////|
    interval.belowBound(x < 0).isTakeBelow(0`]`, x < 0)

    // ----|
    // ///|
    interval.belowBound(x < 0).isTakeBelow(0`)`, x <= 0)

    // Right unbounded cases
    //      |-------
    // ////////|
    (x >= 0 & x <= 1).isTakeBelow(1`]`, x >= 0)

    //      |-------
    // /////|
    (x >= 0 & x <= 0).isTakeBelow(0`]`, x >= 0)

    //      |-------
    // ////|     
    interval.empty.isTakeBelow(0`)`, x >= 0)

    //      |-------
    // ///|
    interval.empty.isTakeBelow(-1`)`, x >= 0)

    // Bounded cases
    //    |----|
    // //////////|
    (x >= 0 & x <= 2).isTakeBelow(5`]`, x >= 0 & x <= 2)

    //    |----|
    // ///////|
    (x >= 0 & x < 2).isTakeBelow(2`)`, x >= 0 & x <= 2)

    //    |----|
    // /////|
    (x >= 0 & x < 1).isTakeBelow(1`)`, x >= 0 & x <= 2)

    //    |----|
    // //|
    interval.empty.isTakeBelow(0`)`, x >= 0 & x <= 2)

    //    |----|
    // /|
    interval.empty.isTakeBelow(-1`)`, x >= 0 & x <= 2)
  }

  private class AssertOps[E, D[X] <: Domain[X]](interval: Interval[E, D]) {

    def isCrossOf(x: Interval[E, D], y: Interval[E, D])(implicit ops: DomainOps[E, D]): Unit = {
      assertSameIntervals(interval, ops.intervals.alg.cross(x, y))
      assertSameIntervals(interval, ops.intervals.alg.cross(y, x))
    }

    def isTakeAbove(bound: Bound.Lower[E], x: Interval[E, D])(implicit ops: DomainOps[E, D]): Unit =
      assertSameIntervals(interval, ops.intervals.alg.takeAboveBound(bound, x))

    def isTakeBelow(bound: Bound.Upper[E], x: Interval[E, D])(implicit ops: DomainOps[E, D]): Unit =
      assertSameIntervals(interval, ops.intervals.alg.takeBelowBound(bound, x))
  }
}
