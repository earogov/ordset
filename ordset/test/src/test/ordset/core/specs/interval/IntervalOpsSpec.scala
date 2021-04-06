package test.ordset.core.specs.interval

import ordset.core.domain.{Domain, DomainOps}
import ordset.core.{Bound, Interval, IntervalBuilder}
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.funspec.AnyFunSpec

@RunWith(classOf[JUnitRunner])
class IntervalOpsSpec extends AnyFunSpec {
  
  import ordset.core.instances.int._
  import ordset.core.syntax.BoundSyntax._
  import ordset.core.syntax.SetBuilderNotation._

  import scala.language.postfixOps

  type Dom = Domain[Int]

  val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]
  val ops: DomainOps[Int, Dom] = DomainOps.defaultDomainOps
  val interval: IntervalBuilder[Int, Dom] = ops.interval

  private implicit def toAssertOps[E, D <: Domain[E]](interval: Interval[E, D]): AssertOps[E, D] =
    new AssertOps[E, D](interval)

  it("should cross two intervals") {

    // Empty cases
    assert(interval.empty.isCrossOf(interval.empty, interval.empty))
    assert(interval.empty.isCrossOf(interval.empty, interval.universal))
    assert(interval.empty.isCrossOf(interval.empty, x <= 0))
    assert(interval.empty.isCrossOf(interval.empty, x > 0 & x < 5))
    assert(interval.empty.isCrossOf(interval.empty, x >= 5))

    // Unbounded cases
    assert(interval.empty.isCrossOf(interval.universal, interval.empty))
    assert(interval.universal.isCrossOf(interval.universal, interval.universal))
    assert(interval(x <= 5).isCrossOf(interval.universal, x <= 5))
    assert((x >= 0 & x < 5).isCrossOf(interval.universal, x >= 0 & x < 5))
    assert(interval(x >= 5).isCrossOf(interval.universal, x >= 5))

    // Left unbounded cases
    // ----|
    //        |-----
    assert(interval.empty.isCrossOf(x <= 0, x > 0))
    assert(interval.empty.isCrossOf(x <= 0, x > 5))
    // --------|
    //     |--------
    assert((x >= 0 & x < 5).isCrossOf(x < 5, x >= 0))
    // ----|
    //     |--------
    assert((x >= 5 & x <= 5).isCrossOf(x <= 5, x >= 5))
    // --------|
    // --------|
    assert(interval(x < 0).isCrossOf(x < 0, x < 0))
    // --------|
    // ------------|
    assert(interval(x < 0).isCrossOf(x < 0, x <= 5))
    // --------|
    //     |-------|
    assert((x >= 0 & x < 5).isCrossOf(x < 5, x >= 0 & x < 10))
    // ----|
    //     |-------|
    assert((x >= 0 & x <= 0).isCrossOf(x <= 0, x >= 0 & x < 10))
    // ------------|
    //     |---|
    assert((x >= 0 & x < 5).isCrossOf(x < 10, x >= 0 & x < 5))
    // ------------|
    //         |---|
    assert((x >= 0 & x < 5).isCrossOf(x < 5, x >= 0 & x < 5))
    // ------------|
    //             |
    assert((x >= 0 & x <= 0).isCrossOf(x <= 0, x >= 0 & x <= 0))

    // Right unbounded cases
    // ----|
    //        |-----
    assert(interval.empty.isCrossOf(x > 5, x <= 0))
    assert(interval.empty.isCrossOf(x > 5, x <= 5))
    // --------|
    //     |--------
    assert((x >= 0 & x < 5).isCrossOf(x >= 0, x < 5))
    // ----|
    //     |--------
    assert((x >= 0 & x <= 0).isCrossOf(x >= 0, x <= 0))
    //     |--------
    // |------------
    assert(interval(x >= 5).isCrossOf(x > 0, x >= 5))
    //     |--------
    //     |--------
    assert(interval(x >= 5).isCrossOf(x >= 5, x >= 5))
    //     |--------
    // |-------|
    assert((x >= 0 & x < 5).isCrossOf(x >= 0, x >= -5 & x < 5))
    // |------------
    //     |---|
    assert((x >= 0 & x < 5).isCrossOf(x > -10, x >= 0 & x < 5))
    // |------------
    // |---|
    assert((x >= 0 & x < 5).isCrossOf(x >= 0, x >= 0 & x < 5))
    // |------------
    // |
    assert((x >= 5 & x <= 5).isCrossOf(x >= 5, x >= 5 & x <= 5))

    // Bounded cases
    // |----|
    //        |----|
    assert(interval.empty.isCrossOf(x >= -10 & x < 0, x >= 0 & x < 10))
    // |-----------|
    //     |---|
    assert((x >= 0 & x <= 5).isCrossOf(x >= -10 & x < 10, x >= 0 & x <= 5))
    // |-----------|
    // |---|
    assert((x >= 0 & x <= 5).isCrossOf(x >= 0 & x < 10, x >= 0 & x <= 5))
    // |-----------|
    // |
    assert((x >= 0 & x <= 0).isCrossOf(x >= 0 & x < 10, x >= 0 & x <= 0))
    // |-----------|
    //         |---|
    assert((x >= 0 & x <= 5).isCrossOf(x >= -10 & x <= 5, x >= 0 & x <= 5))
    // |-----------|
    //             |
    assert((x >= 5 & x <= 5).isCrossOf(x >= -10 & x <= 5, x >= 5 & x <= 5))
    // |-------|
    //     |-------|
    assert((x >= 0 & x <= 5).isCrossOf(x > -10 & x <= 5, x >= 0 & x < 10))
    // |-----|
    //       |-----|
    assert((x >= 5 & x <= 5).isCrossOf(x > -10 & x <= 5, x >= 5 & x < 10))
  }

  it("should cut interval below the bound") {

    // Empty cases
    assert(interval.empty.isCutBelow(0`[`, interval.empty))

    // Unbounded cases
    assert(interval(x >= 0).isCutBelow(0`[`, interval.universal))

    // Left unbounded cases
    // ------|
    // ///|          cut
    assert((x >= -1 & x < 0).isCutBelow(-1`[`, x < 0))

    // ------|
    // /////|        cut
    assert((x >= 0 & x <= 0).isCutBelow(0`[`, x <= 0))

    // ------|
    // //////|       cut
    assert(interval.empty.isCutBelow(0`(`, x <= 0))

    // ------|
    // /////////|    cut
    assert(interval.empty.isCutBelow(3`(`, x <= 0))

    // Right unbounded cases
    //       |------
    // ///|          cut
    assert(interval(x >= 0).isCutBelow(-2`(`, x >= 0))

    //       |------
    // //////|       cut
    assert(interval(x > 0).isCutBelow(0`(`, x >= 0))

    //       |------
    // /////////|    cut
    assert(interval(x > 2).isCutBelow(2`(`, x >= 0))

    // Bounded cases
    //    |----|
    // //|           cut
    assert((x >= 0 & x < 2).isCutBelow(-1`(`, x >= 0 & x < 2))

    //    |----|
    // ///|          cut
    assert((x > 0 & x < 2).isCutBelow(0`(`, x >= 0 & x < 2))

    //    |----|
    // //////|       cut
    assert((x > 1 & x < 2).isCutBelow(1`(`, x >= 0 & x < 2))

    //    |----|
    // ////////|     cut
    assert(interval.empty.isCutBelow(2`[`, x >= 0 & x < 2))

    //    |----|
    // ///////////|  cut
    assert(interval.empty.isCutBelow(3`[`, x >= 0 & x < 2))
  }

  it("should cut interval above the bound") {

    // Empty cases
    assert(interval.empty.isCutAbove(0`]`, interval.empty))

    // Unbounded cases
    assert(interval(x <= 0).isCutAbove(0`]`, interval.universal))

    // Left unbounded cases
    // ----|
    //        |///// cut
    assert(interval(x < 0).isCutAbove(0`]`, x < 0))

    // ----|
    //     |//////// cut
    assert(interval(x < 0).isCutAbove(0`)`, x <= 0))

    // ----|
    //   |////////// cut
    assert(interval(x <= -1).isCutAbove(-1`]`, x < 0))

    // Right unbounded cases
    //      |-------
    //         |//// cut
    assert((x >= 0 & x <= 1).isCutAbove(1`]`, x >= 0))

    //      |-------
    //       |////// cut
    assert((x >= 0 & x <= 0).isCutAbove(0`]`, x >= 0))

    //      |-------
    //      |/////// cut
    assert(interval.empty.isCutAbove(0`)`, x >= 0))

    //      |-------
    //   |////////// cut
    assert(interval.empty.isCutAbove(-1`)`, x >= 0))

    // Bounded cases
    //    |----|
    //           |// cut
    assert((x >= 0 & x <= 2).isCutAbove(5`]`, x >= 0 & x <= 2))

    //    |----|
    //         |//// cut
    assert((x >= 0 & x < 2).isCutAbove(2`)`, x >= 0 & x <= 2))

    //    |----|
    //      |/////// cut
    assert((x >= 0 & x < 1).isCutAbove(1`)`, x >= 0 & x <= 2))

    //    |----|
    //    |///////// cut
    assert(interval.empty.isCutAbove(0`)`, x >= 0 & x <= 2))

    //    |----|
    //  |/////////// cut
    assert(interval.empty.isCutAbove(-1`)`, x >= 0 & x <= 2))
  }

  private class AssertOps[E, D <: Domain[E]](interval: Interval[E, D]) {

    def isCrossOf(x: Interval[E, D], y: Interval[E, D])(implicit ops: DomainOps[E, D]): Boolean =
      ops.intervalHash.eqv(interval, ops.intervalOps.cross(x, y)) &&
        ops.intervalHash.eqv(interval, ops.intervalOps.cross(y, x))

    def isCutBelow(bound: Bound.Lower[E], x: Interval[E, D])(implicit ops: DomainOps[E, D]): Boolean =
      ops.intervalHash.eqv(interval, ops.intervalOps.cutBelow(bound, x))

    def isCutAbove(bound: Bound.Upper[E], x: Interval[E, D])(implicit ops: DomainOps[E, D]): Boolean =
      ops.intervalHash.eqv(interval, ops.intervalOps.cutAbove(bound, x))
  }
}
