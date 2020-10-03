package test.ordset

import ordset.domain.{Domain, DomainOps}
import org.scalatest.funspec.AnyFunSpec

class IntervalOpsSpec extends AnyFunSpec {
  
  import ordset._
  import instances.Int._
  import ordset.syntax.SetBuilderNotation._

  type Dom = Domain[Int]

  val x: BoundBuilder[Int, Dom] = BoundBuilder[Int, Dom]
  val ops: DomainOps[Int, Dom] = DomainOps.defaultDomainOps
  val interval: IntervalBuilder[Int, Dom] = ops.interval

  private implicit def toCrossOps[E, D <: Domain[E]](interval: Interval[E, D]): CrossOps[E, D] =
    new CrossOps[E, D](interval)

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

  private class CrossOps[E, D <: Domain[E]](interval: Interval[E, D]) {

    def isCrossOf(x: Interval[E, D], y: Interval[E, D])(implicit ops: DomainOps[E, D]): Boolean =
      ops.intervalHash.eqv(interval, ops.intervalOps.cross(x, y)) &&
        ops.intervalHash.eqv(interval, ops.intervalOps.cross(y, x))
  }
}
